using Lab3.Ast;
using Lab3.Ast.ClassMembers;
using Lab3.Ast.Declarations;
using Lab3.Parsing;
using Mono.Cecil;
using Mono.Cecil.Cil;
using System;
using System.Collections.Generic;
using System.Linq;
namespace Lab3.Compiling {
	sealed class ProgramCompiler {
		readonly AllTypes types;
		readonly ModuleDefinition module;
		readonly ProgramNode programNode;
		readonly SourceFile sourceFile;
		readonly TypeDefinition entryClass;
		public readonly MethodDefinition MainMethod;
		readonly Dictionary<string, TypeDefinition> typeDefinitionByName
			= new Dictionary<string, TypeDefinition>();
		readonly Dictionary<ClassMethod, MethodDefinition> classMethodDefinitionByNode
			= new Dictionary<ClassMethod, MethodDefinition>();
		readonly Dictionary<FunctionDeclaration, MethodDefinition> functionMethodDefinitionByNode
			= new Dictionary<FunctionDeclaration, MethodDefinition>();
		public ProgramCompiler(
			AllTypes types,
			ProgramNode programNode,
			string entryClassName = "Entry",
			string mainMethodName = "Main"
			) {
			this.types = types;
			module = types.Module;
			this.programNode = programNode;
			sourceFile = programNode.SourceFile;
			entryClass = new TypeDefinition(
				"", entryClassName,
				TypeAttributes.Public
				| TypeAttributes.Abstract
				| TypeAttributes.Sealed
				| TypeAttributes.BeforeFieldInit,
				module.TypeSystem.Object
				);
			module.Types.Add(entryClass);
			MainMethod = new MethodDefinition(
				"Main",
				MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Static,
				module.TypeSystem.Void
				);
			entryClass.Methods.Add(MainMethod);
			module.EntryPoint = MainMethod;
		}
		public void Compile() {
			AddClasses();
			AddClassFields();
			AddClassMethods();
			AddFunctions();
			CompileMainMethod();
			CompileFunctions();
			CompileClassMethods();
		}
		Exception MakeError(INode node, string message) {
			return new Exception(sourceFile.MakeErrorMessage(node.Position, message));
		}
		TypeReference GetTypeReference(TypeNode type) {
			var maybeTypeRef = types.TryGetTypeRef(type);
			if (maybeTypeRef == null) {
				throw MakeError(type, $"Неизвестный тип {type.FormattedString}");
			}
			var typeRef = maybeTypeRef.Value;
			if (typeRef == types.Null) {
				throw MakeError(type, $"Ожидали нормальный тип, получили {type.FormattedString}");
			}
			return typeRef.TypeReference;
		}
		void AddClasses() {
			foreach (var classDeclaration in programNode.Declarations.OfType<ClassDeclaration>()) {
				var className = classDeclaration.Name;
				var td = new TypeDefinition(
					"", className,
					TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.BeforeFieldInit,
					module.TypeSystem.Object
					);
				module.Types.Add(td);
				if (!types.TryAddType(className, new TypeRef(td))) {
					throw MakeError(classDeclaration, $"Класс {className} уже объявлен");
				}
				typeDefinitionByName.Add(className, td);
			}
		}
		void AddClassFields() {
			foreach (var classDeclaration in programNode.Declarations.OfType<ClassDeclaration>()) {
				var className = classDeclaration.Name;
				var td = typeDefinitionByName[className];
				var fieldNames = new HashSet<string>();
				foreach (var classField in classDeclaration.Members.OfType<ClassField>()) {
					var fieldName = classField.Name;
					if (!fieldNames.Add(fieldName)) {
						throw MakeError(classField, $"Поле {fieldName} класса {className} уже объявлено");
					}
					var fd = new FieldDefinition(
						fieldName,
						FieldAttributes.Public,
						GetTypeReference(classField.Type)
						);
					td.Fields.Add(fd);
				}
			}
		}
		void AddClassMethods() {
			foreach (var classDeclaration in programNode.Declarations.OfType<ClassDeclaration>()) {
				var td = typeDefinitionByName[classDeclaration.Name];
				foreach (var classMethod in classDeclaration.Members.OfType<ClassMethod>()) {
					var md = new MethodDefinition(
						classMethod.Name,
						MethodAttributes.Public | MethodAttributes.HideBySig,
						GetTypeReference(classMethod.ReturnType)
						);
					td.Methods.Add(md);
					classMethodDefinitionByNode.Add(classMethod, md);
					AddParameters(md, classMethod.Parameters);
				}
				AddAndCompileClassConstructor(td);
			}
		}
		void AddFunctions() {
			foreach (var functionDeclaration in programNode.Declarations.OfType<FunctionDeclaration>()) {
				var md = new MethodDefinition(
					functionDeclaration.Name,
					MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Static,
					GetTypeReference(functionDeclaration.ReturnType)
					);
				entryClass.Methods.Add(md);
				functionMethodDefinitionByNode.Add(functionDeclaration, md);
				AddParameters(md, functionDeclaration.Parameters);
				types.AddFunction(md);
			}
		}
		void AddParameters(MethodDefinition md, IReadOnlyList<Parameter> Parameters) {
			var parameterNames = new HashSet<string>();
			foreach (var parameterNode in Parameters) {
				var parameterName = parameterNode.Name;
				if (!parameterNames.Add(parameterName)) {
					throw MakeError(parameterNode, $"Параметр {parameterName} уже объявлен");
				}
				var pd = new ParameterDefinition(
					parameterNode.Name,
					ParameterAttributes.None,
					GetTypeReference(parameterNode.Type)
					);
				md.Parameters.Add(pd);
			}
		}
		void AddAndCompileClassConstructor(TypeDefinition type) {
			var ctor = new MethodDefinition(
				".ctor",
				MethodAttributes.Public
				| MethodAttributes.HideBySig
				| MethodAttributes.SpecialName
				| MethodAttributes.RTSpecialName,
				module.TypeSystem.Void
				);
			type.Methods.Add(ctor);
			var asm = ctor.Body.GetILProcessor();
			asm.Emit(OpCodes.Ldarg, ctor.Body.ThisParameter);
			asm.Emit(OpCodes.Call, module.ImportReference(typeof(object).GetConstructor(Type.EmptyTypes)));
			foreach (var field in type.Fields) {
				var pd = new ParameterDefinition(field.Name, ParameterAttributes.None, field.FieldType);
				ctor.Parameters.Add(pd);
				asm.Emit(OpCodes.Ldarg, ctor.Body.ThisParameter);
				asm.Emit(OpCodes.Ldarg, pd);
				asm.Emit(OpCodes.Stfld, field);
			}
			asm.Emit(OpCodes.Ret);
		}
		void CompileMainMethod() {
			MethodBodyCompiler.Compile(sourceFile, types, MainMethod, programNode.Statements);
		}
		void CompileFunctions() {
			foreach (var functionDeclaration in programNode.Declarations.OfType<FunctionDeclaration>()) {
				var md = functionMethodDefinitionByNode[functionDeclaration];
				MethodBodyCompiler.Compile(sourceFile, types, md, functionDeclaration.Body.Statements);
			}
		}
		void CompileClassMethods() {
			foreach (var classDeclaration in programNode.Declarations.OfType<ClassDeclaration>()) {
				var td = typeDefinitionByName[classDeclaration.Name];
				foreach (var classMethod in classDeclaration.Members.OfType<ClassMethod>()) {
					var md = classMethodDefinitionByNode[classMethod];
					MethodBodyCompiler.Compile(sourceFile, types, md, classMethod.Body.Statements);
				}
			}
		}
	}
}
