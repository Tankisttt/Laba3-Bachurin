using BuiltinTypes;
using Lab3.Ast;
using Mono.Cecil;
using System;
using System.Collections.Generic;
using System.Linq;
namespace Lab3.Compiling {
	sealed class AllTypes {
		public readonly ModuleDefinition Module;
		public readonly TypeRef Int;
		public readonly TypeRef Bool;
		public readonly TypeRef Void;
		public readonly TypeRef Object;
		public readonly TypeRef Null;
		readonly Dictionary<string, TypeRef> typeByName = new Dictionary<string, TypeRef>();
		readonly Dictionary<TypeRef, string> nameByType = new Dictionary<TypeRef, string>();
		readonly List<MethodReference> functions = new List<MethodReference>();
		public AllTypes(ModuleDefinition module) {
			Module = module;
			Int = AddBuiltinType("int", module.TypeSystem.Int32);
			Bool = AddBuiltinType("bool", module.TypeSystem.Boolean);
			Void = AddBuiltinType("void", module.TypeSystem.Void);
			Object = AddBuiltinType("object", module.TypeSystem.Object);
			Null = AddBuiltinType("Null", null);
			foreach (var m in typeof(BuiltinFunctions).GetMethods()) {
				if (m.IsStatic && m.IsPublic) {
					AddFunction(module.ImportReference(m));
				}
			}
		}
		public void AddFunction(MethodReference mr) {
			functions.Add(Module.ImportReference(mr));
		}
		TypeRef AddBuiltinType(string name, TypeReference tr) {
			var type = new TypeRef(tr);
			if (!TryAddType(name, type)) {
				throw new Exception();
			}
			return type;
		}
		public bool TryAddType(string name, TypeRef type) {
			if (typeByName.ContainsKey(name) || nameByType.ContainsKey(type)) {
				return false;
			}
			nameByType.Add(type, name);
			typeByName.Add(name, type);
			return true;
		}
		public bool CanAssign(TypeRef sourceType, TypeRef destinationType) {
			if (sourceType == destinationType) {
				return true;
			}
			if (sourceType == Null && destinationType.CanBeNull) {
				return true;
			}
			return false;
		}
		public bool CanCall(MethodReference mr, IReadOnlyList<TypeRef> argumentTypes) {
			var parameterTypes = mr.Parameters.Select(p => new TypeRef(p.ParameterType)).ToList();
			if (parameterTypes.Count != argumentTypes.Count) {
				return false;
			}
			for (var i = 0; i < parameterTypes.Count; i++) {
				if (!CanAssign(argumentTypes[i], parameterTypes[i])) {
					return false;
				}
			}
			return true;
		}
		public IReadOnlyList<MethodReference> GetCallableFunctions(
			string functionName, IReadOnlyList<TypeRef> argumentTypes
			) {
			return functions
				.Where(fn => fn.Name == functionName && CanCall(fn, argumentTypes))
				.Select(Module.ImportReference)
				.ToList();
		}
		public IReadOnlyList<MethodReference> GetCallableMethods(
			TypeRef type, string methodName, IReadOnlyList<TypeRef> argumentTypes
			) {
			return type.GetTypeDefinition().Methods
				.Where(fn => fn.Name == methodName && CanCall(fn, argumentTypes))
				.Select(Module.ImportReference)
				.ToList();
		}
		public TypeRef? TryGetTypeRef(TypeNode type) {
			return TryGetTypeRef(type.Name);
		}
		public TypeRef? TryGetTypeRef(string name) {
			TypeRef type;
			if (typeByName.TryGetValue(name, out type)) {
				return type;
			}
			return null;
		}
		public string GetTypeName(TypeRef type) {
			string name;
			if (nameByType.TryGetValue(type, out name)) {
				return name;
			}
			return type.TypeReference.FullName;
		}
	}
}
