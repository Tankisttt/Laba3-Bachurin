using Lab3.Ast;
using Lab3.Ast.Expressions;
using Lab3.Ast.Statements;
using Lab3.Parsing;
using Mono.Cecil;
using Mono.Cecil.Cil;
using System;
using System.Collections.Generic;

namespace Lab3.Compiling
{
	sealed class MethodBodyCompiler : IStatementVisitor, IExpressionVisitor<TypeRef>
	{
		readonly SourceFile sourceFile;
		readonly AllTypes types;
		readonly ModuleDefinition module;
		readonly MethodDefinition method;
		readonly ILProcessor cil;
		readonly Dictionary<string, VariableDefinition> variables
			= new Dictionary<string, VariableDefinition>();
		MethodBodyCompiler(SourceFile sourceFile, AllTypes types, MethodDefinition method)
		{
			this.sourceFile = sourceFile;
			this.types = types;
			module = types.Module;
			this.method = method;
			cil = method.Body.GetILProcessor();
		}
		public static void Compile(
			SourceFile sourceFile,
			AllTypes types,
			MethodDefinition md,
			IEnumerable<IStatement> statements
			)
		{
			new MethodBodyCompiler(sourceFile, types, md).CompileMethodStatements(statements);
		}

		void CompileMethodStatements(IEnumerable<IStatement> statements)
		{
			foreach (var st in statements)
				CompileStatement(st);
			cil.Emit(OpCodes.Ret);
		}
		Exception MakeError(INode node, string message)
		{
			return new Exception(sourceFile.MakeErrorMessage(node.Position, message));
		}
		Exception WrongType(INode expression, TypeRef actual, TypeRef expected)
		{
			var message = $@"Выражение {expression.FormattedString} имеет тип {
				types.GetTypeName(actual)} вместо {types.GetTypeName(expected)}";
			return MakeError(expression, message);
		}

		#region statements	
		void CompileStatement(IStatement statement)
		{
			statement.Accept(this);
		}
		void CompileBlock(Block block)
		{
			foreach (var statement in block.Statements)
				statement.Accept(this);
		}
		public void VisitAssignment(Assignment statement)
		{
			var id = statement.Target as Identifier;
			if (id != null)
			{
				TypeRef exprType = CompileExpression(statement.Expr);
				VariableDefinition newVar;
				TypeRef? typeToCompare;
				if (variables.ContainsKey(id.Name))
				{
					if (statement.Type != null)
					{
						typeToCompare = types.TryGetTypeRef(statement.Type);
						if (typeToCompare == null) 
							throw MakeError(statement.Type, "wrong type");
					}
					else
						typeToCompare = variables[id.Name].VariableType;

					if (variables[id.Name].VariableType != typeToCompare.Value)
						throw MakeError(statement.Type, "wrong type");

					if (types.CanAssign(exprType, typeToCompare.Value))
						newVar = variables[id.Name];
					else
						throw WrongType(statement, exprType, typeToCompare.Value);
				}
				else
				{
					if (statement.Type != null)
					{
						typeToCompare = types.TryGetTypeRef(statement.Type);

						if (typeToCompare == null)
							throw MakeError(statement.Type, "wrong type");

						if (!types.CanAssign(exprType, typeToCompare.Value))
							throw WrongType(statement, exprType, typeToCompare.Value);
					}
					else
						typeToCompare = exprType.TypeReference;

					newVar = new VariableDefinition(typeToCompare.Value.TypeReference);
					method.Body.Variables.Add(newVar);
					variables[id.Name] = newVar;
				}

				cil.Emit(OpCodes.Stloc, newVar);
				return;
			}
			var member = statement.Target as MemberAccess;
			if (member != null)
			{
				var obj = CompileExpression(member.Obj);
				var typeDef = obj.GetTypeDefinition();
				TypeRef exprType = CompileExpression(statement.Expr);
				foreach (var field in typeDef.Fields)
				{
					if (field.Name == member.Member)
					{
						if (types.CanAssign(exprType, field.FieldType))
							cil.Emit(OpCodes.Stfld, field);		
						else
							throw WrongType(statement, exprType, field.FieldType);
						return;
					}
				}
			}
			throw MakeError(statement, $"{statement.Target.FormattedString}");
		}
		public void VisitExpressionStatement(ExpressionStatement statement)
		{
			var t = CompileExpression(statement.Expr);
			if (t != types.Void)
				cil.Emit(OpCodes.Pop);
		}
		public void VisitIf(If statement)
		{
			var nop = Instruction.Create(OpCodes.Nop);
			var expr = this.CompileExpression(statement.Condition);

			if (expr == types.Bool)
			{
				cil.Emit(OpCodes.Brfalse, nop);
				this.CompileBlock(statement.Body);
				cil.Append(nop);

			}
			else throw WrongType(statement, expr, types.Bool);
		}
		public void VisitReturn(Return statement)
		{
			var retType = this.CompileExpression(statement.Expr);
			if (types.CanAssign(retType, method.ReturnType))
				cil.Emit(OpCodes.Ret);
			else throw WrongType(statement, retType, method.ReturnType);
		}
		public void VisitWhile(While statement)
		{
			var end = Instruction.Create(OpCodes.Nop);
			var compare = Instruction.Create(OpCodes.Nop);

			cil.Append(compare);
			TypeRef expr = CompileExpression(statement.Condition);
			if (expr == types.Bool)
			{
				cil.Emit(OpCodes.Brfalse, end);

				CompileBlock(statement.Body);
				cil.Emit(OpCodes.Br, compare);
			}
			else throw WrongType(statement, expr, types.Bool);
			cil.Append(end);
		}
		#endregion
		#region expressions
		TypeRef CompileExpression(IExpression expression)
		{
			return expression.Accept(this);
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitBinary(Binary node)
		{
			var leftType = CompileExpression(node.Left);
			var rightType = CompileExpression(node.Right);

			if (node.Operator == BinaryOperator.Equal && (rightType.CanBeNull || leftType.CanBeNull))
			{
				if (types.CanAssign(rightType, leftType) || types.CanAssign(leftType, rightType))
				{
					cil.Emit(OpCodes.Ceq);
					return types.Bool;
				}
				else throw MakeError(node, "Разные типы");
			}

			if (leftType == rightType)
			{
				switch (node.Operator)
				{
					case BinaryOperator.Addition:
						cil.Emit(OpCodes.Add);
						return types.Int;
					case BinaryOperator.Division:
						cil.Emit(OpCodes.Div);
						return types.Int;
					case BinaryOperator.Equal:
						cil.Emit(OpCodes.Ceq);
						return types.Bool;
					case BinaryOperator.Less:
						cil.Emit(OpCodes.Clt);
						return types.Bool;
					case BinaryOperator.Multiplication:
						cil.Emit(OpCodes.Mul);
						return types.Int;
					case BinaryOperator.Remainder:
						cil.Emit(OpCodes.Rem);
						return types.Int;
					case BinaryOperator.Subtraction:
						cil.Emit(OpCodes.Sub);
						return types.Int;
					default:
						throw MakeError(node, $"Ошибка в Binary {node}");
				}
			}
			else throw WrongType(node, leftType, rightType);
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitCall(Call expression)
		{
			var member = expression.Function as MemberAccess;
			if (member != null)
			{
				var obj = this.CompileExpression(member.Obj);
				
				List<TypeRef> argumentTypes = new List<TypeRef>();
				foreach (IExpression arg in expression.Arguments)
					argumentTypes.Add(CompileExpression(arg));
				
				var methods = this.types.GetCallableMethods(obj, member.Member, argumentTypes);
				if (methods.Count == 1)
				{
					cil.Emit(OpCodes.Call, module.ImportReference(methods[0]));
					return methods[0].ReturnType;
				}
				else
					throw MakeError(expression, "Не 1 метод");
			}

			var id = expression.Function as Identifier;
			if (id != null)
			{
				var name = id.Name;
				List<TypeRef> argTypes = new List<TypeRef>();
				foreach (var arg in expression.Arguments)
					argTypes.Add(CompileExpression(arg));

				var fns = types.GetCallableFunctions(name, argTypes);
				if (fns.Count > 1)
					throw MakeError(expression, "Не 1 функция");
				
				if (fns.Count == 1)
				{
					cil.Emit(OpCodes.Call, fns[0]);
					return fns[0].ReturnType;
				}

				var metType = this.types.TryGetTypeRef(id.Name);
				if (metType != null)
				{
					var constructors = this.types.GetCallableMethods(metType.Value, ".ctor", argTypes);
					if (constructors.Count == 1)
					{
						cil.Emit(OpCodes.Newobj, constructors[0]);
						return (TypeRef)metType;
					}
					else if (constructors.Count > 1)
						throw MakeError(expression, "У конструкторов одинаковые аргементы");
				}
			}
			throw MakeError(expression, $"Не получилось вызвать {expression.Function.FormattedString}");
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitParentheses(Parentheses expression)
		{
			return this.CompileExpression(expression.Expr);
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitNumber(Number expression)
		{
			
			cil.Emit(OpCodes.Ldc_I4, int.Parse(expression.Lexeme));
			return types.Int;
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitIdentifier(Identifier expression)
		{
			var name = expression.Name;
			switch (name)
			{
				case "false":
					cil.Emit(OpCodes.Ldc_I4, 0);
					return this.types.Bool;
				case "true":
					cil.Emit(OpCodes.Ldc_I4, 1);
					return this.types.Bool;
				case "null":
					cil.Emit(OpCodes.Ldnull);
					return this.types.Null;
				case "this":
					cil.Emit(OpCodes.Ldarg, method.Body.ThisParameter);
					return this.method.DeclaringType;
			}

			VariableDefinition variableDefinition;
			if (variables.TryGetValue(expression.Name, out variableDefinition))
			{
				cil.Emit(OpCodes.Ldloc, variableDefinition);
				return variableDefinition.VariableType;
			}

			foreach (var par in method.Parameters)
			{
				if (par.Name == name)
				{
					cil.Emit(OpCodes.Ldarg, par);
					return par.ParameterType;
				}
			}
			throw MakeError(expression, $"identifier exception {expression.FormattedString}");
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitMemberAccess(MemberAccess expression)
		{
			var obj = CompileExpression(expression.Obj);
			var obj_td = obj.GetTypeDefinition();
			foreach (var field in obj_td.Fields)
			{
				if (field.Name == expression.Member)
				{
					cil.Emit(OpCodes.Ldfld, field);
					return field.FieldType;
				}
			}
			throw MakeError(expression, $"Ошибка тут {expression.Member}");
		}
		TypeRef IExpressionVisitor<TypeRef>.VisitTypedExpression(TypedExpression expression)
		{
			var expr = this.CompileExpression(expression.Expr);
			if (expr.TypeReference == this.types.TryGetTypeRef(expression.Type))
				return expr.TypeReference;
			else
				throw MakeError(expression, $"Неверный тип {expression.FormattedString}");
		}
		#endregion
	}
}
