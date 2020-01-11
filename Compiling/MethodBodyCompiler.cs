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
			throw new NotImplementedException();
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
			throw new NotImplementedException();
		}
		public void VisitAssignment(Assignment statement)
		{
			throw new NotImplementedException();
		}
		public void VisitExpressionStatement(ExpressionStatement statement)
		{
			throw new NotImplementedException();
		}
		public void VisitIf(If statement)
		{
			throw new NotImplementedException();
		}
		public void VisitReturn(Return statement)
		{
			throw new NotImplementedException();
		}
		public void VisitWhile(While statement)
		{
			throw new NotImplementedException();
		}
		#endregion
		#region expressions
		TypeRef CompileExpression(IExpression expression)
		{
			return expression.Accept(this);
		}
		public TypeRef VisitBinary(Binary node)
		{
			throw new NotImplementedException();
		}
		public TypeRef VisitCall(Call expression)
		{
			throw new NotImplementedException();
		}
		public TypeRef VisitParentheses(Parentheses expression)
		{
			throw new NotImplementedException();
		}
		public TypeRef VisitNumber(Number expression)
		{
			throw new NotImplementedException();
		}
		public TypeRef VisitIdentifier(Identifier expression)
		{
			throw new NotImplementedException();
		}
		public TypeRef VisitMemberAccess(MemberAccess expression)
		{
			throw new NotImplementedException();
		}
		public TypeRef VisitTypedExpression(TypedExpression expression)
		{
			throw new NotImplementedException();
		}
		#endregion
	}
}
