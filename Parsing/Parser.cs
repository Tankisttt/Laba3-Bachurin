using Lab3.Ast;
using Lab3.Ast.ClassMembers;
using Lab3.Ast.Declarations;
using Lab3.Ast.Expressions;
using Lab3.Ast.Statements;
using System;
using System.Collections.Generic;
using System.Linq;
namespace Lab3.Parsing
{
	sealed class Parser
	{
		readonly SourceFile sourceFile;
		readonly IReadOnlyList<Token> tokens;
		int tokenIndex = 0;
		Token CurrentToken => tokens[tokenIndex];
		int CurrentPosition => CurrentToken.Position;
		Parser(SourceFile sourceFile, IReadOnlyList<Token> tokens)
		{
			this.sourceFile = sourceFile;
			this.tokens = tokens;
		}
		#region stuff
		string[] DebugCurrentPosition => sourceFile.FormatLines(CurrentPosition,
			inlinePointer: true,
			pointer: " <|> "
			).ToArray();
		string DebugCurrentLine => string.Join("", sourceFile.FormatLines(CurrentPosition,
			linesAround: 0,
			inlinePointer: true,
			pointer: " <|> "
			).ToArray());
		static bool IsNotWhitespace(Token t)
		{
			switch (t.Type)
			{
				case TokenType.Whitespaces:
				case TokenType.SingleLineComment:
				case TokenType.MultiLineComment:
					return false;
			}
			return true;
		}
		void ExpectEof()
		{
			if (!IsType(TokenType.EnfOfFile))
			{
				throw MakeError($"Не допарсили до конца, остался {CurrentToken}");
			}
		}
		void ReadNextToken()
		{
			tokenIndex += 1;
		}
		void Reset()
		{
			tokenIndex = 0;
		}
		Exception MakeError(string message)
		{
			return new Exception(sourceFile.MakeErrorMessage(CurrentPosition, message));
		}
		bool SkipIf(string s)
		{
			if (CurrentIs(s))
			{
				ReadNextToken();
				return true;
			}
			return false;
		}
		bool CurrentIs(string s) => string.Equals(CurrentToken.Lexeme, s, StringComparison.Ordinal);
		bool IsType(TokenType type) => CurrentToken.Type == type;
		void Expect(string s)
		{
			if (!SkipIf(s))
			{
				throw MakeError($"Ожидали \"{s}\", получили {CurrentToken}");
			}
		}
		#endregion
		public static ProgramNode Parse(SourceFile sourceFile)
		{
			var eof = new Token(TokenType.EnfOfFile, "", sourceFile.Text.Length);
			var tokens = Lexer.GetTokens(sourceFile).Concat(new[] { eof }).Where(IsNotWhitespace).ToList();
			var parser = new Parser(sourceFile, tokens);
			return parser.ParseProgram();
		}
		ProgramNode ParseProgram()
		{
			Reset();
			var declarations = new List<IDeclaration>();
			while (true)
			{
				var declaration = TryParseDeclaration();
				if (declaration == null)
				{
					break;
				}
				declarations.Add(declaration);
			}
			var statements = new List<IStatement>();
			while (!IsType(TokenType.EnfOfFile))
			{
				statements.Add(ParseStatement());
			}
			var result = new ProgramNode(sourceFile, declarations, statements);
			ExpectEof();
			return result;
		}
		IDeclaration TryParseDeclaration()
		{
			var pos = CurrentPosition;
			if (SkipIf(ClassDeclaration.Keyword))
			{
				var name = ParseIdentifier();
				var members = ParseClassMembers();
				return new ClassDeclaration(pos, name, members);
			}
			if (SkipIf(FunctionDeclaration.Keyword))
			{
				var type = ParseType();
				var name = ParseIdentifier();
				var parameters = ParseParameters();
				var body = ParseBlock();
				return new FunctionDeclaration(pos, type, name, parameters, body);
			}
			return null;
		}
		IReadOnlyList<IClassMember> ParseClassMembers()
		{
			Expect("{");
			var members = new List<IClassMember>();
			while (!SkipIf("}"))
			{
				members.Add(ParseClassMember());
			}
			return members;
		}
		IClassMember ParseClassMember()
		{
			var pos = CurrentPosition;
			var type = ParseType();
			var name = ParseIdentifier();
			if (SkipIf(";"))
			{
				return new ClassField(pos, type, name);
			}
			var parameters = ParseParameters();
			var body = ParseBlock();
			return new ClassMethod(pos, type, name, parameters, body);
		}
		IReadOnlyList<Parameter> ParseParameters()
		{
			Expect("(");
			var parameters = new List<Parameter>();
			if (!SkipIf(")"))
			{
				parameters.Add(ParseParameter());
				while (SkipIf(","))
				{
					parameters.Add(ParseParameter());
				}
				Expect(")");
			}
			return parameters;
		}
		Parameter ParseParameter()
		{
			var type = ParseType();
			var pos = CurrentPosition;
			var name = ParseIdentifier();
			return new Parameter(pos, type, name);
		}
		Block ParseBlock()
		{
			var pos = CurrentPosition;
			Expect("{");
			var statements = new List<IStatement>();
			while (!SkipIf("}"))
			{
				statements.Add(ParseStatement());
			}
			return new Block(pos, statements);
		}
		IStatement ParseStatement()
		{
			var pos = CurrentPosition;
			if (SkipIf("if"))
			{
				Expect("(");
				var condition = ParseExpression();
				Expect(")");
				var block = ParseBlock();
				return new If(pos, condition, block);
			}
			if (SkipIf("while"))
			{
				Expect("(");
				var condition = ParseExpression();
				Expect(")");
				var block = ParseBlock();
				return new While(pos, condition, block);
			}
			if (SkipIf("return"))
			{
				IExpression expr = null;
				if (!SkipIf(";"))
				{
					expr = ParseExpression();
					Expect(";");
				}
				return new Return(pos, expr);
			}
			var expression = ParseExpression();
			if (SkipIf(";"))
			{
				return new ExpressionStatement(pos, expression);
			}
			TypeNode type = null;
			if (SkipIf(":"))
			{
				type = ParseType();
			}
			Expect("=");
			var restAssigmentExpression = ParseExpression();
			Expect(";");
			return new Assignment(pos, expression, type, restAssigmentExpression);
		}
		TypeNode ParseType()
		{
			return new TypeNode(CurrentPosition, ParseIdentifier());
		}
		string ParseIdentifier()
		{
			if (!IsType(TokenType.Identifier))
			{
				throw MakeError($"Ожидали идентификатор, получили {CurrentToken}");
			}
			var lexeme = CurrentToken.Lexeme;
			ReadNextToken();
			return lexeme;
		}
		#region expressions
		IExpression ParseExpression()
		{
			return ParseEqualityExpression();
		}
		IExpression ParseExpressionType(IExpression expression)
		{
			var pos = CurrentPosition;
			if (SkipIf("::"))
			{
				return new TypedExpression(pos, expression, ParseType());
			}
			return expression;
		}
		IExpression ParseEqualityExpression()
		{
			var left = ParseRelationalExpression();
			while (true)
			{
				var pos = CurrentPosition;
				if (SkipIf("=="))
				{
					var right = ParseRelationalExpression();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Equal, right));
				}
				else
				{
					break;
				}
			}
			return left;
		}
		IExpression ParseRelationalExpression()
		{
			var left = ParseAdditiveExpression();
			while (true)
			{
				var pos = CurrentPosition;
				if (SkipIf("<"))
				{
					var right = ParseAdditiveExpression();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Less, right));
				}
				else
				{
					break;
				}
			}
			return left;
		}
		IExpression ParseAdditiveExpression()
		{
			var left = ParseMultiplicativeExpression();
			while (true)
			{
				var pos = CurrentPosition;
				if (SkipIf("+"))
				{
					var right = ParseMultiplicativeExpression();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Addition, right));
				}
				else if (SkipIf("-"))
				{
					var right = ParseMultiplicativeExpression();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Subtraction, right));
				}
				else
				{
					break;
				}
			}
			return left;
		}
		IExpression ParseMultiplicativeExpression()
		{
			var left = ParsePrimary();
			while (true)
			{
				var pos = CurrentPosition;
				if (SkipIf("*"))
				{
					var right = ParsePrimary();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Multiplication, right));
				}
				else if (SkipIf("/"))
				{
					var right = ParsePrimary();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Division, right));
				}
				else if (SkipIf("%"))
				{
					var right = ParsePrimary();
					left = ParseExpressionType(new Binary(pos, left, BinaryOperator.Remainder, right));
				}
				else
				{
					break;
				}
			}
			return left;
		}
		IExpression ParsePrimary()
		{
			var expression = ParsePrimitive();
			while (true)
			{
				int pos = CurrentPosition;
				if (SkipIf("("))
				{
					var arguments = new List<IExpression>();
					if (!SkipIf(")"))
					{
						arguments.Add(ParseExpression());
						while (SkipIf(","))
						{
							arguments.Add(ParseExpression());
						}
						Expect(")");
					}
					expression = ParseExpressionType(new Call(pos, expression, arguments));
				}
				else if (SkipIf("."))
				{
					var member = ParseIdentifier();
					expression = ParseExpressionType(new MemberAccess(pos, expression, member));
				}
				else
				{
					break;
				}
			}
			return expression;
		}
		IExpression ParsePrimitive()
		{
			var pos = CurrentPosition;
			if (SkipIf("("))
			{
				var expression = new Parentheses(pos, ParseExpression());
				Expect(")");
				return ParseExpressionType(expression);
			}
			if (IsType(TokenType.NumberLiteral))
			{
				var lexeme = CurrentToken.Lexeme;
				ReadNextToken();
				return ParseExpressionType(new Number(pos, lexeme));
			}
			if (IsType(TokenType.Identifier))
			{
				var lexeme = CurrentToken.Lexeme;
				ReadNextToken();
				return ParseExpressionType(new Identifier(pos, lexeme));
			}
			throw MakeError($"Ожидали идентификатор, число или скобку, получили {CurrentToken}");
		}
		#endregion
	}
}
