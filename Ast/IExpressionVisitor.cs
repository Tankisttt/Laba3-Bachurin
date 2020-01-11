using Lab3.Ast.Expressions;
namespace Lab3.Ast {
	interface IExpressionVisitor {
		void VisitBinary(Binary binary);
		void VisitCall(Call call);
		void VisitParentheses(Parentheses parentheses);
		void VisitNumber(Number number);
		void VisitIdentifier(Identifier identifier);
		void VisitMemberAccess(MemberAccess memberAccess);
		void VisitTypedExpression(TypedExpression typedExpression);
	}
	interface IExpressionVisitor<T> {
		T VisitBinary(Binary binary);
		T VisitCall(Call call);
		T VisitParentheses(Parentheses parentheses);
		T VisitNumber(Number number);
		T VisitIdentifier(Identifier identifier);
		T VisitMemberAccess(MemberAccess memberAccess);
		T VisitTypedExpression(TypedExpression typedExpression);
	}
}
