using Lab3.Ast.Statements;
namespace Lab3.Ast {
	interface IStatementVisitor {
		void VisitIf(If ifStatement);
		void VisitWhile(While whiteStatement);
		void VisitExpressionStatement(ExpressionStatement expressionStatement);
		void VisitAssignment(Assignment assignment);
		void VisitReturn(Return returnStatement);
	}
	interface IStatementVisitor<T> {
		T VisitIf(If ifStatement);
		T VisitWhile(While whiteStatement);
		T VisitExpressionStatement(ExpressionStatement expressionStatement);
		T VisitAssignment(Assignment assignment);
		T VisitReturn(Return returnStatement);
	}
}
