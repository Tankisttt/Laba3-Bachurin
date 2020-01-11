namespace Lab3.Ast.Statements {
	sealed class ExpressionStatement : IStatement {
		public int Position { get; }
		public readonly IExpression Expr;
		public ExpressionStatement(int position, IExpression expr) {
			Position = position;
			Expr = expr;
		}
		public string FormattedString => $"{Expr.FormattedString};\n";
		public void Accept(IStatementVisitor visitor) => visitor.VisitExpressionStatement(this);
		public T Accept<T>(IStatementVisitor<T> visitor) => visitor.VisitExpressionStatement(this);
	}
}
