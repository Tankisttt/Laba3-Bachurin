namespace Lab3.Ast.Statements {
	sealed class Return : IStatement {
		public int Position { get; }
		public readonly IExpression Expr;
		public Return(int position, IExpression expr) {
			Position = position;
			Expr = expr;
		}
		public string FormattedString => $"return{(Expr == null ? "" : $" {Expr.FormattedString}")};\n";
		public void Accept(IStatementVisitor visitor) => visitor.VisitReturn(this);
		public T Accept<T>(IStatementVisitor<T> visitor) => visitor.VisitReturn(this);
	}
}
