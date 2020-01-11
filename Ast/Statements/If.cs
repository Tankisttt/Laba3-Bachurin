namespace Lab3.Ast.Statements {
	sealed class If : IStatement {
		public int Position { get; }
		public readonly IExpression Condition;
		public readonly Block Body;
		public If(int position, IExpression condition, Block body) {
			Position = position;
			Condition = condition;
			Body = body;
		}
		public string FormattedString => $"if ({Condition.FormattedString}) {Body.FormattedString}";
		public void Accept(IStatementVisitor visitor) => visitor.VisitIf(this);
		public T Accept<T>(IStatementVisitor<T> visitor) => visitor.VisitIf(this);
	}
}
