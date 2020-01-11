namespace Lab3.Ast.Statements {
	sealed class While : IStatement {
		public int Position { get; }
		public readonly IExpression Condition;
		public readonly Block Body;
		public While(int position, IExpression condition, Block body) {
			Position = position;
			Condition = condition;
			Body = body;
		}
		public string FormattedString => $"while ({Condition.FormattedString}) {Body.FormattedString}";
		public void Accept(IStatementVisitor visitor) => visitor.VisitWhile(this);
		public T Accept<T>(IStatementVisitor<T> visitor) => visitor.VisitWhile(this);
	}
}
