namespace Lab3.Ast.Statements {
	sealed class Assignment : IStatement {
		public int Position { get; }
		public readonly IExpression Target;
		public readonly TypeNode Type;
		public readonly IExpression Expr;
		public readonly string Operator;
		public Assignment(int position, IExpression target, TypeNode type, IExpression expr, string oper) {
			Position = position;
			Target = target;
			Type = type;
			Expr = expr;
			Operator = oper;
		}
		public string FormattedString {
			get {
				var type = Type != null ? $" : {Type.FormattedString}" : "";
				return $"{Target.FormattedString}{type} {Operator} {Expr.FormattedString};\n";
			}
		}
		public void Accept(IStatementVisitor visitor) => visitor.VisitAssignment(this);
		public T Accept<T>(IStatementVisitor<T> visitor) => visitor.VisitAssignment(this);
	}
}
