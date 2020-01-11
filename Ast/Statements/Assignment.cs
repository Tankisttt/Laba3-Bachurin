namespace Lab3.Ast.Statements
{
	sealed class Assignment : IStatement
	{
		public int Position { get; }
		public readonly IExpression Target;
		public readonly TypeNode Type;
		public readonly IExpression Expr;
		public Assignment(int position, IExpression target, TypeNode type, IExpression expr)
		{
			Position = position;
			Target = target;
			Type = type;
			Expr = expr;
		}
		public string FormattedString
		{
			get
			{
				var type = Type != null ? $" : {Type.FormattedString}" : "";
				return $"{Target.FormattedString}{type} = {Expr.FormattedString};\n";
			}
		}
		public void Accept(IStatementVisitor visitor) => visitor.VisitAssignment(this);
		public T Accept<T>(IStatementVisitor<T> visitor) => visitor.VisitAssignment(this);
	}
}
