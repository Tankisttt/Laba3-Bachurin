namespace Lab3.Ast.Expressions {
	sealed class TypedExpression : IExpression {
		public int Position { get; }
		public readonly IExpression Expr;
		public readonly TypeNode Type;
		public TypedExpression(int position, IExpression expr, TypeNode type) {
			Position = position;
			Expr = expr;
			Type = type;
		}
		public string FormattedString => $"{Expr.FormattedString}::{Type.FormattedString}";
		public void Accept(IExpressionVisitor visitor) => visitor.VisitTypedExpression(this);
		public T Accept<T>(IExpressionVisitor<T> visitor) => visitor.VisitTypedExpression(this);
	}
}
