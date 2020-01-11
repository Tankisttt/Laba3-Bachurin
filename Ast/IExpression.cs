namespace Lab3.Ast {
	interface IExpression : INode {
		void Accept(IExpressionVisitor visitor);
		T Accept<T>(IExpressionVisitor<T> visitor);
	}
}
