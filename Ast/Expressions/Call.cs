using System.Collections.Generic;
using System.Linq;
namespace Lab3.Ast.Expressions {
	sealed class Call : IExpression {
		public int Position { get; }
		public readonly IExpression Function;
		public readonly IReadOnlyList<IExpression> Arguments;
		public Call(int position, IExpression function, IReadOnlyList<IExpression> arguments) {
			Position = position;
			Function = function;
			Arguments = arguments;
		}
		public string FormattedString {
			get {
				var arguments = string.Join(", ", Arguments.Select(x => x.FormattedString));
				return $"{Function.FormattedString}({arguments})";
			}
		}
		public void Accept(IExpressionVisitor visitor) => visitor.VisitCall(this);
		public T Accept<T>(IExpressionVisitor<T> visitor) => visitor.VisitCall(this);
	}
}
