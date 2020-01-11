using System.Collections.Generic;
using System.Linq;
namespace Lab3.Ast.Declarations {
	sealed class FunctionDeclaration : IDeclaration {
		public int Position { get; }
		public readonly TypeNode ReturnType;
		public readonly string Name;
		public readonly IReadOnlyList<Parameter> Parameters;
		public readonly Block Body;
		public const string Keyword = "func";
		public FunctionDeclaration(
			int position,
			TypeNode returnType,
			string name,
			IReadOnlyList<Parameter> parameters,
			Block body
			) {
			Position = position;
			ReturnType = returnType;
			Name = name;
			Parameters = parameters;
			Body = body;
		}
		public string FormattedString {
			get {
				var parameters = string.Join(", ", Parameters.Select(x => x.FormattedString));
				return $"{Keyword} {ReturnType.FormattedString} {Name}({parameters}) {Body.FormattedString}";
			}
		}
	}
}
