using System.Collections.Generic;
using System.Linq;
namespace Lab3.Ast {
	sealed class Block : INode {
		public int Position { get; }
		public readonly IReadOnlyList<IStatement> Statements;
		public Block(int position, IReadOnlyList<IStatement> statements) {
			Position = position;
			Statements = statements;
		}
		public string FormattedString {
			get {
				return "{\n" + string.Join("", Statements.Select(x => x.FormattedString)) + "}\n";
			}
		}
	}
}
