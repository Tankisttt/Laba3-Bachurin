using Lab3.Parsing;
using System.Collections.Generic;
using System.Linq;
namespace Lab3.Ast {
	sealed class ProgramNode : INode {
		public int Position => 0;
		public readonly SourceFile SourceFile;
		public readonly IReadOnlyList<IDeclaration> Declarations;
		public readonly IReadOnlyList<IStatement> Statements;
		public ProgramNode(SourceFile sourceFile, IReadOnlyList<IDeclaration> declarations, IReadOnlyList<IStatement> statements) {
			SourceFile = sourceFile;
			Declarations = declarations;
			Statements = statements;
		}
		public string FormattedString => string.Join("\n",
			string.Join("", Declarations.Select(x => x.FormattedString)),
			string.Join("", Statements.Select(x => x.FormattedString))
		);
	}
}
