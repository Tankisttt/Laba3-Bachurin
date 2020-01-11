namespace Lab3.Ast {
	interface INode {
		int Position { get; }
		string FormattedString { get; }
	}
}
