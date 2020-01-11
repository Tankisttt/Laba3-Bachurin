namespace Lab3.Ast {
	class TypeNode : INode {
		public int Position { get; }
		public readonly string Name;
		public TypeNode(int position, string name) {
			Position = position;
			Name = name;
		}
		public string FormattedString => Name;
	}
}
