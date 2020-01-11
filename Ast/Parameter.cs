namespace Lab3.Ast {
	sealed class Parameter : INode {
		public int Position { get; }
		public readonly TypeNode Type;
		public readonly string Name;
		public Parameter(int position, TypeNode type, string name) {
			Position = position;
			Type = type;
			Name = name;
		}
		public string FormattedString => $"{Type.FormattedString} {Name}";
	}
}
