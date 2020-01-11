namespace Lab3.Ast.ClassMembers {
	sealed class ClassField : IClassMember {
		public int Position { get; }
		public readonly TypeNode Type;
		public readonly string Name;
		public ClassField(int position, TypeNode type, string name) {
			Position = position;
			Type = type;
			Name = name;
		}
		public string FormattedString => $"{Type.FormattedString} {Name};\n";
	}
}
