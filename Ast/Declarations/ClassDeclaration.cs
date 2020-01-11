using System.Collections.Generic;
using System.Linq;
namespace Lab3.Ast.Declarations {
	sealed class ClassDeclaration : IDeclaration {
		public int Position { get; }
		public readonly string Name;
		public readonly IReadOnlyList<IClassMember> Members;
		public const string Keyword = "class";
		public ClassDeclaration(int position, string name, IReadOnlyList<IClassMember> members) {
			Position = position;
			Name = name;
			Members = members;
		}
		public string FormattedString {
			get {
				var members = string.Join("", Members.Select(x => x.FormattedString));
				return $"{Keyword} {Name} {{\n{members}}}\n";
			}
		}
	}
}
