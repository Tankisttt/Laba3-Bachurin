using System.Text.RegularExpressions;
namespace Lab3.Parsing {
	static class RegexUtils {
		const RegexOptions CoolRegexOptions =
			RegexOptions.Compiled |
			RegexOptions.CultureInvariant |
			RegexOptions.ExplicitCapture |
			RegexOptions.IgnorePatternWhitespace |
			RegexOptions.Singleline |
			RegexOptions.Multiline |
			RegexOptions.None;
		public static Regex CreateRegex(string pattern) {
			return new Regex(pattern, CoolRegexOptions);
		}
	}
}
