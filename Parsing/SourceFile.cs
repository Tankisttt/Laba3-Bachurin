using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
namespace Lab3.Parsing {
	sealed class SourceFile {
		public readonly string Path;
		public readonly string Text;
		SourceFile(string path, string text) {
			Path = path;
			Text = text;
		}
		public static SourceFile FromString(string sourceCode) {
			return new SourceFile("<memory>", sourceCode);
		}
		public static SourceFile Read(string path) {
			var text = File.ReadAllText(path);
			return new SourceFile(path, text);
		}
		static readonly Regex newLine = RegexUtils.CreateRegex(@"(?<=\r\n|\n)");
		static readonly Regex nonTab = RegexUtils.CreateRegex(@"[^\t]");
		public IEnumerable<string> FormatLines(
		  int offset,
		  int linesAround = 1,
		  bool inlinePointer = false,
		  string pointer = "^",
		  int maxLineNumberLength = 5
		  ) {
			var lines = newLine.Split(Text);
			var lineOffset = 0;
			var lineIndex = 0;
			var columnIndex = 0;
			foreach (var line in lines) {
				if (offset < lineOffset + line.Length) {
					columnIndex = offset - lineOffset;
					break;
				}
				lineOffset += line.Length;
				lineIndex += 1;
			}
			for (var i = -linesAround; i <= linesAround; i++) {
				var j = lineIndex + i;
				if (!(0 <= j && j < lines.Length)) {
					continue;
				}
				var line = lines[j].TrimEnd();
				var lineNumber = $"{j + 1}:".PadRight(maxLineNumberLength).Substring(0, maxLineNumberLength);
				var empty = new string(' ', maxLineNumberLength);
				if (i == 0) {
					if (inlinePointer) {
						yield return lineNumber + line.Substring(0, columnIndex) + pointer + line.Substring(columnIndex);
					}
					else {
						yield return lineNumber + line;
						yield return new string(' ', maxLineNumberLength) + nonTab.Replace(lines[lineIndex].Substring(0, columnIndex), " ") + pointer;
					}
				}
				else {
					yield return lineNumber + line;
				}
			}
		}
		public string MakeErrorMessage(int offset, string message) {
			return message + "\n" + string.Join("\n", FormatLines(offset));
		}
	}
}
