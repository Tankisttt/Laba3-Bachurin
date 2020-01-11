using System;
namespace BuiltinTypes {
	public static class BuiltinFunctions {
		public static void dump() {
			Console.WriteLine();
		}
		public static void dump(int v) {
			Console.WriteLine(v);
		}
		public static void dump(bool v) {
			Console.WriteLine(v ? "true" : "false");
		}
		public static int trace(int v) {
			Console.Write($">> ");
			dump(v);
			return v;
		}
		public static bool trace(bool v) {
			Console.Write($">> ");
			dump(v);
			return v;
		}
	}
}
