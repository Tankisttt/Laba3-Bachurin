using Lab3.Ast;
using Lab3.Compiling;
using Lab3.Parsing;
using Mono.Cecil;
using System;
using System.Reflection;
namespace Lab3 {
	sealed class Program {
		static ProgramNode CheckedParse(SourceFile sourceFile) {
			var programNode = Parser.Parse(sourceFile);
			var code2 = programNode.FormattedString;
			var programNode2 = Parser.Parse(SourceFile.FromString(code2));
			var code3 = programNode2.FormattedString;
			if (code2 != code3) {
				Console.WriteLine(code2);
				Console.WriteLine(code3);
				throw new Exception($"Кривой парсер или {nameof(INode.FormattedString)} у узлов");
			}
			return programNode;
		}
		static void Main(string[] args) {
			var sourceFile = SourceFile.Read("../../code.txt");
			var programNode = CheckedParse(sourceFile);
			var module = ModuleDefinition.CreateModule("out", ModuleKind.Console);
			var types = new AllTypes(module);
			var programCompiler = new ProgramCompiler(types, programNode, "Entry", "Main");
			programCompiler.Compile();
			module.EntryPoint = programCompiler.MainMethod;
			module.Write("out.exe");
			Assembly.LoadFrom("out.exe").GetType("Entry").GetMethod("Main").Invoke(null, new object[] { });
		}
	}
}
