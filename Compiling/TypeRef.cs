using Mono.Cecil;
using System;
namespace Lab3.Compiling {
	struct TypeRef : IEquatable<TypeRef> {
		public readonly TypeReference TypeReference;
		public TypeRef(TypeReference typeReference) {
			TypeReference = typeReference;
		}
		public static implicit operator TypeRef(TypeReference typeReference) {
			return new TypeRef(typeReference);
		}
		public bool Equals(TypeRef other) {
			var x = TypeReference;
			var y = other.TypeReference;
			return x == y || x != null && y != null && x.FullName == y.FullName;
		}
		public override bool Equals(object obj) {
			return obj is TypeRef && Equals((TypeRef)obj);
		}
		public override int GetHashCode() {
			return TypeReference == null ? 0 : TypeReference.FullName.GetHashCode();
		}
		public static bool operator ==(TypeRef a, TypeRef b) {
			return a.Equals(b);
		}
		public static bool operator !=(TypeRef a, TypeRef b) {
			return !a.Equals(b);
		}
		public TypeDefinition GetTypeDefinition() {
			return TypeReference.Resolve();
		}
		public bool CanBeNull {
			get {
				if (TypeReference == null) {
					return true;
				}
				return !TypeReference.IsValueType;
			}
		}
	}
}
