package structureextractor.util

import java.io.Closeable

/**
 * This is provides the [[use]] extension method to [[Closeable]]s (e.g., [[FileWriter]]s).
 * 
 * [[use]] invokes the provided function on the [[Closeable]], and then [[close()]]s it, so that 
 * the known lifetime of the [[Closeable]] is clearly visible, with no possibility of forgetting to
 * close it in some abstruse circumstance.
 */
class ManagedExtension[A <: Closeable](resource: A) {
	var used = false

	def use[R](f: A => R): R = {
		if (used)
			throw new AssertionError("Code bug resulted in second invocation of Managed.use")
		used = true

		try {
			f(resource)
		} finally {
			resource.close()
		}
	}
}

object ManagedExtension {
	implicit def apply[A <: Closeable](resource: A) = new Managed(resource)
}

/**
	* The non-"extension" version must be constructed explicitly, which results in easier-to-read
	* code, in my opinion.
	* 
	* For syntactic convenience, this version makes [[apply]] a synonym of [[use]].
	*/
class Managed[A <: Closeable](resource: A) extends ManagedExtension(resource) {
	def apply[R](f: A => R): R = use(f)
}

object Managed {
	def apply[A <: Closeable](resource: A) = new Managed(resource)
}

