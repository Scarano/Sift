package structureextractor.util

import java.io.Closeable

class Managed[A <: Closeable](resource: A) {
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

object Managed {
	implicit def apply[A <: Closeable](resource: A) = new Managed(resource)
}
