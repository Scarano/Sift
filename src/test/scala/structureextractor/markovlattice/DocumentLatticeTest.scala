package structureextractor.markovlattice

import org.scalatest.funsuite.AnyFunSuite

import DocumentLattice.labelPartitions

class DocumentLatticeTest extends AnyFunSuite {
  test("labelPartitions should divide label slices at label discontinuities") {
	  val labels = Array(0, 0, 1, 1).map(Option.apply)
    assert(labelPartitions(labels, 0, 1) == List((0, 1)))
    assert(labelPartitions(labels, 0, 2) == List((0, 2)))
    assert(labelPartitions(labels, 0, 3) == List((0, 2), (2, 3)))
    assert(labelPartitions(labels, 3, 4) == List((3, 4)))
    assert(labelPartitions(labels, 0, 4) == List((0, 2), (2, 4)))
    assert(labelPartitions(labels, 1, 4) == List((1, 2), (2, 4)))
  }
}
