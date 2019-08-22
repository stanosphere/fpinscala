package trees

object PlayingWithTrees extends App {
  val square = (x: Int) => x * x
  val orphan = Leaf(1)
  val wheel = Node(1, List(Leaf(19), Leaf(2)))
  println(orphan.map(_ + 1))
  println(wheel.map(square))
  println(wheel.traverse(square)(_+_))
}
