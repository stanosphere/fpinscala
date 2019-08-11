case class MyBool(x: Boolean) {
  def and(that: MyBool): MyBool = if (x) that else this
  def or(that: MyBool): MyBool = if (x) this else that
  def negate: MyBool = MyBool(!x)

}

object MyBool  {
  def not(x: MyBool): MyBool = x.negate
  def xor(x: MyBool, y: MyBool): MyBool = (x or y) and not(x and y)
}