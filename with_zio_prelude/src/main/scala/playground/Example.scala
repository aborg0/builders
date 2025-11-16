package playground

object Example {
  def main(args: Array[String]): Unit = {
    println(TripleO().i(3).op("Op").v("Op"))
    println(TripleO().i(4).op("JOp").v("NonOp"))
  }

}
