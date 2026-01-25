package playground

object Example {
  def main(args: Array[String]): Unit = {
    println(TripleValO().i(3).op("Op").v("Op").s(2))
    println(TripleValVal().i(4).op("JOp").v("NonOp").s(-2))
  }

}
