import org.scalacheck._
import org.scalacheck.Gen.{alphaStr, listOf, numChar}
import org.scalacheck.Prop.{AnyOperators, BooleanOperators}

object StringUtilsProps extends Properties("StringUtils") {
  //  property("truncate")={
  //    Prop.forAll{(s:String, n:Int)=>
  //     lazy val t= StringUtils.truncate(s,n)
  //      if(n<0)
  //        Prop.throws(
  //          classOf[StringIndexOutOfBoundsException]){t}
  //      else
  //        (s.length <= n && t==s) || (s.length > n && t==s.take(n)+ "...")
  //    }
  //  }

  property("truncate") = {
    Prop.forAll { (s: String, n: Int) =>
      (n >= 0) ==> {
        val t = StringUtils.truncate(s, n)
        (s.length <= n && t == s) || (s.length > n && t == s.take(n) + "...")
      }

    }
  }


  property("tokenize") =
    Prop.forAll(listOf(alphaStr), numChar) {
      (ts, d) =>
        val str = ts.mkString(d.toString)
        StringUtils.tokenize(str, d).toList ?= ts
    }

  property("contains") = Prop.forAll {
    (s1: String, s2: String, s3: String) =>
      StringUtils.contains(s1 + s2 + s3, s2)
  }
}
