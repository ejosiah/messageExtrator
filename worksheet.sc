val aList = List.empty[String]

aList.fold("Hello")((acc, a) => s"$acc, $a")
aList.mkString(", ")