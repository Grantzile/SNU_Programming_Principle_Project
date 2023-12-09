// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html

import org.parboiled2.{ErrorFormatter, ParseError}
import pp202302.project.common.{_, given}
import pp202302.project.impl.{_, given}

import scala.annotation.tailrec
import scala.util.{Failure, Success}

class TestSuite extends munit.FunSuite {
  import Val._
  import Arg._
  import Bind._
  import Expr._
  import IOAction._

  def parse(str: String): Expr =
    val parser = new ExprParser(str)
    val result = parser.Input.run()
    result match
      case Success(v) => v
      case Failure(e: ParseError) =>
        throw new RuntimeException(
          parser.formatError(e, new ErrorFormatter(showTraces = true))
        )
      case Failure(e) =>
        e.printStackTrace()
        throw new RuntimeException(e)

  def eval(code: String): Val =
    parse(code).evaluate().get.evaluate

  def execIO(code: String, input: String): (Val, String) =
    val dummyIO: IOType.DummyIO = IOType.DummyIO(input, "")
    val result = parse(code).interpret(dummyIO, dummyIO).get.evaluate
    (result, dummyIO.output)

  def runAssert(code: String, expect: Val) =
    assertEquals(eval(code), expect)

  def runAssertIO(code: String, expect: Val, input: String, output: String) =
    val (result, outputResult) = execIO(code, input)
    assertEquals(result, expect)
    assertEquals(outputResult, output)

  def runCheckIO(code: String, input: String, output: String) =
    val (result, outputResult) = execIO(code, input)
    assertEquals(outputResult, output)

  // test("Basic parse") {
  //   runAssert("3", VInt(3))
  //   runAssert("3.5", VFloat(3.5))
  // }

  // test("Basic let") {
  //   val adder =
  //     """
  //       |(let (val x 1) (val y 2) (+ x y))
  //       |""".stripMargin

  //   runAssert(adder, VInt(3))
  // }

  // test("Basic let float") {
  //   val mul =
  //     """
  //       |(let (val x 3) (val y 2.5) (* x y))
  //       |""".stripMargin

  //   runAssert(mul, VFloat(7.5f))
  // }

  // test("Basic let string") {
  //   val adder =
  //     """
  //       |(let (val x "Hello") (val y ", world!") (+ x y))
  //       |""".stripMargin

  //   runAssert(adder, VString("Hello, world!"))
  // }

  // test("Basic def") {
  //   val adder =
  //     """
  //       |(let (def f (x) (+ x -1)) (app f 2.0))
  //       |""".stripMargin

  //   runAssert(adder, VFloat(1.0f))
  // }

  // test("Basic let*") {
  //   val adder =
  //     """
  //       |(let (val x 3) (val y (+ x 5)) (+ x y))
  //       |""".stripMargin

  //   runAssert(adder, VInt(11))
  // }

  // test("Basic def*") {
  //   val adder =
  //     """
  //       |(let
  //       |  (val x 3)
  //       |  (def y () 5)
  //       |  (def f (y) (+ x y))
  //       |  (+ x (+ (app y) (app f 5)))
  //       |)
  //       |""".stripMargin

  //   runAssert(adder, VInt(16))
  // }

  // test("Basic lazy val*") {
  //   val adder =
  //     """
  //       |(let (val x 3) (lazy-val y (+ x 5)) (+ x y))
  //       |""".stripMargin

  //   runAssert(adder, VInt(11))
  // }

  // test("First-order function") {
  //   val adder =
  //     """
  //       |(let
  //       |  (def mul2 (n) (* n 2))
  //       |  (def g (f x) (+ (app f 4) x))
  //       |  (app g mul2 5))
  //       |""".stripMargin

  //   runAssert(adder, VInt(13))
  // }

  // test("Basic pair type") {
  //   val code =
  //     """
  //       |(let (val x 13) (val y 3) (cons (/ x y) (% x y)))
  //       |""".stripMargin

  //   runAssert(code, VCons(VInt(4), VInt(1)))
  // }

  // test("Basic comparison") {
  //   val code1 = "(if (< 4 1) 0 3)"
  //   val code2 = "(if (> 4 1) 3 0)"
  //   val code3 = "(if (= 4 4) 3 0)"
  //   val code4 = "(if (= 4.0 4) 3 0)"
  //   val code5 = "(if (= 2 4.0) 0 3)"
  //   val code6 = """(if (= "str" "str") 3 0)"""
  //   val code7 = """(if (= "str" "str2") 0 3)"""
  //   val code8 = """(if (= "str" 1) 0 3)"""
  //   runAssert(code1, VInt(3))
  //   runAssert(code2, VInt(3))
  //   runAssert(code3, VInt(3))
  //   runAssert(code4, VInt(3))
  //   runAssert(code5, VInt(3))
  //   runAssert(code6, VInt(3))
  //   runAssert(code7, VInt(3))
  //   runAssert(code8, VInt(3))
  // }

  // test("Basic if") {
  //   val code =
  //     """
  //       |(let
  //       |  (val x 3)
  //       |  (val y 5)
  //       |  (if (< x y) (+ x y) (- x y)))
  //       |""".stripMargin

  //   runAssert(code, VInt(8))
  // }

  // test("Basic if2") {
  //   val code =
  //     """
  //       |(let
  //       |  (val x 3)
  //       |  (val y 5)
  //       |  (if (> x y) (+ x y) (- x y)))
  //       |""".stripMargin

  //   runAssert(code, VInt(-2))
  // }

  // test("Basic if3") {
  //   val code =
  //     """
  //       |(let
  //       |  (val x 3)
  //       |  (val y 5)
  //       |  (if (= x y) (+ x y) (- x y)))
  //       |""".stripMargin

  //   runAssert(code, VInt(-2))
  // }

  // test("Baic recursion") {
  //   val adder =
  //     """
  //       |(let
  //       |  (def sum (n)
  //       |       (if (> n 0)
  //       |           (+ (app sum (- n 1)) n)
  //       |           0))
  //       |  (app sum 5))""".stripMargin

  //   runAssert(adder, VInt(15))
  // }

  // test("Fibonacci(10)") {
  //   val fib =
  //     """(let
  //       |  (def fib (n)
  //       |       (if (> n 0)
  //       |           (if (> n 1)
  //       |               (+ (app fib (- n 1)) (app fib (- n 2)))
  //       |               1)
  //       |           0))
  //       |  (app fib 10))""".stripMargin

  //   runAssert(fib, VInt(55))
  // }

  test("andb(true, true)") {
    val andb =
      """(let
        |  (def andb (a b) (if a b 0))
        |  (val x (> 3 1))
        |  (val y (< 2 3))
        |  (app andb x y)
        |)
      """.stripMargin

    runAssert(andb, VInt(1))
  }

  // test("isdiv(15, 5)") {
  //   val isdiv =
  //     """(let
  //       |  (def isdiv (n p) (= (% n p) 0))
  //       |  (app isdiv 15 5)
  //       |)
  //     """.stripMargin

  //   runAssert(isdiv, VInt(1))
  // }

  // test("predicates") {
  //   val checkPred =
  //     """(let
  //       |  (def notb (p) (if p 0 1))
  //       |  (def andb (a b) (if a b 0))
  //       |  (val nilp (app andb (nil? nil) (app notb (nil? (cons 3 nil)))))
  //       |  (val intp (app andb (int? 3) (app notb (int? 3.0))))
  //       |  (val floatp (app andb (float? 3.0) (app notb (float? 3))))
  //       |  (val stringp (app andb (string? "") (app notb (string? 3))))
  //       |  (val pairp (app andb (app andb (pair? nil) (pair? (cons 3 nil))) (app notb (pair? 3))))
  //       |  (app andb nilp (app andb intp (app andb floatp (app andb stringp pairp))))
  //       |)
  //       |""".stripMargin

  //   runAssert(checkPred, VInt(1))
  // }

  // test("basic len") {
  //   val isdiv =
  //     """(let
  //       |  (val slen (len "asdfe"))
  //       |  (val clen (len (cons 3 (cons "a" (cons 1.0 nil)))))
  //       |  (+ slen clen)
  //       |)
  //         """.stripMargin

  //   runAssert(isdiv, VInt(8))
  // }

  // test("basic substr") {
  //   val isdiv =
  //     """(let
  //       |  (def remove (s n) (+ (substr s 0 n) (substr s (+ n 1) (len s))))
  //       |  (val test "string")
  //       |  (app remove test 2)
  //       |)
  //           """.stripMargin

  //   runAssert(isdiv, VString("sting"))
  // }

  // test("basic substr2") {
  //   val isdiv =
  //     """(let
  //       |  (def remove (s n) (+ (substr s 0 n) (substr s (+ n 1) (len s))))
  //       |  (val test "string")
  //       |  (app remove test 0)
  //       |)
  //           """.stripMargin

  //   runAssert(isdiv, VString("tring"))
  // }

  // test("forall(isdiv, 5, [10; 5; 15])") {
  //   val isdiv =
  //     """(let
  //       |  (def isdiv (n p) (= (% n p) 0))
  //       |  (def andb (a b) (if a b 0))
  //       |  (let
  //       |    (def forall (f n lst)
  //       |         (if (nil? lst)
  //       |             1
  //       |             (app andb (app f (fst lst) n) (app forall f n (snd lst)))
  //       |         ))
  //       |    (app forall isdiv 5 (cons 10 nil))
  //       |  )
  //       |)
  //       """.stripMargin

  //   runAssert(isdiv, VInt(1))
  // }

  // test("forall(isdiv, 5, [10; 5; 15; 31])") {
  //   val isdiv =
  //     """(let
  //       |  (def isdiv (n p) (= (% n p) 0))
  //       |  (def andb (a b) (if a b 0))
  //       |  (let
  //       |    (def forall (f n lst)
  //       |         (if (nil? lst)
  //       |             1
  //       |             (app andb (app f (fst lst) n) (app forall f n (snd lst)))
  //       |         ))
  //       |    (app forall isdiv 5 (cons 10 (cons 5 (cons 15 (cons 31 nil)))))
  //       |  )
  //       |)
  //     """.stripMargin

  //   runAssert(isdiv, VInt(0))
  // }

  // test("snoc [1; 2; 3; 4] 10") {
  //   val snoc =
  //     """(let
  //       |    (def snoc (tl hd)
  //       |         (if (pair? tl)
  //       |             (cons (fst tl) (app snoc (snd tl) hd))
  //       |             (cons tl hd)
  //       |         )
  //       |    )
  //       |    (app snoc (cons 1 (cons 2 (cons 3 4))) 10)
  //       |)
  //     """.stripMargin
  //   runAssert(
  //     snoc,
  //     VCons(VInt(1), VCons(VInt(2), VCons(VInt(3), VCons(VInt(4), VInt(10)))))
  //   )
  // }

  // test("by-name") {
  //   runAssert(
  //     """
  //       |(let
  //       |  (lazy-val x (+ 3 5))
  //       |  (def f ((by-name a) (by-name b)) (+ a b))
  //       |  (app f x (+ 1 2)))
  //       |""".stripMargin,
  //     VInt(11)
  //   )
  // }

  // test("lazy infinite loop") {
  //   runAssert(
  //     """
  //       |(let
  //       |  (def loop () (app loop))
  //       |  (lazy-val x (app loop))
  //       |  (def f (a (by-name b)) a)
  //       |  (val bb (app f x (app loop)))
  //       |  3)
  //       |""".stripMargin,
  //     VInt(3)
  //   )
  // }

  // test("tailrec") {
  //   val recrec =
  //     """
  //       |(let
  //       |  (def f (x sum fn)
  //       |       (if (> x 0)
  //       |           (app fn (- x 1) (+ x sum))
  //       |           sum
  //       |       ))
  //       |  (def f2 (x sum)
  //       |       (if (> x 0)
  //       |           (app f (- x 1) (+ x sum) f2)
  //       |           sum
  //       |       ))
  //       |  (app f 20000 0 f2))
  //       |""".stripMargin
  //   runAssert(recrec, VInt(200010000))
  // }

  // test("basic read") {
  //   val read =
  //     """
  //       |(let
  //       |  (defIO f (x)
  //       |    (readline y)
  //       |    (+ x y))
  //       |  (app f "3"))
  //       |""".stripMargin

  //   runAssertIO(read, VString("35"), "5", "")

  //   val read2 =
  //     """
  //       |(let
  //       |  (defIO f (x)
  //       |    (readline y)
  //       |    (readline z)
  //       |    (+ (+ x y) z))
  //       |  (app f "3"))
  //       |""".stripMargin

  //   runAssertIO(read2, VString("31234"), "12\n34\n56", "")
  // }

  // test("basic print") {
  //   val print =
  //     """
  //     |(let
  //     |  (defIO f (x)
  //     |    (print x)
  //     |    1)
  //     |  (app f "3"))
  //     |""".stripMargin

  //   runAssertIO(print, VInt(1), "", "3")
  // }

  // test("runIO") {
  //   val runio =
  //     """
  //       |(let
  //       |  (defIO f (x)
  //       |     (readline y)
  //       |     (+ x y))
  //       |  (defIO g (x)
  //       |     (runIO y (app f x))
  //       |     (readline z)
  //       |     (runIO w (app f z))
  //       |     (runIO a (+ x (+ y (+ z w))))
  //       |     (print a)
  //       |     w)
  //       |  (app g "3")
  //       |)
  //       |""".stripMargin

  //   runAssertIO(runio, VString("bc"), "a\nb\nc\nd", "33abbc")

  // }

  // test("action gen") {
  //   // You must use s"""...""" to enable escaped character '\n' in a triple-quoted string.
  //   // Normal """...""" handle a backslash as a literal backslash.
  //   val genio =
  //     s"""
  //       |(let
  //       |  (def isEq (x)
  //       |    (let
  //       |      (defIO rw ()
  //       |         (readline input)
  //       |         (runIO p (if (= input x) "true\n" "false\n"))
  //       |         (print p)
  //       |         1)
  //       |      rw
  //       |    )
  //       |  )
  //       |  (val is3 (app isEq "3"))
  //       |  (val is4 (app isEq "4"))
  //       |
  //       |  (defIO run34 ()
  //       |    (runIO _ (app is3))
  //       |    (runIO _ (app is4))
  //       |    (runIO _ (app is3))
  //       |    0
  //       |  )
  //       |  (app run34)
  //       |)
  //       |""".stripMargin

  //   runAssertIO(genio, VInt(0), "3\n4\n5\n6", "true\ntrue\nfalse\n")
  // }

  // test("calculator") {
  //   runCheckIO(calculator, "1+2\nexit\n", "3\n")
  //   runCheckIO(calculator, "10+2\n2+3*5\nexit\n", "12\n17\n")
  // }
}
