package pp202302.project.impl

import pp202302.project.common.{_, given}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

import Arg._
import Bind._
import Expr._
import IOAction._
import Val._


given exprInterpreter[Env, V](using
    envOps: EnvOps[Env, V],
    lazyOps: LazyOps[Val, V]
): Interpreter[Expr, V] with
  
  private def eval(env: Env, e: Expr): V = {


    @tailrec
    def fullHandler(env: Env, e: Expr): V = {
      println(e)
       e match {
      case EInt(value) => lazyOps.toLazy(VInt(value))
      case EFloat(value) => lazyOps.toLazy(VFloat(value))
      case EString(value) => lazyOps.toLazy(VString(value))
      case EName(x) => env.findItem(x) match {
        case Some(value) => value
        case None => throw new Exception("name not founded")
      }
      case ENil => lazyOps.toLazy(VNil)
      case ECons(head, tail) => {
        val firstValue = lazyOps.evaluate(eval(env, head))
        val secondValue = lazyOps.evaluate(eval(env, tail))
        lazyOps.toLazy(VCons(firstValue, secondValue))
      }
      case EFst(expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VCons(head, tail) => lazyOps.toLazy(head)
          case _ => throw new Exception("EFst type error")
        }
      }
      case ESnd(expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VCons(head, tail) => lazyOps.toLazy(tail)
          case _ => throw new Exception("EFst type error")
        }
      }
      case ENilP(expr: Expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VNil => lazyOps.toLazy(VInt(1))
          case _ => lazyOps.toLazy(VInt(0))
        }
      }
      case EIntP(expr: Expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VInt(x) => lazyOps.toLazy(VInt(1))
          case _ => lazyOps.toLazy(VInt(0))
        }
      }
      case EFloatP(expr: Expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VFloat(x) => lazyOps.toLazy(VInt(1))
          case _ => lazyOps.toLazy(VInt(0))
        }
      }
      case EStringP(expr: Expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VString(x) => lazyOps.toLazy(VInt(1))
          case _ => lazyOps.toLazy(VInt(0))
        }
      }
      case EPairP(expr: Expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VCons(x, y) => lazyOps.toLazy(VInt(1))
          case VNil => lazyOps.toLazy(VInt(1))
          case _ => lazyOps.toLazy(VInt(0))
        }
      }
      case ESubstr(expr: Expr, startPoint: Expr, endPoint: Expr) => {
        val value = lazyOps.evaluate(eval(env, expr))
        val startValue = lazyOps.evaluate(eval(env, startPoint))
        val endValue = lazyOps.evaluate(eval(env, endPoint))
        (value, startValue, endValue) match {
          case (VString(mainString), VInt(starts), VInt(ends)) => {
            lazyOps.toLazy(VString(mainString.substring(starts, ends)))
          }
          case _ => throw new Error("ESubStr type Error")
        }
      }
      case ELen(expr: Expr) => {
        def consCounter(cons: Val, counter: Int): Int = cons match {
          case VCons(head, tail) => {
            consCounter(tail, counter + 1)
          }
          case _ => counter
        }
        val value = lazyOps.evaluate(eval(env, expr))
        value match {
          case VString(value) => lazyOps.toLazy(VInt(value.length))
          case VCons(head, tail) => lazyOps.toLazy(VInt(consCounter(value, 0)))
          case VNil => lazyOps.toLazy(VInt(0))
          case _ => {
            print("ELen error: ")
            throw new Error("ELen type Error")
          }
        }
      }
      case EIf(cond: Expr, ifTrue: Expr, ifFalse: Expr) => {
        val result = lazyOps.evaluate(eval(env, cond))
        result match {
          case VInt(value) => value match {
            case 0 => fullHandler(env, ifFalse)
            case _ => fullHandler(env, ifTrue)
          }
          case VFloat(value) => value match {
            case 0.0 => fullHandler(env, ifFalse)
            case _ => fullHandler(env, ifTrue)
          }
          case _ => throw new Error("EIf type error")
        }
      }
      case ELet(bs: List[Bind], e: Expr) => {
        val newEnv = env.pushEmptyFrame
        @tailrec
        def setEnv(innerEnv: Env, bs: List[Bind]): Env = {
          bs match {
            case bind :: remainder => {
              bind match {
                case BDef(f: String, params: List[Arg], body: Expr) => {
                  setEnv(innerEnv.setItem(f, lazyOps.toLazy(VFunc[Env](f, params, body, innerEnv))), remainder)
                }
                case BVal(x: String, e: Expr) => {
                  // 일반 val도 lazy val로 취급하면 일단 되긴 함. 근데 이게 맞나? ㅅㅂ
                  // setEnv(innerEnv.setItem(x, eval(innerEnv, e)), remainder)
                  setEnv(innerEnv.setItem(x, lazyOps.pend(() => lazyOps.evaluate(eval(innerEnv, e)))), remainder)
                }
                case BLVal(x: String, e: Expr) => {
                  // TODO : make this lazy
                  setEnv(innerEnv.setItem(x, lazyOps.pend(() => lazyOps.evaluate(eval(innerEnv, e)))), remainder)
                }
                case BDefIO(
                  f: String,
                  params: List[Arg.AVName],
                  actions: List[IOAction],
                  returns: Expr
                ) => {
                  println(s"register VIOAction\nf: ${f}\nresult: ${VIOAction(f, params, actions, returns, innerEnv)}")
                  setEnv(innerEnv.setItem(f, VIOAction(f, params, actions, returns, innerEnv).toLazy), remainder)
                }
              }
            }
            case Nil => innerEnv
          }
        }
        val goEnv = setEnv(newEnv, bs);
        // in this point, all value has been recorded.
        // DEBUG: 안되면 eval로
        fullHandler(goEnv, e)
        // eval(setEnv(newEnv, bs), e)
      }
      case EApp(f, args) => {
        print(f)
        @tailrec
        def getFunctionEnv(paramsAndArgs: List[(Arg, Expr)], refEnv: Env, funcEnv: Env): Env = {
          paramsAndArgs match {
            case (param, argValue) :: remainder => {
              param match {
                case ANName(x) => {
                  getFunctionEnv(remainder, refEnv, funcEnv.setItem(x, lazyOps.pend(() => lazyOps.evaluate(eval(refEnv, argValue))))) 
                }
                case AVName(x) => {
                  getFunctionEnv(remainder, refEnv, funcEnv.setItem(x, eval(refEnv, argValue)))
                }
              }
            }
            case Nil => funcEnv
          }
        }

        val function = f match {
          case EName(funcName) => env.findItem(funcName) match {
            case Some(value) => value.evaluate
            case None => throw new Exception("name not founded")
          }
          case _ => throw new Error("unexpected function name")
        }
        function match {
          case VFunc(funcName: String, params: List[Arg], body: Expr, funcEnv: Env) => {
              
            val newInnerFuncEnv = getFunctionEnv(params zip args, env, funcEnv)   // TODO: 검증하기. (funcEnv가 뭐하는건지 아직 잘 모르겠음)
            val considerRecursiveFuncEnv = newInnerFuncEnv.setItem(funcName, lazyOps.toLazy(VFunc[Env](funcName, params, body, newInnerFuncEnv)))
            
            fullHandler(considerRecursiveFuncEnv, body) // 일단 임시로 cont 그대로 넣기, 나중에 넣을거임
          }
          case VIOAction(actionName: String, params: List[Arg.AVName], actions: List[IOAction], returns: Expr, funcEnv: Env) => {
            println("invoked VIOAction")
            val evaluatedVals: List[Val] =
              for arg <- args
                yield lazyOps.evaluate(eval(env, arg))
            println("End VIOAction?")

            VIOThunk(VIOAction(actionName, params, actions, returns, funcEnv), evaluatedVals).toLazy
            // val newInnerFuncEnv = getFunctionEnv(params zip args, env, funcEnv)
            // val considerRecursiveFuncEnv = newInnerFuncEnv.setItem(actionName, lazyOps.toLazy(VIOAction[Env](actionName, params, actions, returns, newInnerFuncEnv)))
            // fullHandler(considerRecursiveFuncEnv, returns, cont)
          }
          case something => {
            println(something)
            throw new Exception("EApp type error")
          }
        }
      };
      case EAdd(left, right) => {
        println(s"left:${left}")
        println(s"right:${right}")
        val leftValue = eval(env, left).evaluate
        val rightValue = eval(env, right).evaluate
        (leftValue, rightValue) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(leftValue + rightValue))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue + rightValue))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VFloat(leftValue + rightValue))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue + rightValue))
          case (VString(leftValue), VString(rightValue)) => lazyOps.toLazy(VString(leftValue + rightValue))
          case remaining => {
            println(s"!!remaining!! : ${remaining}")
            throw new Exception("EAdd type error")
          }
        }
      }
      case ESub(left, right) => {
        val leftValue = lazyOps.evaluate(eval(env, left))
        val rightValue = lazyOps.evaluate(eval(env, right))
        (leftValue, rightValue) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(leftValue - rightValue))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue - rightValue))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VFloat(leftValue - rightValue))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue - rightValue))
        }
      }
      case EMul(left, right) => {
        val leftValue = lazyOps.evaluate(eval(env, left))
        val rightValue = lazyOps.evaluate(eval(env, right))
        (leftValue, rightValue) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(leftValue * rightValue))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue * rightValue))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VFloat(leftValue * rightValue))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue * rightValue))
        }
      }
      case EDiv(left, right) => {
        // Take care of div-zero
        val leftValue = lazyOps.evaluate(eval(env, left))
        val rightValue = lazyOps.evaluate(eval(env, right))
        (leftValue, rightValue) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(leftValue / rightValue))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue / rightValue))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VFloat(leftValue / rightValue))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue / rightValue))
          case _ => throw new Error("EDiv type error")
        }
      }
      case EMod(left, right) => {
        val leftValue = lazyOps.evaluate(eval(env, left))
        val rightValue = lazyOps.evaluate(eval(env, right))
        (leftValue, rightValue) match {
          case (VInt(leftValue), VInt(rightValue)) => {
            lazyOps.toLazy(VInt(leftValue % rightValue))
          }
          case _ => throw new Error("EMod type error")
        }
      }
      case EEq(left: Expr, right: Expr) => {
        val leftResult = lazyOps.evaluate(eval(env, left))
        val rightResult = lazyOps.evaluate(eval(env, right))
        (leftResult, rightResult) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(if (leftValue == rightValue) 1 else 0))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VInt(if (leftValue == rightValue) 1 else 0))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(if (leftValue == rightValue) 1 else 0))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VInt(if (leftValue == rightValue) 1 else 0))
          case (VString(leftValue), VString(rightValue)) => lazyOps.toLazy(VInt(if (leftValue == rightValue) 1 else 0))
          //case (VCons(leftFirstValue, leftSecondValue), VCons(rightFirstValue, rightSecondValue)) => lazyOps.toLazy(VInt(if (leftValue == rightValue && leftSecondValue == rightSecondValue) 1 else 0))
          case (VNil, VNil) => lazyOps.toLazy(VInt(1))
          // any other case?
          case _ => lazyOps.toLazy(VInt(0))
        }
        
      }
      case ELt(left: Expr, right: Expr) => {
        val leftResult = lazyOps.evaluate(eval(env, left))
        val rightResult = lazyOps.evaluate(eval(env, right))
        (leftResult, rightResult) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(if (leftValue < rightValue) 1 else 0))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VInt(if (leftValue < rightValue) 1 else 0))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(if (leftValue < rightValue) 1 else 0))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VInt(if (leftValue < rightValue) 1 else 0))
        }
      }
      case EGt(left: Expr, right: Expr) => {
        val leftResult = lazyOps.evaluate(eval(env, left))
        val rightResult = lazyOps.evaluate(eval(env, right))
        (leftResult, rightResult) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(if (leftValue > rightValue) 1 else 0))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VInt(if (leftValue > rightValue) 1 else 0))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(if (leftValue > rightValue) 1 else 0))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VInt(if (leftValue > rightValue) 1 else 0))
        }
      }
    }}
    fullHandler(env, e);
  }

  extension (e: Expr)
    def evaluate(): Try[V] = {
      try {
        Success(eval(envOps.emptyEnv(), e))
      } catch {
        case e: Exception => Failure(e)
      }
    }

    def interpret[R, W](reader: R, writer: W)(using
        showOps: Show[V],
        readOps: Reader[R],
        writeOps: Writer[W]
    ): Try[V] = {
      try {
        val result = eval(envOps.emptyEnv(), e).evaluate
        println("result:")
        println(result); // 지울 것
        result match {
          case VIOThunk[Env](action, args) =>
          {
            def fetchLine(): Option[String] = {
              println("invoked")
              readOps.readChar(reader)() match {
                case Some(value) => {
                // what about "\r\n"?
                  if (value == '\r' || value == '\n'){
                    None;
                  }
                  else {
                    val tails = fetchLine() match {
                      case Some(tailString) => tailString
                      case None => ""
                    }
                    Option(value.toString + tails)
                  }
                }
                case None => None
              }
            }

            @tailrec
            def printString(targetString: String): Unit = {
              if (targetString.nonEmpty){
                writeOps.writeChar(writer)(targetString.head)
                printString(targetString.tail)
              }
            }
            def runActions(env: Env, actions: List[IOAction]): Env = actions match {
              case IORun(x: String, e: Expr) :: remainder => {
                println(s"IORun: ${e}")
                val newEnv = env.setItem(x, eval(env, e).evaluate.toLazy);
                runActions(newEnv, remainder);
              }
              case IOReadLine(x: String) :: remainder => {
                fetchLine() match {
                  case Some(fetchedString) => {
                    val newEnv = env.setItem(x, VString(fetchedString).toLazy);
                    runActions(newEnv, remainder);
                  }
                  case None => throw new Error("Empty Line Input");
                }
              }
              case IOPrint(e: Expr) :: remainder => {
                def evalStringPrinter(evaluatedValue: Val): Unit = {
                  evaluatedValue match {
                    case VNil                         => printString("Nil")
                    case VInt(value)                  => printString(value.toString)
                    case VFloat(value)                => printString(value.toString)
                    case VString(stringValue)         => printString(stringValue)
                    case VCons(head: Val, tail: Val)  => {
                      printString("(")
                      evalStringPrinter(head)
                      printString(",")
                      evalStringPrinter(tail)
                      printString(")")
                    }
                  }
                }
                evalStringPrinter(eval(env, e).evaluate);
                runActions(env, remainder);
              }
              case Nil => env
            }
            def registerArgs(env: Env, registerParams: List[(Arg.AVName, Val)]): Env = registerParams match {
              case (Arg.AVName(name), value) :: remainder => {
                println(s"try to register ${name}!")
                val newEnv = env.setItem(name, value.toLazy)
                registerArgs(newEnv, remainder)
              }
              case Nil => env;
            }
            val newEnv = registerArgs(action.env, action.params zip args)
            val finalEnv = runActions(newEnv, action.actions)
            Success(eval(finalEnv, action.returns))
          }
          case r => Success(r.toLazy)
        }
      } catch {
        case e: Exception => Failure(e)
      }
    }
