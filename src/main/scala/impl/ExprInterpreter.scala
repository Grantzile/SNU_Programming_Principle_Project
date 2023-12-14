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
    def appHandler(f:Expr, args: List[Expr]): V = {
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
        lazyOps.evaluate(eval(env, f)) match {
          case VFunc(funcName: String, params: List[Arg], body: Expr, funcEnv: Env) => {
              
            val newInnerFuncEnv = getFunctionEnv(params zip args, env, funcEnv)   // TODO: 검증하기. (funcEnv가 뭐하는건지 아직 잘 모르겠음)
            val considerRecursiveFuncEnv = newInnerFuncEnv.setItem(funcName, lazyOps.toLazy(VFunc[Env](funcName, params, body, newInnerFuncEnv)))
            
            body match {
              case EApp(insideF, insideArgs) => {
                appHandler(insideF, insideArgs);
              }
              case _ => {
                lazyOps.toLazy(lazyOps.evaluate(eval(considerRecursiveFuncEnv, body)))
              }
            }
          }
          case _ => {
            throw new Exception("EApp type error")
          }
        }
      }
    e match {
      case EInt(value) => lazyOps.toLazy(VInt(value))
      case EFloat(value) => lazyOps.toLazy(VFloat(value))
      case EString(value) => lazyOps.toLazy(VString(value))
      // need test
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
            case 0 => eval(env, ifFalse)
            case _ => eval(env, ifTrue)
          }
          case VFloat(value) => value match {
            case 0.0 => eval(env, ifFalse)
            case _ => eval(env, ifTrue)
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
                // case BDefIO(
                //   f: String,
                //   params: List[Arg.AVName],
                //   actions: List[IOAction],
                //   returns: Expr
                // ) => {
                //   // env.setItem(f, (params: List[Arg.AVName], actions: List[IOAction]) => {

                //   //   for (action <- actions) {
                //   //     action match {
                //   //       case IORun(x: String, e: Expr) => {
                          
                //   //       }
                //   //     }
                //   //   }
                    
                //   // })
                // }
              }
            }
            case Nil => innerEnv
          }
        }
        val goEnv = setEnv(newEnv, bs);
        // in this point, all value has been recorded.
        eval(goEnv, e)
        // eval(setEnv(newEnv, bs), e)
      }
      case EApp(f, args) => appHandler(f, args);
      case EAdd(left, right) => {
        val leftValue = lazyOps.evaluate(eval(env, left))
        val rightValue = lazyOps.evaluate(eval(env, right))
        (leftValue, rightValue) match {
          case (VInt(leftValue), VInt(rightValue)) => lazyOps.toLazy(VInt(leftValue + rightValue))
          case (VInt(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue + rightValue))
          case (VFloat(leftValue), VInt(rightValue)) => lazyOps.toLazy(VFloat(leftValue + rightValue))
          case (VFloat(leftValue), VFloat(rightValue)) => lazyOps.toLazy(VFloat(leftValue + rightValue))
          case (VString(leftValue), VString(rightValue)) => lazyOps.toLazy(VString(leftValue + rightValue))
          case _ => throw new Exception("EAdd type error")
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
    }
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
        result match {
          case VIOThunk[Env](action, args) => ???
          case r => Success(r.toLazy)
        }
      } catch {
        case e: Exception => Failure(e)
      }
    }
