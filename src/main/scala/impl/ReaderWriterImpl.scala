package pp202302.project.impl

import pp202302.project.common._

import IOType._

given Reader[IOType] with
  extension (r: IOType)
    // Hint: Use scala.Console.in.read().toChar
    def readChar(): Option[Char] = r match {
      case StdIO => {
        Option(scala.Console.in.read().toChar)
      }
      case dummy: DummyIO => dummy.input.length() match {
        case 0 => {
          None
        }
        case _ => {
          def removeRemainingEndlines(dummy: DummyIO): Unit = {
            for (_ <- 0 until dummy.input.length) {
              if (dummy.input.nonEmpty && (dummy.input.head == '\n' || dummy.input.head == '\r')) {
                dummy.input = dummy.input.tail
              } else {
                return
              }
            }
          }
          val result = dummy.input.head
          dummy.input = dummy.input.tail
          if (result == '\n' || result == '\r'){
            // fix this!
            removeRemainingEndlines(dummy)
          }
          Option(result)
        }
      }
    }

given Writer[IOType] with
  extension (w: IOType)
    def writeChar(c: Char): Unit = w match {
      case StdIO => {
        print(c)
      }
      case dummy: DummyIO => {
        dummy.output += c
      }
    }
