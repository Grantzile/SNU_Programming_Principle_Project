package pp202302.project.impl

import pp202302.project.common.MapEnv
import pp202302.project.common._

import scala.annotation.tailrec

given mapEnvImpl[V]: EnvOps[MapEnv[V], V] with
  def emptyEnv() = MapEnv[V](Map.empty[String, V]:: Nil)

  extension (env: MapEnv[V])
    def pushEmptyFrame = {
      // env.frames의 첫번째를 복사해서 넣어야 되는 거 아님?
      MapEnv[V](Map.empty[String, V] :: env.frames)
    }

    def popFrame: MapEnv[V] = env.frames match {
      case head :: next => MapEnv[V](next)
      case _ => MapEnv[V](Nil)
    }

    def setItem(name: String, item: V): MapEnv[V] = {
      env.frames.headOption match {
        case Some(value) => MapEnv(value + (name -> item) :: env.frames.tail)
        case _ => env
      }
    }
 
    def findItem(name: String): Option[V] = {
      def searchItem(frames: List[Map[String, V]], name: String): Option[V] = frames match {
        case head :: tail => {
          head.get(name) match {
            case Some(value) => Option(value)
            case None => searchItem(tail, name)
          }
        }
        case Nil => throw new Error(s"No such item: ${name}")// Option.empty[V]
      }
      searchItem(env.frames, name)
    }
