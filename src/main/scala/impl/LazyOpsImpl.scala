package pp202302.project.impl

import pp202302.project.common._

given LazyOps[Val, LazyVal[Val]] with
  def pend(pending: () => Val): LazyVal[Val] =
    LVLazy[Val](pending) // () => Val을 받아 LazyVal[Val]을 생성하는 생성자 또는 팩토리 메서드를 가정

  extension (value: Val) 
    def toLazy: LazyVal[Val] =
      LVLazy[Val](() => value) // 즉시 평가된 값을 지연 평가될 값으로 변환

  extension (value: LazyVal[Val]) 
    def evaluate: Val = value match {
      case LVLazy(v) => v()
      case LVVal(v) => v
    }
      // LazyVal 내부의 계산을 실행하고 결과를 반환
