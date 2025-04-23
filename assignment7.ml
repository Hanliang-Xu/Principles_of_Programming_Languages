let producer_behavior = "
Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
Let producer =
  Fun me -> y (Fun this -> Fun internal_stock -> Fun msg ->
    Match msg With
    | `consume(a) -> 
      (If (internal_stock = 0)
      Then ((me <- `produce(me); (a <- `wait(0))); (this internal_stock))
      Else ((a <- `delivered(0)); (this (internal_stock - 1))))
    | `produce(_) -> (this (internal_stock + 1))
  )
In
producer
";;

let fixed_producer_tester = "
Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
Let producer =
  Fun me -> y (Fun this -> Fun internal_stock -> Fun msg ->
    Match msg With
    | `consume(a) ->
         If (internal_stock = 0)
         Then (me <- `produce_and_wait(a); this internal_stock)
         Else (a <- `delivered(0); this (internal_stock - 1))
    | `produce(_) -> this (internal_stock + 1)
    | `produce_and_wait(a) -> a <- `wait(0); this (internal_stock + 1)
  )
In
producer
";;

(*
Q2 Part c:
For our producer_behavior:
G_0: {<producer, ...>, <consumer, ...>}
G_1: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_2: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `delivered(0)}
G_3: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_4: {<producer, ...>, <consumer, ...>} ∪ {<producer, `produce(producer)>, <consumer, `wait(0)>}
Possibility 1:
G_5: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `wait(0)>}
G_6: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_7: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `delivered(0)>}
Possibility 2 (the possibility that doesn't work now):
G_5': {<producer, ...>, <consumer, ...>} ∪ {<producer, `produce(producer)>, <producer, `consume(consumer)>}
G_6': {<producer, ...>, <consumer, ...>} ∪ {<producer, `produce(producer)>, <producer, `produce(producer)>, <consumer, `wait(0)>}
G_7': {<producer, ...>, <consumer, ...>} ∪ {<producer, `produce(producer)>, <producer, `produce(producer)>, <consumer, `wait(0)>}
At this point, no message can be matched with `delivered(_), which causes the error

With our fixed_producer_tester:
G_0: {<producer, ...>, <consumer, ...>}
G_1: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_2: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `delivered(0)}
G_3: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_4: {<producer, ...>, <consumer, ...>} ∪ {<producer, `produce_and_wait(consumer)>}
G_5: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `wait(0)>}
G_6: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_7: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `delivered(0)>}

The following cycles can repeat
G_8: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_9: {<producer, ...>, <consumer, ...>} ∪ {<producer, `produce_and_wait(consumer)>}
G_10: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `wait(0)>}
...
G_k: {<producer, ...>, <consumer, ...>} ∪ {<producer, `consume(consumer)>}
G_{k+1}: {<producer, ...>, <consumer, ...>} ∪ {<consumer, `delivered(0)>}
*)

let consumer_behavior = "
Let y = (Fun b -> Let w = Fun s -> Fun m -> b (s s) m In w w) In
Let consumer = 
  Fun me -> y (Fun this -> Fun current_state ->
    Let producer = Fst(current_state) In
    Let demand = Snd(current_state) In
    Fun msg ->
      Match msg With
      | `purchase(_) -> ((producer <- `consume(me)); this current_state)
      | `wait(_) -> ((producer <- `consume(me)); this current_state)
      | `delivered(_) ->
        (Let new_demand = (demand - 1) In
        (If (new_demand = 0) Then 0 Else (me <- `purchase(0)); this (producer, new_demand)))
  ) In
consumer
";;