module OrchestrationCE.Orchestration
open OrchestrationCE.Coordination
open OrchestrationCE.GenericOrchestration
       
type CircuitBreaker<'T1, 'T2> =
    | Continue of 'T1
    | Break of 'T2 list
    
module CircuitBreaker =
    let map f = function
        | Continue y -> Continue (f y)
        | Break y -> Break y

    let mapBreak f = function
        | Continue y -> Continue y
        | Break y -> Break (f y)

    let retn x =
        Continue x

    let combine breakf continuef = function
        | Continue x -> continuef x
        | Break x -> breakf x
        
    let merge = function
        | Continue x, Continue y -> Continue (x, y)
        | Break x, Continue _
        | Continue _, Break x -> Break x
        | Break x, Break y -> Break (x @ y)
    
type Orchestration<'a, 'b, 'c> = Coordination<'a option, CircuitBreaker<'b, 'c>>

let rec raiseToOrchestration workflow = function
    | Some event ->
        let { Result = result; Next = next } = workflow event
        { Result = result |> List.map Continue
          Next = Option.map raiseToOrchestration next }
    | None ->
        { Result = []; Next = None }
        
let rec raiseToOrchestrationWithActions actions workflow = function
    | Some event ->
        let { Result = result; Next = next } = workflow event
        { Result = result |> List.map Continue
          Next = Option.map (raiseToOrchestrationWithActions actions) next }
    | None ->
        { Result = [(Break actions)]; Next = None }
        
let switchMap f = switchMap (CircuitBreaker.combine (Break >> Coordination.retn)) f
let mergeMap f = mergeMap (CircuitBreaker.combine (Break >> Coordination.retn)) f
let concatMap f = concatMap (CircuitBreaker.combine (Break >> Coordination.retn)) f
let exhaustMap f = exhaustMap (CircuitBreaker.combine (Break >> Coordination.retn)) f

let mapBreak f = Coordination.map (CircuitBreaker.combine (List.map f >> Break) (id >> Continue))

type OrchestrationBuilder() =

    member _.Bind (m, f) =
        m |> take 1 |> switchMap f
   
    member _.Return(result) =
        retn CircuitBreaker.retn result
        
    member _.ReturnFrom (m) =
        m
        
    member _.MergeSources(orchestration1, orchestration2) =
        zip CircuitBreaker.merge (orchestration1 |> take 1) (orchestration2 |> take 1)
    
    member _.Zero() =
        retn CircuitBreaker.retn ()

    member _.Yield(x) =
        retn CircuitBreaker.retn x
        
    member _.YieldFrom(x) =
        CircuitBreaker.retn x

    member _.Combine(a,b) =
        appendPair a b

    member _.Delay(f) =
        f()
    
let orchestration = OrchestrationBuilder()