module OrchestrationCE.Coordination

type Coordination<'TEvent, 'TData> = 'TEvent -> CoordinationResult<'TEvent, 'TData>
and CoordinationResult<'TEvent, 'TData> =
    { Result: 'TData list
      Next: Coordination<'TEvent, 'TData> option }
    
let retn result _ = { Result = List.singleton result; Next = None }

let empty = fun _ -> { Result = []; Next = None }

let rec map f coordination =
    coordination
    >> fun { Result = x; Next = next } ->
        { Result = List.map f x; Next = Option.map (map f) next }
        
let rec private switch' currentCoordination higherOrderCoordination event =
    match (higherOrderCoordination event), currentCoordination with
    | { Result = []; Next = None }, None ->
        { Result = []; Next = None }
    | { Result = []; Next = None }, Some currentCoordination ->
        currentCoordination event
    | { Result = []; Next = Some nextHigherOrderCoordination }, None ->
        { Result = []; Next = Some (switch' currentCoordination nextHigherOrderCoordination) }
    | { Result = []; Next = Some nextHigherOrderCoordination }, Some currentCoordination ->
        let { Result = result; Next = next } = currentCoordination event
        { Result = result; Next = Some (switch' next nextHigherOrderCoordination) }
    | { Result = coordinations; Next = None }, _ ->
        (List.last coordinations) event
    | { Result = workflows; Next = Some nextHigherOrderCoordination }, _ ->
        let { Result = result; Next = next } = (List.last workflows) event
        { Result = result; Next = Some (switch' next nextHigherOrderCoordination) }
        
let rec private flatten flattenPair higherOrderCoordination event =
    higherOrderCoordination event
    |> (function
        | { Result = coordinations; Next = None } ->
          List.map (fun c -> c event) coordinations 
          |> List.fold (fun acc res -> flattenPair (acc, res)) { Result = []; Next = None }
        | { Result = workflows; Next = Some nextHigherOrderCoordination } ->
          List.map (fun c -> c event) workflows
          |> List.fold (fun acc res -> flattenPair (acc, res)) { Result = []; Next = None }
          |> (function
            | { Result = result; Next = Some next } ->
                { Result = result
                  Next = Some (fun e -> flattenPair (next e, (flatten flattenPair nextHigherOrderCoordination e))) } 
            | { Result = result; Next = None } ->
                { Result = result
                  Next = Some (flatten flattenPair nextHigherOrderCoordination) }))
    
let rec private mergePair = function
    | { Result = result1; Next = Some nextFn1 }, { Result = result2; Next = Some nextFn2 } ->
            { Result = List.append result1 result2
              Next = Some (fun event -> mergePair (nextFn1 event, nextFn2 event)) } 
    | { Result = result1; Next = Some nextFn }, { Result = result2;  Next = None }
    | { Result = result1;  Next = None }, { Result = result2;  Next = Some nextFn } ->
        { Result = List.append result1 result2; Next = Some nextFn }
    | { Result = result1; Next = None }, { Result = result2; Next = None } ->
      { Result = List.append result1 result2; Next = None }         

let rec private concatPair = function
    | { Result = result1; Next = None }, { Result = result2; Next = next } ->
        { Result = List.append result1 result2; Next = next }
    | { Result = result; Next = Some next }, ({ Next = None; } as result2) ->
        { Result = result; Next = Some (fun e -> concatPair (next e, result2)) }
    | { Result = result1; Next = Some next1 }, { Result = result2; Next = Some next2 } ->
        let next2'' e =
            let { Result = result''; Next = next2' } = next2 e
            { Result = List.append result2 result''; Next = next2' }
        { Result = result1; Next = Some (fun e -> concatPair ((next1 e), next2'' e)) }
            
let rec private exhaustPair = function
    | { Result = result1; Next = None }, { Result = result2; Next = next2 }->
        { Result = List.append result1 result2; Next = next2 }
    | { Result = result; Next = Some next }, { Next = None; } ->
        { Result = result; Next = Some next }
    | { Result = result; Next = Some next1 }, { Next = Some next2; } ->
        { Result = result; Next = Some (fun e -> exhaustPair (next1 e, next2 e)) }
        
let merge orc' = flatten mergePair orc'
let switch coordination' = switch' None coordination'
let concat orc' = flatten concatPair orc'
let exhaust orc' = flatten exhaustPair orc'
let mergeMap f = map f >> merge
let switchMap f = map f >> switch
let concatMap f = map f >> concat
let exhaustMap f = map f >> exhaust

let rec filter predicate coordination =
    coordination
    >> (fun { Result = result; Next = next } ->
        let results' = List.filter predicate result
        { Result = results'; Next = Option.map (filter predicate) next })
    
let rec choose chooser coordination =
    coordination
    >> (fun { Result = result; Next = next } ->
        let results' = List.choose chooser result
        { Result = results'; Next = Option.map (choose chooser) next })

let rec take count coordination =
    coordination
    >> fun { Result = result;  Next = next } ->
            let results' = result |> List.truncate count
            let remainingCount = count - results'.Length
            let next' = if remainingCount > 0
                        then Option.map (take remainingCount) next
                        else None
            { Result = results';  Next = next' }
          
let rec event chooser e =
    let result =
        chooser e
        |> Option.toList
    { Result = result; Next = Some (event chooser) }
        
let rec zip coordination1 coordination2 e =
    match coordination1 e, coordination2 e with
    | { Result = result1; Next = next1 }, { Result = result2; Next = next2 } when result1.Length > result2.Length ->
        let taken, remaining = List.splitAt result2.Length result1
        let getNext = function
            | Some next1, Some next2 ->
                let next1 e =
                    let { Result = nextResult; Next = newNext } = next1 e
                    { Result = remaining @ nextResult; Next = newNext }
                Some (zip next1 next2)
            | None, Some next2 ->
                let next1 _ = { Result = remaining; Next = None }
                Some (zip next1 next2)
            | _ -> None
        { Result = List.zip taken result2; Next = getNext (next1, next2) }
    | { Result = result1; Next = next1 }, { Result = result2; Next = next2 } when result1.Length < result2.Length ->
        let taken, remaining = List.splitAt result1.Length result2
        let getNext = function
            | Some next1, Some next2 ->
                let next2 e =
                    let { Result = nextResult; Next = newNext } = next2 e
                    { Result = remaining @ nextResult; Next = newNext }
                Some (zip next1 next2)
            | Some next1, None ->
                let next2 _ = { Result = remaining; Next = None }
                Some (zip next1 next2)
            | _ -> None
        { Result = List.zip result1 taken; Next = getNext (next1, next2) }
    | { Result = result1; Next = next1 }, { Result = result2; Next = next2 } ->
        let getNext = function
            | Some next1, Some next2 -> Some (zip next1 next2)
            | _ -> None
        { Result = List.zip result1 result2; Next = getNext (next1, next2) }

let distinctUntilChanged coordination =
    let rec distinctUntilChanged' currentValue coordination =
        coordination >> fun result ->
        match result, currentValue with
        | { Result = result; Next = None }, None ->
            { Result = result |> Seq.distinct |> Seq.toList; Next = None }
        |  { Result = result; Next = Some next }, None ->
            let distinctResults = result |> Seq.distinct |> Seq.toList
            let last = distinctResults |> List.tryLast
            { Result = distinctResults; Next = Some (distinctUntilChanged' last next) }
        | { Result = result; Next = None }, Some currentValue ->
            { Result = result |> Seq.distinct |> Seq.skipWhile (fun x -> x = currentValue) |> Seq.toList
              Next = None }
        | { Result = result; Next = Some next }, Some currentValue ->
            let distinctResults = result |> Seq.distinct |> Seq.skipWhile (fun x -> x = currentValue) |> Seq.toList
            let last = distinctResults |> List.tryLast |> Option.defaultValue currentValue
            { Result = result |> Seq.distinct |> Seq.skipWhile (fun x -> x = currentValue) |> Seq.toList
              Next = Some (distinctUntilChanged' (Some last) next) }
    distinctUntilChanged' None coordination
    
let rec takeWhile condition coordination =
    coordination
    >> fun { Result = result;  Next = next } ->
        let results' = result |> List.takeWhile condition
        let difference = result.Length - results'.Length
        let next' = if difference = 0
                    then Option.map (takeWhile condition) next
                    else None
        { Result = results';  Next = next' }
            
let rec appendPair coordination1 coordination2 e =
    match coordination1 e with
    | { Result = result; Next = Some next } ->
        { Result = result; Next = Some (appendPair next coordination2) }
    | { Result = result; Next = None } ->
        let { Result = result2; Next = next2 } = coordination2 e
        { Result = result @ result2; Next = next2 }

let scan f seed coordination =
    let rec scan' f seed coordination =
        coordination
        >> fun { Result = result; Next = next } ->
            // Need to skip the first element because it is the seed
            let result' = result |> List.scan f seed |> List.skip 1
            // Need to get the last element of the result to use as the seed for the next coordination
            let nextSeed = result' |> List.tryLast |> Option.defaultValue seed
            { Result = result'; Next = Option.map (scan' f nextSeed) next }
    coordination
    >> fun { Result = result; Next = next } ->
        // In order to keep parity with List.scan, we need to include the seed in the result so don't skip first element
        // for first step in coordination
        let result' = result |> List.scan f seed
        // Need to get the last element of the result to use as the seed for the next coordination
        let nextSeed = result' |> List.tryLast |> Option.defaultValue seed
        { Result = result'; Next = Option.map (scan' f nextSeed) next }

let rec skip count coordination =
    coordination
    >> fun { Result = result;  Next = next } ->
        let remainingCount = count - result.Length
        let next' = if remainingCount > 0
                    then Option.map (skip remainingCount) next
                    else next

        let result' = if remainingCount >= 0
                      then []
                      else result |> List.skip count

        { Result = result';  Next = next' }

let rec compose (coordination2: Coordination<'b, 'c>) (coordination1: Coordination<'a, 'b>): Coordination<'a, 'c> =
    let accumulator { Result = accumulated; Next = next } result =
        let { Result = result2; Next = next2 } = 
            next 
            |> Option.map (fun next -> next result) 
            |> Option.defaultValue { Result = []; Next = None }
        { Result = accumulated @ result2; Next = next2 }

    coordination1
    >> fun { Result = result; Next = next } ->
        let { Result = result2; Next = next2 } = List.fold accumulator { Result = []; Next = Some coordination2 } result
        { Result = result2; 
          Next = match next, next2 with
                 | Some next, Some next2 -> Some (compose next2 next)
                 | _ -> None }

let rec collect f coordination =
    coordination
    >> fun { Result = result; Next = next } ->
        let result' = result |> List.collect f
        { Result = result'; Next = next |> Option.map (collect f) }

let rec combine orc1 orc2 event =
    let { Result = r1; Next = n1 } = orc1 event
    let { Result = r2; Next = n2 } = orc2 event
    { Result = r1 @ r2; 
      Next = match n1, n2 with
             | None, None -> None
             | Some n1, None -> Some n1 
             | None, Some n2 -> Some n2
             | Some n1, Some n2 -> Some (combine n1 n2) }

let applyRecursively getEventFromResults coordination =
    coordination
    >> (fun { Result = result; Next = next } ->
        let event = getEventFromResults result
        match event, next with
            | Some event, Some next -> 
                next event
            | _ -> 
                { Result = result; Next = next })

type CoordinationBuilder() =
    member _.Bind(m, f) =
        m |> take 1 |> switchMap f
        
    member _.Return(m) =
        retn m
        
    member _.ReturnFrom(m) =
        m
        
    member _.Yield(x) =
        retn x
        
    member _.YieldFrom(x) =
        x
        
    member _.Combine(a,b) =
        appendPair a b
        
    member _.Delay(f) =
        f()
        
    member _.MergeSources(coordination1, coordination2) =
        zip (coordination1 |> take 1) (coordination2 |> take 1)

    member __.Zero() =
        retn ()

let coordination = CoordinationBuilder()