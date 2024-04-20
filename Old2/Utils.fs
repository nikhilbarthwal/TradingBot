namespace CryptoBot




module Utils =

    let Wait (timeout:int) = System.Threading.Thread.Sleep(timeout * 1000)
    let Normalize (f: float) = Math.Round(f, 3)
    let CurrentTime(): time = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

    let inline Loop (max: int) (delay: int) (f: int -> ^T option): ^T option =
        assert (max > 1)
        let rec loop (i:int): ^T option =
           match (f i) with
           | Some z -> Some(z)
           | None -> if i = max then None else (Wait delay ; loop (i+1))
        in loop 1

    let inline Repeat (count:int) (delay: int) (cutoff: DateTime) (f: int -> bool option): bool =
        let rec loop (i:int): bool =
            if  (DateTime.Now >= cutoff)  || (i >= count) then false else
                Wait delay
                match (f i) with Some(b) -> b | None -> loop (i+1)
        in loop 1

    let inline Collect (init: ^T, cond: ^T -> bool, incr: ^T -> ^T)
        (func: ^T -> ^U): ^U list =
            let rec loop (l: ^U list) (x:^T): ^U list =
                if (cond x) then (loop ((func x)::l) (incr x)) else (List.rev l)
            in (loop [] init)

    let inline CreateMap (l: ^U list) (f: ^U -> ^V) =
        l |> List.map (fun x -> x, f x) |> Map.ofList

    let inline NewArray (size:int) (z: ^T) = [| for _ in 1 .. size -> z |]

    let inline AllTrue (f: ^T -> bool) (z:^T list): bool =
        let rec loop (l: ^T list) =
            match l with h::t -> (if (f h) then (loop t) else false) | [] -> true
        in loop z

    let Percent (a: float, b: float): float = 100.0 * (a-b) / b

    let WaitUntil (time: DateTime): unit =
        Log.Info(0, $"Waiting till {time.ToString()}")
        let rec wait (): unit = if DateTime.Now < time then (Wait(60) ; wait())
        wait()
