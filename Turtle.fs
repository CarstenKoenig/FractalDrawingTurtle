open System
open System.Drawing
open System.Windows.Forms

module Turtle =

    type Distance = float32
    type Angle = float32

    type Cmd = 
        private
        | Jump of float32 * float32
        | Move of Distance
        | Turn of Angle
        | Push
        | Pop

    let private factor =
        float32 (2.0 * Math.PI / 360.0)

    let jump = Jump

    let move = Move

    let turnGrad (grad : float32) =
        Turn (grad * factor)

    let push = Push

    let pop = Pop        


    [<Struct>]
    type State =
        {
            x : float32
            y : float32
            angle : float32
            dirX : float32
            dirY : float32
        }

    let initState =
        {
            x = 0.0f
            y = 0.0f
            angle = 0.0f
            dirX = 1.0f
            dirY = 0.0f
        }

    type Params =
        {
            line : (float32 * float32) -> (float32 * float32) -> unit
        }    

    let createParams (gr : Graphics) (pen : Pen) width height =
        let factX = width / 100.0f
        let w2 = width / 2.0f
        let factY = height / 100.0f
        let h2 = height / 2.0f
        let viewX = fun x -> x * factX + w2
        let viewY = fun y -> y * factY + h2
        {
            line = fun (x0,y0) (x1,y1) ->
                gr.DrawLine (pen, viewX x0, viewY y0, viewX x1, viewY y1)
        }

    let execute (tParams : Params) (cmds : Cmd list) =
        let rec iter (stack : State list) (state : State) =
            function
            | [] -> ()
            | cmd :: cmds ->
                match cmd with
                | Push -> iter (state :: stack) state cmds
                | Pop  -> 
                    match stack with
                    | [] -> iter [] state cmds
                    | state' :: stack' -> iter stack' state' cmds
                | Jump (x,y) ->
                    let state' = { state with x = x; y = y }
                    iter stack state' cmds
                | Move d ->
                    let state' = { state with x = state.x + d * state.dirX; y = state.y + d * state.dirY }
                    tParams.line (state.x, state.y) (state'.x, state'.y)
                    iter stack state' cmds
                | Turn angle ->
                    let angle' = state.angle + angle
                    let state' = { state with dirX = cos angle'; dirY = - sin angle'; angle = angle' }
                    iter stack state' cmds
        iter [] initState cmds

let nGon n =
    let angle = 360.0f / float32 n
    List.replicate n [ Turtle.move 20.0f; Turtle.turnGrad angle]
    |> List.concat


module LSystem =

    type LSys =
        {
            start : string
            rules : Map<char, string>
        }

    let startWith start =
        { 
            start = start
            rules = Map.empty
        }        


    let addRule sym repl (sys : LSys) =
        let rules' = Map.add sym repl sys.rules
        { sys with rules = rules' }

    let private applyRule (sys : LSys) (c : char) =
        match sys.rules.TryFind c with
        | Some s -> s
        | None   -> String [|c|]        

    let rec private expand (sys : LSys) (depth : int) (state : string) : string =
        if depth <= 0 then state else
        state
        |> String.collect (applyRule sys)
        |> expand sys (depth-1)

    let execute (interpret : char -> Turtle.Cmd option) (depth : int) (sys : LSys) =
        expand sys depth sys.start
        |> Seq.choose interpret
        |> Seq.toList

let kochKurve n =
    let d = 100.0f / pown 3.0f n
    let interp =
        function
        | 'F' -> Some <| Turtle.move d
        | '+' -> Some <| Turtle.turnGrad 90.0f
        | '-' -> Some <| Turtle.turnGrad -90.0f
        | _   -> None
    LSystem.startWith "F"
    |> LSystem.addRule 'F' "F+F-F-F+F"
    |> LSystem.execute interp n
    |> fun cmds -> Turtle.jump (-50.0f, 25.0f) :: cmds
        

let snowFlake n =
    let d = 50.0f / pown 3.0f n
    let interp =
        function
        | 'F' -> Some <| Turtle.move d
        | '+' -> Some <| Turtle.turnGrad 60.0f
        | '-' -> Some <| Turtle.turnGrad -60.0f
        | _   -> None
    LSystem.startWith "Y"
    |> LSystem.addRule 'F' "F+F--F+F"
    |> LSystem.addRule 'Y' "F+F+F+F+F+F"
    |> LSystem.execute interp (n+1)
    |> fun cmds -> Turtle.jump (-25.0f, 42.5f) :: cmds


let sierpinski n =
    let d = 100.0f / pown 4.0f n
    let interp =
        function
        | 'A' -> Some <| Turtle.move d
        | 'B' -> Some <| Turtle.move d
        | '+' -> Some <| Turtle.turnGrad +60.0f
        | '-' -> Some <| Turtle.turnGrad -60.0f
        | _   -> None
    LSystem.startWith "A"
    |> LSystem.addRule 'A' "B-A-B"
    |> LSystem.addRule 'B' "A+B+A"
    |> LSystem.execute interp (2*n)
    |> fun cmds -> Turtle.jump (-50.0f, 49.0f) :: cmds


let plant n =
    let d = 100.0f / pown 2.3f n
    let interp =
        function
        | 'F' -> Some <| Turtle.move d
        | '+' -> Some <| Turtle.turnGrad -25.0f
        | '-' -> Some <| Turtle.turnGrad +25.0f
        | '[' -> Some <| Turtle.push
        | ']' -> Some <| Turtle.pop
        | _   -> None
    LSystem.startWith "--X"
    |> LSystem.addRule 'X' "F[-X][X]F[-X]+FX"
    |> LSystem.addRule 'F' "FF"
    |> LSystem.execute interp n
    |> fun cmds -> Turtle.jump (-50.0f, 50.0f) :: cmds


module Program =

    let mutable private turtleCommands : Turtle.Cmd list = []

    let private appForm =
        let pen = new Pen(brush = Brushes.Black, width = 1.0f)

        let form = new Form(Width = 800, Height = 800, Text = "Turtle Fun")

        let onPaint (e : PaintEventArgs) =
            e.Graphics.Clear Color.White
            e.Graphics.InterpolationMode <- Drawing2D.InterpolationMode.High
            e.Graphics.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality

            let tPs = Turtle.createParams e.Graphics pen (float32 form.ClientSize.Width) (float32 form.ClientSize.Height)
            Turtle.execute tPs turtleCommands

        form.Paint.Add onPaint
        form.Resize.Add (fun e -> form.Invalidate true)
        form

    let setTurtleProgramm prg =
        turtleCommands <- prg
        appForm.Invalidate true    


    [<EntryPoint>]
    [<STAThread>]
    let main argv = 
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault false
        setTurtleProgramm <| snowFlake 5

        Application.Run appForm
        0