namespace GameOfLifeMvu

module Shell =

    open System
    open Elmish
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Hosts
    open Avalonia.FuncUI.Elmish
    open Avalonia.Controls.Primitives
    open Avalonia.Controls.Shapes
    open Avalonia.Layout

    type Cell =
        | Dead
        | Alive of age: int

    let neighbors =
        [
            -1, -1
            -1, 0
            -1, 1
            0, -1
            0, 1
            1, -1
            1, 0
            1, 1
        ]

    let nextGeneration (cells: Cell [,]) (row: int, col: int) =
        let isAlive (row, col) =
            if row < 0 || col < 0 || row >= cells.GetLength(0) || col >= cells.GetLength(1)
            then false
            else cells.[row, col] <> Dead
        let liveNeighbors =
            neighbors
            |> Seq.sumBy (fun (dx, dy) ->
                if isAlive (row + dx, col + dy)
                then 1
                else 0
                )
        match cells.[row, col] with
        | Dead ->
            if liveNeighbors = 3
            then Alive 0
            else Dead
        | Alive age ->
            if liveNeighbors = 2 || liveNeighbors = 3
            then Alive (age + 1)
            else Dead

    type State = {
        Generation: int
        Running: bool
        Cells: Cell [,]
        }
        with
        member this.Rows = this.Cells.GetLength(0)
        member this.Columns = this.Cells.GetLength(1)

    type Msg =
        | StartStop
        | NextGeneration
        | Restart

    type Configuration = {
        Rows: int
        Columns: int
        CellSize: int
        RefreshIntervalMilliseconds: int
        }

    let config = {
        Rows = 50
        Columns = 50
        CellSize = 5
        RefreshIntervalMilliseconds = 500
        }

    let init () =

        let rng = Random()
        let cells =
            Array2D.init config.Rows config.Columns (fun row col ->
                if rng.NextDouble() <= 0.8
                then Dead
                else Alive 0
                )
        { Cells = cells; Running = false; Generation = 0 }, Cmd.none

    let waitAndUpdate =
        Cmd.OfAsync.perform
            (fun () -> async { do! Async.Sleep config.RefreshIntervalMilliseconds })
            ()
            (fun () -> NextGeneration)

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | Restart -> init ()
        | StartStop ->
            let cmd =
                if state.Running
                then Cmd.none
                else Cmd.ofMsg NextGeneration
            { state with Running = not state.Running }, cmd
        | NextGeneration ->
            if state.Running
            then
                let updated =
                    state.Cells
                    |> Array2D.mapi (fun row col _ ->
                        nextGeneration state.Cells (row, col)
                        )
                { state with
                    Cells = updated;
                    Generation = state.Generation + 1
                },
                waitAndUpdate
            else state, Cmd.none

    let deadCell =
        Rectangle.create [
            Rectangle.width config.CellSize
            Rectangle.height config.CellSize
            Rectangle.fill "Black"
            ]

    let youngCell =
        Rectangle.create [
            Rectangle.width config.CellSize
            Rectangle.height config.CellSize
            Rectangle.fill "HotPink"
            ]

    let oldCell =
        Rectangle.create [
            Rectangle.width config.CellSize
            Rectangle.height config.CellSize
            Rectangle.fill "Purple"
            ]

    let view (state: State) (dispatch: Msg -> unit) =
        StackPanel.create [

            StackPanel.orientation Orientation.Vertical
            StackPanel.horizontalAlignment HorizontalAlignment.Center

            StackPanel.children [

                UniformGrid.create [

                    UniformGrid.margin 5

                    UniformGrid.columns state.Columns
                    UniformGrid.rows state.Rows

                    UniformGrid.width (state.Columns * config.CellSize |> float)
                    UniformGrid.height (state.Rows * config.CellSize |> float)

                    UniformGrid.children [
                        for row in 0 .. state.Rows - 1 do
                            for col in 0 .. state.Columns - 1 do
                                match state.Cells.[row, col] with
                                | Dead -> deadCell
                                | Alive age ->
                                    if age < 2
                                    then youngCell
                                    else oldCell
                        ]
                    ]

                StackPanel.create [

                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                    StackPanel.margin 5

                    StackPanel.children [
                        Button.create [
                            Button.content (if state.Running then "Pause" else "Start")
                            Button.onClick (fun _ -> dispatch StartStop)
                            ]

                        Button.create [
                            Button.content "New"
                            Button.onClick (fun _ -> dispatch Restart)
                            ]
                        ]
                    ]

                TextBlock.create [
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    TextBlock.text $"Generation {state.Generation}"
                    ]
                ]
            ]

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit HostWindow()
        do
            base.Title <- "Game of Life"
            base.Width <- 500.0
            base.Height <- 600.0
            base.MinWidth <- 500.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram init update view
            |> Program.withHost this
            |> Program.run
