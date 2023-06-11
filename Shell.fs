namespace GameOfLifeMvu

module Shell =

    open Elmish
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Hosts
    open Avalonia.FuncUI.Elmish
    open Avalonia.Controls.Primitives
    open Avalonia.Controls.Shapes

    type State = {
        Rows: int
        Columns: int
        }


    type Msg =
        | NextGeneration

    let init () =
        {
            Rows = 100
            Columns = 100
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | NextGeneration -> state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        StackPanel.create [
            StackPanel.children [
                UniformGrid.create [

                    UniformGrid.columns state.Columns
                    UniformGrid.rows state.Rows

                    UniformGrid.width (state.Columns * 5 |> float)
                    UniformGrid.height (state.Rows * 5 |> float)

                    UniformGrid.children [
                        for row in 0 .. state.Rows - 1 do
                            for col in 0 .. state.Columns - 1 do
                                Rectangle.create [
                                    Rectangle.width 5
                                    Rectangle.height 5
                                    Rectangle.fill "pink"
                                    ]
                        ]
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
            base.Title <- "Full App"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

            Elmish.Program.mkProgram init update view
            |> Program.withHost this
            |> Program.run
