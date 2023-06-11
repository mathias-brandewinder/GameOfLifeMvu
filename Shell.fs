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

    type State = {
        Rows: int
        Columns: int
        }

    type Msg =
        | NextGeneration

    let init () =
        {
            Rows = 10
            Columns = 10
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<_> =
        match msg with
        | NextGeneration -> state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit) =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [
                    TextBlock.text "TODO"
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
