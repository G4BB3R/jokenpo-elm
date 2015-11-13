import Html             exposing (..)
import Html.Attributes  exposing (..)
import Html.Events      exposing (..)
import Signal           exposing (..)
import StartApp.Simple  exposing (start)
import Random           exposing (generate, int, initialSeed)

type Win       = Draw   | Lose     | Won
type GameState = Menu   | Creditos | Play        | End Win
type Joken     = Pedra  | Papel    | Tesoura
type Action    = ActNop | ActMenu  | ActCreditos | ActStart | ActPlay Joken

type alias Model = { state     : GameState
                   , jogada    : Joken
                   , jogada_pc : Joken
                   , seed      : Random.Seed
                   }

doPieceWin : Joken -> Joken -> Win
doPieceWin p1 p2 =
    case p1 of
        Pedra -> case p2 of
                     Pedra   -> Draw
                     Papel   -> Lose
                     Tesoura -> Won
        Papel -> case p2 of
                     Pedra   -> Won
                     Papel   -> Draw
                     Tesoura -> Lose
        Tesoura -> case p2 of
                     Pedra   -> Lose
                     Papel   -> Won
                     Tesoura -> Draw

getPieceById : Int -> Joken
getPieceById id =
    case id of
        1 -> Pedra
        2 -> Papel
        3 -> Tesoura

getBtnAttrs : Joken -> List Attribute
getBtnAttrs joken =
    let url = case joken of
                  Pedra   -> "http://1.bp.blogspot.com/-I_CrWXZrXK8/UbRyoN8kHAI/AAAAAAABSas/zjAgLoyHzW0/s1600/pedra.jpg"
                  Papel   -> "http://www.print-class.com/imagenes/contenido/originales/papel-sublimacion.jpg"
                  Tesoura -> "https://image.freepik.com/fotos-gratis/tesoura_2926575.jpg"
    in  [ width 200, height 200, src url]

model : Model
model = { state     = Menu
        , jogada    = Pedra
        , jogada_pc = Pedra
        , seed      = initialSeed 31415
        }

view_menu : Address Action -> Model -> Html
view_menu address model =
    div []
        [ text "[TODO] Aqui vai uma pequena explicação de como jogar"
        , br [] []
        , br [] []
        , button [ onClick address ActStart    ] [ text "Jogar" ]
        , button [ onClick address ActCreditos ] [ text "Créditos" ]
        ]

view_play : Address Action -> Model -> Html
view_play address model =
    div [] [ b [] [text "Faça a sua jogada!"]
           , br [] []
           , div [] (List.map (\j -> img (onClick address (ActPlay j) :: getBtnAttrs j) [] ) [Pedra, Papel, Tesoura])
           , br [] []
           , button [ onClick address ActMenu ] [ text "Menu" ]
           ]

view_end : Address Action -> Model -> Win -> Html
view_end address model result =
    let
        texto =
            case result of
                Won  -> "Você ganhou!"
                Lose -> "Você perdeu!"
                Draw -> "Temos um empate!"
    in
        div [] [ b  [] [ text texto ]
               , br [] []
               , br [] []
               , div [ style [("display", "inline-block")] ]
                        [ text "Você:"
                        , br [] []
                        , img (getBtnAttrs model.jogada) [] ]
               , div [ style [("display", "inline-block")] ]
                        [ text "Oponente:"
                        , br [] []
                        , img (getBtnAttrs model.jogada_pc) [] ]
               , br [] []
               , br [] []
               , button [ onClick address ActMenu ] [ text "Back to menu" ]
               , button [ onClick address ActStart] [ text "Play again" ]
               ]

view_creditos : Address Action -> Model -> Html
view_creditos address model =
    div []
        [ text "Feito por mim, Gabriel Torrecillas Sartori"
        , br [] []
        , text "Apenas um jogo simples pra praticar a linguagem de programação "
        , a [ href "http://elm-lang.org", attribute "target" "_blank" ] [ text "Elm" ]
        , text ","
        , br [] []
        , text "que compila para Html/CSS/Js :)"
        , br [] []
        , br [] []
        , img [ width 250 , src "https://i.imgur.com/Sex1E8m.jpg"] []
        , br [] []
        , br [] []
        , button [ onClick address ActMenu ] [ text "Back" ]
        ]

view : Address Action -> Model -> Html
view address model = let
    game_div =
        case model.state of
                Menu       -> view_menu     address model
                Creditos   -> view_creditos address model
                Play       -> view_play     address model
                End result -> view_end      address model result
    in
    div [ style [("text-align", "center")]]
        [ h1 [] [ text "JO KEN PO" ]
        , game_div
        ]

update : Action -> Model -> Model
update action model =
    case action of
        ActNop      -> model
        ActMenu     -> { model | state <- Menu }
        ActCreditos -> { model | state <- Creditos }
        ActStart    -> { model | state <- Play }
        ActPlay minha_jogada ->
          let
              (jogada_raw, seed') = generate (int 1 3) model.seed
              jogada_pc = getPieceById jogada_raw
              vitoria   = doPieceWin minha_jogada jogada_pc
          in
              { model | state     <- End vitoria
                      , jogada    <- minha_jogada
                      , jogada_pc <- jogada_pc
                      , seed      <- seed' }

main : Signal Html
main =
    StartApp.Simple.start { model = model, view = view, update = update }
