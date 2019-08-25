# Elm-Procedure

This package provides an abstraction that allows you to orchestrate commands, subscriptions, and tasks.

Create a `Procedure` and then use `Procedure.run` or `Procedure.try` to produce a `Cmd` that can be executed.

## Examples

Here are some common use cases for a `Procedure`.

#### Transform the result of a command

The following procedure will make an Http GET request, and then transform the result. If
the result is `Ok` then a function will be applied to the value; if the result is `Err` then
an alternative procedure providing a default value will be executed. Once the procedure is
complete, the value will be tagged with `StringTagger` and passed to the `update` function. 

```
Procedure.fetchResult (\tagger ->
  Http.get
    { url = "http://fun.com/fun.html"
    , expect = Http.expectString tagger
    }
  )
  |> Procedure.map (\words ->
    "Fun stuff: " ++ words
  )
  |> Procedure.catch (\error ->
    Procedure.provide "No Response"
  )
  |> Procedure.run ProcedureTagger StringTagger
```

#### Transform the value of a subscription

The following procedure will listen for messages to `mySubscription`, filter incoming
values by applying a function, map selected values to a String, tag those values with
`StringTagger` and send them to the `update` function.

```
Channel.join mySubscription
  |> Channel.filter (\_ data -> modBy 2 data == 0)
  |> Channel.accept
  |> Procedure.map String.fromInt
  |> Procedure.run ProcedureTagger StringTagger
```

#### Send a message through a port and wait for a response

The following procedure will send a command via `myPortCommand` built using the unique identifier for
this procedure, `key`. Then the procedure will wait for incoming messages
via `myPortSubscription` and filter those messages, selecting only those that match the unique
identifier for this procedure. Once one message has been accepted, the value will be tagged with
`DataTagger` and passed to the `update` function. Finally, the subscription will be terminated. 

```
Channel.open (\key -> myPortCommand key)
  |> Channel.connect myPortSubscription
  |> Channel.filter (\key data -> data.key == key)
  |> Channel.acceptOne
  |> Procedure.run ProcedureTagger DataTagger
```

## Usage

Follow these steps so that your program can process `Cmd` values produced with `Procedure.run` or `Procedure.try`.

1. Add a custom `Msg` type that will tag `Procedure.Program.Msg` values:
```
type Msg
  = ProcedureMsg (Procedure.Program.Msg Msg)
```
2. Add the `Procedure.Program.Model` to your program's model:
```
type alias Model =
  { procModel: Procedure.Program.Model Msg
  }
```
3. Add a case to your `update` function to handle `Procedure.Program.Msg` values:
```
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProcedureMsg procMsg ->
      Procedure.Program.update procMsg model.procModel
        |> Tuple.mapFirst (\updated -> { model | procModel = updated })
```
4. Initialize the `Procedure.Program.Model` when you initialize your program:
```
init : MyFlags -> ( Model, Cmd Msg )
init flags =
  ( { procModel = Procedure.Program.init }, Cmd.none )
```
