# Elm-Procedure

This package allows you to chain together commands, subscriptions, and tasks to describe a procedure. 

Create a `Procedure` and then produce a `Cmd` that can be executed via `Procedure.run` or `Procedure.try`.

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
