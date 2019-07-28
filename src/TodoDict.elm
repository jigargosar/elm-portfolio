module TodoDict exposing
    ( TodoDict
    , completed
    , completedWithProjectId
    , markCompleted
    , markPending
    , pending
    , pendingWithProjectId
    )

import Basics.Extra
import Dict exposing (Dict)
import Todo exposing (Todo, TodoId)


type alias TodoDict =
    Dict TodoId Todo


filter : Todo.Filter -> TodoDict -> List Todo
filter f model =
    model
        |> Dict.values
        |> Todo.filter f


pending model =
    model
        |> filter Todo.Pending
        |> Todo.sortPending


completed model =
    model
        |> Dict.values
        |> Todo.filter Todo.Completed
        |> Todo.sortCompleted


pendingWithProjectId pid model =
    pending model
        |> List.filter (Todo.projectIdEq pid)


completedWithProjectId pid model =
    completed model
        |> List.filter (Todo.projectIdEq pid)


type alias Millis =
    Int


markCompleted todoId now model =
    pending model
        |> List.filter (\t -> Todo.isPending t && Todo.idEq todoId t)
        |> List.head
        |> Maybe.map
            (Todo.markCompleted
                >> Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> sortPendingTodos now
            )


markPending todoId now model =
    completed model
        |> List.filter (\t -> Todo.isCompleted t && Todo.idEq todoId t)
        |> List.head
        |> Maybe.map
            (Todo.markPending
                >> Todo.setSortIdx Basics.Extra.maxSafeInteger
                >> Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> sortPendingTodos now
            )


sortPendingTodos : Millis -> TodoDict -> TodoDict
sortPendingTodos now todos =
    pending todos
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, t ) ->
                if t.sortIdx == i then
                    Nothing

                else
                    Just (Todo.setSortIdx i t |> Todo.setModifiedAt now)
            )
        |> List.foldl (\t -> Dict.insert t.id t) todos
