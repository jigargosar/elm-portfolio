module TodoDict exposing
    ( TodoDict
    , completed
    , completedWithProjectId
    , markCompleted
    , markPending
    , pending
    , pendingWithId
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


filterSort f s model =
    model |> Dict.values |> Todo.filterSort f s


pending : TodoDict -> List Todo
pending =
    filterSort Todo.Pending [ Todo.ByIdx ]


completed : TodoDict -> List Todo
completed =
    filterSort Todo.Completed [ Todo.ByRecentlyModified ]


pendingWithProjectId pid model =
    pending model
        |> Todo.filter (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))


completedWithProjectId pid model =
    completed model
        |> Todo.filter (Todo.AndFilter Todo.Completed (Todo.BelongsToProject pid))


type alias Millis =
    Int


pendingWithId todoId =
    Dict.get todoId
        >> Maybe.andThen (List.singleton >> Todo.filter Todo.Pending >> List.head)


markCompleted todoId now model =
    model
        |> Dict.get todoId
        |> Maybe.andThen (Todo.setCompleted True)
        |> Maybe.map
            (Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updateSortIdx now
            )


markPending todoId now model =
    model
        |> Dict.get todoId
        |> Maybe.andThen (Todo.setCompleted False)
        |> Maybe.map
            (Todo.setSortIdx Basics.Extra.maxSafeInteger
                >> Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updateSortIdx now
            )


updateSortIdx : Millis -> TodoDict -> TodoDict
updateSortIdx now todos =
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
