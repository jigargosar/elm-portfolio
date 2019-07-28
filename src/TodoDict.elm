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


markCompleted todoId now model =
    pending model
        |> List.filter (\t -> Todo.isPending t && Todo.idEq todoId t)
        |> List.head
        |> Maybe.map
            (Todo.markCompleted
                >> Todo.setModifiedAt now
                >> (\t -> Dict.insert t.id t model)
                >> updateSortIdx now
            )


ter : Bool -> a -> a -> a
ter bool a b =
    if bool then
        a

    else
        b


maybeFilter pred =
    Maybe.andThen (\val -> ter (pred val) (Just val) Nothing)


markPending todoId now model =
    model
        |> Dict.get todoId
        |> maybeFilter Todo.isCompleted
        |> Maybe.map
            (Todo.markPending
                >> Todo.setSortIdx Basics.Extra.maxSafeInteger
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
