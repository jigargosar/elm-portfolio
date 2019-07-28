module TodoDict exposing
    ( TodoDict
    , completedForProjectList
    , completedList
    , markCompleted
    , markPending
    , pendingList
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


pendingList : TodoDict -> List Todo
pendingList =
    filterSort Todo.Pending [ Todo.ByIdx ]


completedList : TodoDict -> List Todo
completedList =
    filterSort Todo.Completed [ Todo.ByRecentlyModified ]


pendingWithProjectId pid model =
    pendingList model
        |> Todo.filter (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))


completedForProjectList pid model =
    completedList model
        |> Todo.filter (Todo.AndFilter Todo.Completed (Todo.BelongsToProject pid))


type alias Millis =
    Int


pendingWithId : TodoId -> TodoDict -> Maybe Todo
pendingWithId todoId =
    Dict.get todoId
        >> Maybe.andThen (Todo.filterSingle Todo.Pending)


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
    pendingList todos
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, t ) ->
                if t.sortIdx == i then
                    Nothing

                else
                    Just (Todo.setSortIdx i t |> Todo.setModifiedAt now)
            )
        |> List.foldl (\t -> Dict.insert t.id t) todos
