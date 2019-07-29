module InlineEditTodo exposing (Msg(..))

import Project exposing (Project)
import ProjectId exposing (ProjectId)
import Todo exposing (Todo, TodoId)


type Msg
    = SetTitle String
    | SetProjectId ProjectId
    | Save
    | Cancel
