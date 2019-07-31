module ProjectDict exposing (ProjectDict, fromList)

import Dict exposing (Dict)
import Dict.Extra
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)


type alias ProjectDict =
    Dict ProjectId Project


fromList : ProjectList -> ProjectDict
fromList =
    Dict.Extra.fromListBy .id
