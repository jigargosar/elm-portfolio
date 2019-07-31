module ProjectDict exposing (ProjectDict)

import Dict exposing (Dict)
import Project exposing (Project)
import ProjectId exposing (ProjectId)


type alias ProjectDict =
    Dict ProjectId Project
