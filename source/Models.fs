module Models
open System

// Model shared between server and client
type Comment = {
    id: DateTime option
    author: string
    text: string
}

type Creature() =
    member val Name = "Creature" with get, set

type Terrain = | Cave | Mountain | Forest | Plain

type Encounter() =
    member val Terrain = Plain with get, set
    member val Creatures : Creature[] = [||] with get, set