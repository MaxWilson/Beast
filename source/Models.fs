module Models
open System

// Model shared between server and client
type MonsterId = string
type Lineup = MonsterId list
type LineupId = string
type MonsterStats = {
    Id: MonsterId
    Name: string
    HP: int
}
type Encounter = | Monsters of LineupId | ShortRest | LongRest
type Adventure = Encounter list
type Menagerie = Map<MonsterId, MonsterStats>
type Lineups = Map<LineupId, Lineup>