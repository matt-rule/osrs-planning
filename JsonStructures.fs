module JsonStructures

open Newtonsoft.Json

[<Measure>] type lv
[<Measure>] type xp

type PlayerMetricLv(name, score) =
    member this.Name : string = name
    member this.Score : int<lv> = score

type PlayerMetricExp(name, score) =
    member this.Name : string = name
    member this.Score : int<xp> = score

type DiarySection(regionName, tier, requirements) =
    member this.RegionName : string = regionName
    member this.Tier : string = tier
    member this.Requirements : PlayerMetricLv list = requirements

let LoadDiariesFromJson jsonString =
    JsonConvert.DeserializeObject<DiarySection list>(jsonString)
