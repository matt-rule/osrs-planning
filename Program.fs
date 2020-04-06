open Newtonsoft.Json
open System
open System.IO

open JsonStructures
open System.Net
open HtmlAgilityPack
open System.Text.RegularExpressions

let loadDiariesFromFilename = "diaries-generated.json"

let allSkills = [
    "attack";"hitpoints";"mining";"strength";"agility";"smithing";"defence";"herblore";"fishing";"ranged";"thieving";"cooking";"prayer";"crafting";"firemaking";"magic";"fletching";"woodcutting";"runecrafting";"slayer";"farming";"construction";"hunter"
]

//Programming the Runescape EXP Formula
//http://tritecode.wikidot.com/article:2008:09:16:runescape-exp
//https://oldschool.runescape.wiki/w/Experience
let experienceFromLevel1ToX(x : int<lv>) : int<xp> =
    {1..(int x)-1}
    |> Seq.map double
    |> Seq.sumBy (fun x ->
        (x + 300.0 * Math.Pow(2.0, x / 7.0))
        |> Math.Floor
    )
    |> (fun x ->
        ((x / 4.0) |> Math.Floor |> int) * 1<xp>
    )

let experienceFromLevelsXToY x y =
    (experienceFromLevel1ToX y)
    - (experienceFromLevel1ToX x)

let experienceToReachSingleLevel level =
    (experienceFromLevel1ToX level)
    - (experienceFromLevel1ToX (level-1<lv>))

let testExperienceFunctions() =
    printfn "%A" ({1..30} |> Seq.map ((( * ) 1<lv>) >> experienceFromLevel1ToX) |> List.ofSeq)
    printfn "%A" ({1..30} |> Seq.map ((( * ) 1<lv>) >> experienceToReachSingleLevel) |> List.ofSeq)

let extraExperienceRequiredForMetric (playerProfile : PlayerMetricExp list) (metric : PlayerMetricLv) =
    match playerProfile |> Seq.tryFind (fun x -> x.Name = metric.Name) with
    | None -> experienceFromLevel1ToX(metric.Score)
    | Some currentMetricValue ->
        experienceFromLevel1ToX(metric.Score) - currentMetricValue.Score

let extraExperienceRequiredForSection (playerProfile : PlayerMetricExp list) (section : DiarySection) =
    section.Requirements
    |> Seq.where (fun x -> allSkills |> List.contains x.Name)
    |> Seq.map (
        (fun diaryRequirement ->
            PlayerMetricExp(
                diaryRequirement.Name,
                experienceFromLevel1ToX(diaryRequirement.Score)
            )
        )
        >> (fun diaryExpRequirement ->
            match (playerProfile |> Seq.tryFind (fun y -> y.Name = diaryExpRequirement.Name)) with
            | None -> diaryExpRequirement
            | Some matchingPlayerMetric ->
                PlayerMetricExp(
                    diaryExpRequirement.Name,
                    diaryExpRequirement.Score - matchingPlayerMetric.Score
                )
        )
    )
    |> Seq.sumBy (fun x -> x.Score)

let profileWithCompleteDiarySection (profile : PlayerMetricExp list) (section : DiarySection) =
    (
        profile
        |> List.map (fun x ->
            let diaryRequirement =
                section.Requirements
                |> List.tryFind (fun y ->
                    y.Name = x.Name
                )
            match diaryRequirement with
            | None -> x
            | Some req ->
                PlayerMetricExp(
                    req.Name,
                    experienceFromLevel1ToX(req.Score)
                )
        )
    )
    @ (
        (section.Requirements)
        |> List.where (fun requirement -> profile |> List.tryFind(fun y -> y.Name = requirement.Name) = None)
        |> List.map (fun x ->
            PlayerMetricExp(
                x.Name,
                experienceFromLevel1ToX(x.Score)
            )
        )
    )

let rec recSortDiaries
        (playerProfile : PlayerMetricExp list)
        (sorted : DiarySection list)
        (unsorted : DiarySection list) =
    if List.isEmpty unsorted
    then
        sorted
    else
        let sectionToAdd = unsorted |> List.minBy (extraExperienceRequiredForSection playerProfile)
        let nextSorted = sorted @ [sectionToAdd]
        let nextUnsorted =
            unsorted
            |> List.where (fun x ->
                x.RegionName <> sectionToAdd.RegionName
                || x.Tier <> sectionToAdd.Tier
            )
        recSortDiaries (profileWithCompleteDiarySection playerProfile sectionToAdd) nextSorted nextUnsorted

let sortDiaries playerProfile unsortedAchievements =
    recSortDiaries playerProfile [] unsortedAchievements

//https://oldschool.runescape.wiki/w/Achievement_Diary#Easy
let sortByExperienceRequired() =
    let playerProfile : PlayerMetricExp list = []

    File.ReadAllText(loadDiariesFromFilename)
    |> LoadDiariesFromJson
    |> sortDiaries playerProfile
    |> Seq.ofList
    |> Seq.iter (fun x ->
        printf "%s (%s) : %i total"
            x.RegionName
            x.Tier
            (extraExperienceRequiredForSection playerProfile x)

        x.Requirements
        |> List.iter(fun y ->
            printf "\n\t%i %s (%i xp)"
                y.Score
                y.Name
                (extraExperienceRequiredForMetric playerProfile y)
        )
        Console.WriteLine()
    )

let generateJson() =
    let areaFileString = File.ReadAllText("diary-webpages.json")
    let areaStrings : string list = JsonConvert.DeserializeObject<string list>(areaFileString)

    // easy, medium, hard, elite
    let tableIndexes = [
        (3,"easy");
        (7,"medium");
        (11,"hard");
        (15,"elite")
    ]

    let client : WebClient = new WebClient()

    let diariesObject =
        areaStrings
        |> Seq.collect (fun areaString ->
            let htmlString =
                client.DownloadString(
                    "https://oldschool.runescape.wiki/w/"
                    + areaString
                    + "_Diary"
                )

            let doc = HtmlDocument()
            doc.LoadHtml(htmlString)

            tableIndexes
            |> List.map(fun tableIndex ->
                let docNode = doc.DocumentNode
                let tdNodesMatchingXPath = docNode.SelectNodes("//table[" + string (fst tableIndex) + "]/tbody/tr/td")

                Console.WriteLine(tdNodesMatchingXPath |> Seq.length)
                let validNode =
                    tdNodesMatchingXPath
                    |> Seq.find (fun tdNode ->
                        Regex.Replace(tdNode.InnerHtml, "\\s+", "") <> ""
                    )
                //Console.WriteLine("|" + validNode.InnerHtml + "|")

                let liNodesMatchingXPath = validNode.SelectNodes("ul/li")

                // if isNull liNodesMatchingXPath then
                //     failwith ("null error " + ":" + areaString + ":" + string (fst tableIndex))

                DiarySection(
                    areaString,
                    snd tableIndex,
                    liNodesMatchingXPath
                    |> Seq.where (fun node -> not (isNull (node.SelectNodes("a[1]"))))
                    |> Seq.map (fun node ->
                        PlayerMetricLv(
                            (node.SelectNodes("a[1]").[0].InnerText).ToLower(),
                            (
                                try
                                    (node.InnerHtml.Split("<").[0]) |> Int32.Parse
                                with
                                    | ex ->
                                        Console.WriteLine ("error parsing int:" + areaString + ":" + string (fst tableIndex))
                                        0
                            ) * 1<lv>
                        )
                    )
                    |> List.ofSeq
                )
            )
        )

    let jsonOutputString = JsonConvert.SerializeObject(diariesObject)
    File.WriteAllText("diaries-generated.json", jsonOutputString)

[<EntryPoint>]
let main argv =
    //testExperienceFunctions()
    //generateJson()
    sortByExperienceRequired()
    0
