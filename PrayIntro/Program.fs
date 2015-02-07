open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let str_to_array (str: string) = str.ToCharArray() |> Array.map (fun c -> c.ToString())
let tiangan = "甲乙丙丁戊己庚辛壬癸" |> str_to_array
let dizhi = "子丑寅卯辰巳午未申酉戌亥" |> str_to_array
let prayables = "乙丑甲戌甲申己亥己酉甲寅庚申丙戌"

let index (ld:string) = 
    let t = tiangan |> Array.findIndex (fun x -> x = ld.[0].ToString())
    let d = dizhi |> Array.findIndex (fun x -> x = ld.[1].ToString())
    t, d

let next (t, d) = (t + 1) % 10, (d + 1) % 12

let index_to_string (t, d) = tiangan.[t] + dizhi.[d]

let rec get_lunardays (current:string) (duration:int) : string list= 
    match duration with
    | du when du > 0 -> 
        let next_lunarday = current |> index |> next |> index_to_string
        current :: get_lunardays next_lunarday (du - 1)
    | du when du = 0 -> []
    | _ -> []

let lds = get_lunardays "甲子" 60 |> List.toArray
    
let get_lunarday_from_date (date:string) = 
    let zero = System.DateTime.Parse("2014-02-22")
    let dt = System.DateTime.Parse(date)
    let days = (dt - zero).Days
    lds.[days % 60]
    
    
let rec get_lunardays_with_range from' to' = 
    let from_date = System.DateTime.Parse(from')
    let to_date = System.DateTime.Parse(to')
    let span = (to_date - from_date).Days
    match span with
    | sp when sp < 0 -> []
    | _ as sp -> 
        let next' = from_date.AddDays(1.0).ToString("yyyy-MM-dd")
        let item = (from', from' |> get_lunarday_from_date)
        item :: (get_lunardays_with_range next' to')
        
let is_prayable (date:string, lday:string) :bool = if prayables.IndexOf(lday) >= 0 then true else false

[<EntryPoint>]
let main argv =     
    let all_lunardasy_2014 =  get_lunardays_with_range (DateTime.Now.ToString("yyyy-MM-dd")) "2014-12-31" |> List.toArray
    Array.FindAll(all_lunardasy_2014, (fun x -> x |> is_prayable))
        |> Array.iter (fun (d, l) -> printfn "%s" (d + "\t" + l))
    Console.ReadLine() |> ignore
    0
    
    