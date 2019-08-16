open CCOpt.Infix

type t =
    | Relate of Core.Scene.relation * Variable.t * Variable.t
    | Select of Core.Thing.attribute * Core.Cat.t * Variable.t

let of_json json = let module J = Utility.JSON in
    match J.assoc json "kind" >>= J.string with
        | Some s when s = "relate" ->
            let relation = J.assoc json "relation" >>= J.string in
            let left = J.assoc json "left" >>= Variable.of_json in
            let right = J.assoc json "right" >>= Variable.of_json in
            begin match relation, left, right with
                | Some rel, Some l, Some r ->
                    Some (Relate (rel, l, r))
                | _ -> None
            end
        | Some s when s = "select" ->
            let attribute = J.assoc json "attribute" >>= J.string in
            let value = J.assoc json "value" >>= Core.Cat.of_json in
            let variable = J.assoc json "variable" >>= Variable.of_json in
            begin match attribute, value, variable with
                | Some attr, Some value, Some x ->
                    Some (Select (attr, value, x))
                | _ -> None
            end
        | _ -> None

let to_json = function
    | Relate (rel, x, y) ->
        `Assoc [
            ("kind", `String "relate");
            ("relation", `String rel);
            ("left", Variable.to_json x);
            ("right", Variable.to_json y)
        ]
    | Select (attr, value, x) ->
        `Assoc [
            ("kind", `String "select");
            ("attribute", `String attr);
            ("value", Core.Cat.to_json value);
            ("variable", Variable.to_json x)
        ]

let to_string = function
    | Relate (rel, x, y) ->
        rel ^ "(" ^ (Variable.to_string x) ^ ", " ^ (Variable.to_string y) ^ ")"
    | Select (attr, value, x) ->
        (Variable.to_string x) ^ "." ^ attr ^ " = " ^ (Core.Cat.to_string value)

let evaluate conjunct scene = match conjunct with
    | Relate (rel, x, y) ->
        begin match Core.Scene.relation scene rel with
            | Some ls -> ls
                |> CCList.map (fun (i, j) ->
                    Row.of_list [(x, i) ; (y, j)])
                |> Table.of_list
            | _ -> Some (Table.empty_with_variables [x]) end
    | Select (attr, value, x) ->
        let rows = Core.Scene.things_idx scene
            |> CCList.filter_map (fun (i, thing) ->
                match Core.Thing.attribute thing attr with
                    | Some cat -> if cat = value
                        then Some (Row.singleton x i)
                        else None
                    | _ -> None) in
        if CCList.is_empty rows then
            Some (Table.empty_with_variables [x])
        else Table.of_list rows