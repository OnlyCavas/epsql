type column = { name : string; type_oid : int }
type t = { columns : column list; rows : string option list list; tag : string }
type response = { result : t list; errors : Message.error list }
