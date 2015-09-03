
open CalendarLib

type t = {
	service : string;
	last_ping : Calendar.t option;
	errors_count : int option;
	last_error : Calendar.t option;
	limit : int option;
	minutes : float option;
	last_fix : Calendar.t option;
	fix_count : int option;
}

let rec lookup_els name rlst docs =
	let open Nethtml in
	let rec loop rlst = function
		| [] -> rlst
		| (Element (n, _, _)) as el :: tl when n = name ->
			el :: loop rlst tl
		| Element (_, _, els) :: tl ->
			let rlst = lookup_els name rlst els in
			loop rlst tl
		| Data _ :: tl -> loop rlst tl
	in
	loop rlst docs

let data_filter str =
	let open BatString in
	nreplace ~str ~sub:"&nbsp;" ~by:" " |> trim

let parse_html html =
	let open Nethtml in
	let table = Lexing.from_string html |> parse_document |> lookup_els "table" [] |> List.hd in
	let fmap = BatList.filter_map in
	lookup_els "tr" [] [table]
		|> fmap (function
			| Element (_, _, els) -> (
				let vals = lookup_els "td" [] els
					|> fmap (function | Element ("td", _, [Data str]) -> let str = data_filter str in Some str | _ -> None)
				in
				let opt f a = try Some (f a) with _ -> None in
				let parse_dt s = opt Printer.CalendarPrinter.from_string s in
				let opt_int s = opt int_of_string s in
				match vals with
					| [f1; f2; f3; f4; f5; f6; f7; f8] -> (
						try
							assert (f1 <> "");
							Some {
								service = f1;
								last_ping = parse_dt f2;
								errors_count = opt_int f3;
								last_error = parse_dt f4;
								limit = opt_int f5;
								minutes = opt float_of_string f6;
								last_fix = parse_dt f7;
								fix_count = opt_int f8;
							}
						with
							| _ -> None
					)
					| _ -> None
			)
			| _ -> None
	)
