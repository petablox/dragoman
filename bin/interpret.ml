
let establish_server server_fun sockaddr =
   let domain = Unix.domain_of_sockaddr sockaddr in
   let sock = Unix.socket domain Unix.SOCK_STREAM 0 
   in Unix.bind sock sockaddr ;
      Unix.listen sock 3;
      while true do
        let (s, _) = Unix.accept sock 
        in match Unix.fork() with
               0 -> if Unix.fork() <> 0 then exit 0 ; 
                    let inchan = Unix.in_channel_of_descr s 
                    and outchan = Unix.out_channel_of_descr s 
                    in server_fun inchan outchan ;
                       close_in inchan ;
                       close_out outchan ;
                       exit 0
             | id -> Unix.close s; ignore(Unix.waitpid [] id)
      done ;;

let get_my_addr () =
    (Unix.gethostbyname("127.0.0.1")).Unix.h_addr_list.(0) ;;

let main_server serv_fun =
   if Array.length Sys.argv < 2 then Printf.eprintf "usage : serv_up port\n"
   else try
          let port =  int_of_string Sys.argv.(1) in 
          let my_address = get_my_addr() 
          in establish_server serv_fun  (Unix.ADDR_INET(my_address, port))
        with
          Failure _ -> 
            Printf.eprintf "serv_up : bad port number\n" ;;

let verbose = ref false

let problem json_str = Yojson.Basic.from_string json_str
    |> Problem.of_json
    |> CCOpt.get_exn

let interpret_service ic oc =
   try while true do     
         let json_str = input_line ic in 
         let pb = problem json_str in 
         let tb = Horn.Clause.evaluate ~verbose:!verbose pb.clause pb.scene |> CCOpt.get_exn 
         in output_string oc ( tb |> Core.Table.to_csv ) ; flush oc
       done
   with _ -> exit 0 ;;

let go_interpret_service () =
    Unix.handle_unix_error main_server interpret_service ;;

(* print_string (Unix.gethostname()); *)
go_interpret_service ();;

