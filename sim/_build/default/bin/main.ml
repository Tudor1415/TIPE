open Lib;;

let v_max = 100.0 ;;                (* m/s *)
let a_max  = 9.81;;                  (* m/s² *)
let stop_time = 15.0;;              (* s *)
let dt = 0.1;;                      (* s⁻¹ *)

type distance = Indef | D of float;; (* Distance in meters, distance non algebrique *)

type vehicule = P of float*distance*int | V of float*float*float*float*int*int;;(*
                                           P -> Pad : - Vitesse actuelle (m/s)
                                                      - Distance jusqu'à la prochaine station (m)
                                                      - Prochaine station
                                           V -> Voiture: - Temps jusqu'à l'arrivée optimal deupis le depart (constant) (s)
                                                         - Temps dans le circuit (s)
                                                         - Vitesse actuelle (m/s)
                                                         - Vitesse moyenne *10e6 (jusqu'à maintenant) (m/s)
                                                         - Sommet départ
                                                         - Sommet d'arivée
                                           *)

let rec fast_exp x n = match n with
  | 0 -> 1
  | _ -> let y = x*x in
    if n mod 2 = 0 then
      fast_exp y (n/2)
    else
      y * (fast_exp y (n/2))
;;

(* Defining arithmetic operators for the distance type *)
let (++) d1 d2 = match d1, d2 with
  | Indef, _ -> Indef
  | _, Indef -> Indef
  | D(x), D(y) -> D(x +. y)
;;

let (--) d1 d2 = match d1, d2 with
  | Indef, _ -> Indef
  | _, Indef -> Indef
  | D(x), D(y) -> D(Float.abs (x -. y))
;;

let (>>) d1 d2 = match d1, d2 with
  | Indef, _ -> true
  | _, Indef -> false
  | D(x), D(y) -> x>y
;;

let dist_to_float d = match d with
  | Indef -> Float.infinity
  | D(x) -> x
;;

let print_dist d = match d with
  | Indef -> print_string "Non définie.";
  | D(x) -> print_float x;
;;

(* Fonction qui donne la distance d'un trajet *)
let get_next_station station total = (station + 1) mod total;; (* Car le circuit est un cycle simple. *)
let get_distance graph departure arrival =
  let total = Array.length graph in

  let rec aux graph departure arrival distance =
    if arrival = departure then
      distance
    else
      begin
       let next_station = get_next_station departure total in
       aux graph next_station arrival (graph.(departure).(next_station) ++ distance)
      end
  in
  aux graph departure arrival (D(0.0));;

(* Fonction qui donne la distance jusqu'à l'arrivée *)
let get_dist_to_dest graph car = match car with
  | P(_, _, _) -> Indef
  | V(_, tmps_circ, _, vit_moy, stat_dep, stat_arr) -> (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*.vit_moy));
;;

let get_dist_to_dest_idx graph index car_array = match car_array.(index) with
  | P(_, _, _) -> Indef
  | V(_, tmps_circ, _, vit_moy, stat_dep, stat_arr) -> (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*.vit_moy));
;;
(* Fonction qui donne la distance jusqu'à la prochaine voiture *)
(* Automobile -> voiture ou pad *)

let get_next_automobile index automobile_array = automobile_array.((index + 1) mod (Array.length automobile_array))
;;

let distance_to_next_automobile graph index automobile_array =
  let n = Array.length automobile_array in
  if n < 2 then
    Indef
  else
    let automobile = automobile_array.(index) in
    let next_automobile = get_next_automobile index automobile_array in
    match automobile, next_automobile with
    | V(_,_,_,_,_,s_arr_1), V(_,_,_,_,_,s_arr_2) ->
      (* Cas où on a deux voitures *)
      let dist_btw_arrAUTOMOBILE_arrNEXT_AUTOMOBILE = get_distance graph s_arr_1 s_arr_2 in
      (get_dist_to_dest graph automobile) -- ((get_dist_to_dest graph next_automobile) ++ (dist_btw_arrAUTOMOBILE_arrNEXT_AUTOMOBILE)) (* A revoir avec un dessin les distances ne sont pas algebriques *)
    | V(_,_,_,_,_,s_arr), P(_, dist_p, stat_p) ->
      (* Cas où on a une voiture et un pad devant *)
      let dist_btw_arrAUTOMOBILE_statP = get_distance graph s_arr stat_p in
      (get_dist_to_dest graph automobile) -- (dist_p ++ (dist_btw_arrAUTOMOBILE_statP))
    | P(_, dist_p, stat_p),  V(_,_,_,_,_,s_arr) ->
      (* Cas où on a un pad et une voiture devant *)
      let dist_btw_arrAUTOMOBILE_statP = get_distance graph s_arr stat_p in
      (get_dist_to_dest graph automobile) -- (dist_p ++ (dist_btw_arrAUTOMOBILE_statP))
    | P(_, dist_p1, stat_p1),  P(_, dist_p2, stat_p2) ->
      (* Cas où on a un pad et un pad devant *)
      let dist_btw_statP1_statP2 = get_distance graph stat_p1 stat_p2 in
      dist_p1 -- (dist_p2 ++ (dist_btw_statP1_statP2))
;;




(* Fonction qui donne la vitesse actuelle de la voiture *)
let speed_control graph index automobile_array dt =
  let dist_to_next_auto = distance_to_next_automobile graph index automobile_array in
  let dist_to_dest  = get_dist_to_dest_idx graph index automobile_array in
  let next_auto = get_next_automobile index automobile_array in
  let auto = automobile_array.(index) in

  (* let get_impact_time v1 v2 distance = (dist_to_int distance)/(abs (v1-v2)) in *)
  let get_max_speed_no_impact v2 distance = Float.min v_max (Float.max (((dist_to_float distance) -. 2.0)/.dt) v2) in (* 2 m distance min *)
  let get_new_speed speed max_speed = Float.min (Float.abs (speed -. max_speed)) (speed +. a_max*.dt) in

  if dist_to_dest >> dist_to_next_auto then
    begin
      match auto, next_auto with
      | V(_,_,v1,_,_,_), P(v2, _, _) -> get_new_speed v1 (get_max_speed_no_impact v2 dist_to_next_auto)
      | V(_,_,v1,_,_,_), V(_,_,v2,_,_,_) -> get_new_speed v1 (get_max_speed_no_impact v2 dist_to_next_auto)
      | P(v1, _, _), V(_,_,v2,_,_,_) -> get_new_speed v1 (get_max_speed_no_impact v2 dist_to_next_auto)
      | P(v1, _, _), P(v2, _, _) -> get_new_speed v1 (get_max_speed_no_impact v2 dist_to_next_auto)
    end
  else
    begin
      match auto with
      | V(_,_,v1,_,_,_)-> get_new_speed v1 (get_max_speed_no_impact 0.0 dist_to_dest)
      | P(v1, _, _) -> get_new_speed v1 (get_max_speed_no_impact 0.0 dist_to_dest)
    end
;;

(* let speed_control graph index automobile_array dt =
 * 
 * ;; *)

(* Fonction qui affiche une voiture *)
let disp_car car = match car with
  | P(vit, dist, stat) ->
    print_string "----------------------------------------------------------"; print_newline ();
    print_string "                          Pad :                           "; print_newline ();
    print_string "----------------------------------------------------------"; print_newline ();
    print_newline ();
    print_string "Vitesse actuelle (m/s): "; print_float vit; print_newline ();
    print_string "Distance jusqu'à prochaine station (m): "; print_dist dist ; print_newline ();
    print_string "Prochaine station : "; print_int stat; print_newline ();
    print_string "=========================================================="; print_newline ();
  | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
    print_string "----------------------------------------------------------"; print_newline ();
    print_string "                        Voiture :                         "; print_newline ();
    print_string "----------------------------------------------------------"; print_newline ();
    print_newline ();
    print_string "Temps optimal (s): "; print_float tmps_opt; print_newline ();
    print_string "Temps dans le circuit (s): "; print_float tmps_circ; print_newline ();
    print_string "Vitesse actuelle (m/s): "; print_float vit_act; print_newline ();
    print_string "Vitesse moyenne x10^6 (m/s): "; print_float vit_moy; print_newline ();
    print_string "Station de départ: "; print_int stat_dep; print_newline ();
    print_string "Station d'arrivé: "; print_int stat_arr; print_newline ();
    print_string "=========================================================="; print_newline ();
;;

(* On calcule chaque itération à un intervale de dt *)
let sim_terminate_on_exit_no_entry graph car_array dt stop_time timeout =
  (* C'est la fonction principale, elle s'arrête quand il n'y a plus de voitures dans le tableau car_array. A chaque ittération, elle met à jour les paramètres de chaqu'une des voitures et les transforme en pads si beusion est. *)
  (* Les valeurs de chaque voiture sont stockés dans 6 collones de la matrice data. Pour ne pas avoir de problèmes de redimensionnement, on prend comme dimension la somme des temps optimaux * 2 divisé par dt. L'hypothèse est que nous ne pouvons pas aire pire que 2*le tmps opt.*)
  let get_dimensions num_cars car_array =
    let rec aux acc i =
      if i < num_cars then
        match car_array.(i) with
        | P(_, _, _) -> aux acc (i+1)
        | V(tmps_opt, _, _, _, _, _) -> aux (acc +. tmps_opt) (i+1)
      else acc
    in int_of_float (4.0*.(aux 0.0 0)/.dt)
  in

  let num_cars = Array.length car_array in
  let stop_array = Array.make num_cars (-1) in (* Tableau qui contient le # de tours que un vehicule s'arrête. *)
  let num_stations = Array.length graph in
  let dim = min (get_dimensions num_cars car_array) timeout in
  let data = Array.make_matrix dim (6*num_cars + 1) 0.0 in (* 6 colonnes pour les voitures et 1 pour le # d'ittération. Cette approche est très coûteuse en mémoire vive car il faut garder en mémoire toutes les données. Il faut l'optimiser ou faire un data dump après n opérations, n une valeur à trouver empiriquement. *)

  let terminate_loop car_array =
    let rec aux acc i =
      if (i < num_cars)&&acc then
        match car_array.(i) with
        | P(_,_,_) -> aux acc (i+1)
        | V(_, _, _, _, _, _) -> aux false (i+1)
      else acc
    in aux true 0
  in

  let update_data iter car_array =
    data.(iter).(6*num_cars) <- 0.0;
    for i=0 to (num_cars - 1) do
      match car_array.(i) with
      | P(vit_act, dist_p, stat_p) ->
        data.(iter).(6*i) <- vit_act;
        data.(iter).(6*i+1) <- (dist_to_float dist_p);
        data.(iter).(6*i+2) <- float_of_int stat_p;
        data.(iter).(6*i+3) <- (-1.0);
        data.(iter).(6*i+4) <- (-1.0);
        data.(iter).(6*i+5) <- (-1.0);
      | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
        data.(iter).(6*i) <- tmps_opt;
        data.(iter).(6*i+1) <- tmps_circ;
        data.(iter).(6*i+2) <- vit_act;
        data.(iter).(6*i+3) <- vit_moy;
        data.(iter).(6*i+4) <- float_of_int stat_dep;
        data.(iter).(6*i+5) <- float_of_int stat_arr;
    done;
  in

  let write_data file =
    let oc = open_out file in
    (* Writing colmn names to file *)
    for car = 0 to num_cars-1 do
      Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_optimal_time" car "Current_pad_speed" car;
      Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_circuit_time" car "Distance_to_destination_pad" car;
      Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_current_speed" car "Next_station_pad" car;
      Printf.fprintf oc "%s_%d," "Vehicle_average_speed" car;
      Printf.fprintf oc "%s_%d," "Vehicle_departure_station" car;
      Printf.fprintf oc "%s_%d," "Vehicle_arrival_station" car;
    done;
    Printf.fprintf oc "%s\n" "Iteration";

    (* Writing data to file *)
    for iter = 0 to dim -1 do
      for col = 0 to (6*num_cars - 1) do
        if not (((col mod 6) = 4)||((col mod 6) = 5)) then (
          Printf.fprintf oc "%f," data.(iter).(col);
        )
        else
          Printf.fprintf oc "%d," (int_of_float data.(iter).(col));
      done;
      Printf.fprintf oc "%d\n" iter;
    done;
    close_out oc;
  in

  let rec loop n car_array =
    if (not (terminate_loop car_array)) &&(n < timeout) then
      begin
        let new_car_array = Array.copy car_array in
        (* On met à jour les données de chaque voiture. *)
        for i=0 to (num_cars-1) do
          match new_car_array.(i) with
          | P(_, dist_p, stat_p) ->
            Printf.printf "PAD/n";
            let speed = speed_control graph i car_array dt in
            let dist = dist_p -- (D(speed*.dt)) in (* Ici interviennent les défauts de la discretisation, on considére que la voiture arrive à la vitesse demandé instantanement. C'est pour ça que le speed control doit être bein fait.  *)
            if (dist_to_float dist) < 2.0 then
              let next_station = get_next_station i num_stations in
              new_car_array.(i) <- P(speed, dist, next_station);
            else
              let next_station = stat_p in
              new_car_array.(i) <- P(speed, dist, next_station);
          | V(tmps_opt, tmps_circ, _, vit_moy, stat_dep, stat_arr) ->
            let dist_to_dest = get_dist_to_dest_idx graph i car_array in
            if ((dist_to_float dist_to_dest) < 10.0)&&(stop_array.(i) <> (-1) ) then (* stop array = -1 si la voiture s'est déjà arrêté. Attention aux erreurs de discretisation, il faudrait peut être un intervalle car la voiture pourrait ne pas s'arrêter exactement à la station *)
              begin
                if stop_array.(i) = 0 then (* Si la voiture ne s'est pas encore arrêté. et doit s'arrêter*)
                  begin
                    Printf.printf "arrêt \n";
                    let res = tmps_circ in
                    Printf.printf "ratio: %f %d_%d \n" res stat_dep stat_arr;
                    stop_array.(i) <- int_of_float (stop_time /. dt);
                    let speed = 0.0 in
                    new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
                else if stop_array.(i) = (-1) then (* Si la voiture doit repartir *)
                  begin
                    Printf.printf "depart \n";
                    stop_array.(i) <- 0;
                    let speed = speed_control graph i car_array dt in
                    new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
                else if stop_array.(i) = 1 then (* Si la voiture est à l'arrêt mais va reparit la prochaine ittération, la voiture se transforme en pad. *)
                  begin
                    stop_array.(i) <- (-1);
                    let speed = 0.0 in
                    let next_station = get_next_station stat_arr num_stations in
                    let distance = get_distance graph stat_arr next_station in
                    new_car_array.(i) <- P(speed, distance, next_station);
                  end
                else (* Si la voiture est à l'arrêt mais ne va pas reparit la prochaine ittération *)
                  begin
                    stop_array.(i) <- (stop_array.(i) - 1);
                    let speed = 0.0 in
                    new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
              end
            else
              begin
                let speed = speed_control graph i car_array dt in
                stop_array.(i) <- 0;
                new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
              end
        done;
        update_data n new_car_array;
        loop (n+1) new_car_array;
      end
    else
      begin
        Printf.printf "writing ... \n";
        write_data "output.txt";
        Printf.printf "done \n" ;
      end
  in
  update_data 0 car_array;
  loop 1 car_array;
;;

let sim_terminate_on_exit_with_entry graph car_array entry_queue dt stop_time timeout =
  (* C'est la fonction principale, elle s'arrête quand il n'y a plus de voitures dans le tableau car_array. A chaque ittération, elle met à jour les paramètres de chaqu'une des voitures et les transforme en pads si beusion est. *)
  (* Les valeurs de chaque voiture sont stockés dans 6 collones de la matrice data. Pour ne pas avoir de problèmes de redimensionnement, on prend comme dimension la somme des temps optimaux * 2 divisé par dt. L'hypothèse est que nous ne pouvons pas aire pire que 2*le tmps opt.*)
  let get_dimensions num_cars car_array =
    let rec aux acc i =
      if i < num_cars then
        match car_array.(i) with
        | P(_, _, _) -> aux acc (i+1)
        | V(tmps_opt, _, _, _, _, _) -> aux (acc +. tmps_opt) (i+1)
      else acc
    in int_of_float (4.0*.(aux 0.0 0)/.dt)
  in

  let num_cars = Array.length car_array in
  let stop_array = Array.make num_cars (-1) in (* Tableau qui contient le # de tours que un vehicule s'arrête. *)
  let num_stations = Array.length graph in
  let dim = min (get_dimensions num_cars car_array) timeout in
  let data = Array.make_matrix dim (6*num_cars + 1) 0.0 in (* 6 colonnes pour les voitures et 1 pour le # d'ittération. Cette approche est très coûteuse en mémoire vive car il faut garder en mémoire toutes les données. Il faut l'optimiser ou faire un data dump après n opérations, n une valeur à trouver empiriquement. *)

  let terminate_loop car_array =
    let rec aux acc i =
      if (i < num_cars)&&acc then
        match car_array.(i) with
        | P(_,_,_) -> aux acc (i+1)
        | V(_, _, _, _, _, _) -> aux false (i+1)
      else acc
    in aux true 0
  in

  let update_data iter car_array =
    data.(iter).(6*num_cars) <- 0.0;
    for i=0 to (num_cars - 1) do
      match car_array.(i) with
      | P(vit_act, dist_p, stat_p) ->
        data.(iter).(6*i) <- vit_act;
        data.(iter).(6*i+1) <- (dist_to_float dist_p);
        data.(iter).(6*i+2) <- float_of_int stat_p;
        data.(iter).(6*i+3) <- (-1.0);
        data.(iter).(6*i+4) <- (-1.0);
        data.(iter).(6*i+5) <- (-1.0);
      | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
        data.(iter).(6*i) <- tmps_opt;
        data.(iter).(6*i+1) <- tmps_circ;
        data.(iter).(6*i+2) <- vit_act;
        data.(iter).(6*i+3) <- vit_moy;
        data.(iter).(6*i+4) <- float_of_int stat_dep;
        data.(iter).(6*i+5) <- float_of_int stat_arr;
    done;
  in

  let write_data file =
    let oc = open_out file in
    (* Writing colmn names to file *)
    for car = 0 to num_cars-1 do
      Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_optimal_time" car "Current_pad_speed" car;
      Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_circuit_time" car "Distance_to_destination_pad" car;
      Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_current_speed" car "Next_station_pad" car;
      Printf.fprintf oc "%s_%d," "Vehicle_average_speed" car;
      Printf.fprintf oc "%s_%d," "Vehicle_departure_station" car;
      Printf.fprintf oc "%s_%d," "Vehicle_arrival_station" car;
    done;
    Printf.fprintf oc "%s\n" "Iteration";

    (* Writing data to file *)
    for iter = 0 to dim -1 do
      for col = 0 to (6*num_cars - 1) do
        if not (((col mod 6) = 4)||((col mod 6) = 5)) then (
          Printf.fprintf oc "%f," data.(iter).(col);
        )
        else
          Printf.fprintf oc "%d," (int_of_float data.(iter).(col));
      done;
      Printf.fprintf oc "%d\n" iter;
    done;
    close_out oc;
  in

  let rec loop n car_array entry_queue =
    if (not (terminate_loop car_array)) &&(n < timeout) then
      begin
        let new_car_array = Array.copy car_array in
        (* On met à jour les données de chaque voiture. *)
        for i=0 to (num_cars-1) do
          match new_car_array.(i) with
          | P(_, dist_p, stat_p) ->
            Printf.printf "PAD/n";
            let speed = speed_control graph i car_array dt in
            let dist = dist_p -- (D(speed*.dt)) in (* Ici interviennent les défauts de la discretisation, on considére que la voiture arrive à la vitesse demandé instantanement. C'est pour ça que le speed control doit être bein fait.  *)
            if (dist_to_float dist) < 2.0 then
              let next_station = get_next_station i num_stations in
              new_car_array.(i) <- P(speed, dist, next_station);
            else
              let next_station = stat_p in
              new_car_array.(i) <- P(speed, dist, next_station);
          | V(tmps_opt, tmps_circ, _, vit_moy, stat_dep, stat_arr) ->
            let dist_to_dest = get_dist_to_dest_idx graph i car_array in
            if ((dist_to_float dist_to_dest) < 10.0)&&(stop_array.(i) <> (-1) ) then (* stop array = -1 si la voiture s'est déjà arrêté. Attention aux erreurs de discretisation, il faudrait peut être un intervalle car la voiture pourrait ne pas s'arrêter exactement à la station *)
              begin
                if stop_array.(i) = 0 then (* Si la voiture ne s'est pas encore arrêté. et doit s'arrêter*)
                  begin
                    Printf.printf "arrêt \n";
                    let res = tmps_circ in
                    Printf.printf "ratio: %f %d_%d \n" res stat_dep stat_arr;
                    stop_array.(i) <- int_of_float (stop_time /. dt);
                    let speed = 0.0 in
                    new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
                else if stop_array.(i) = (-1) then (* Si la voiture doit repartir *)
                  begin
                    Printf.printf "depart \n";
                    stop_array.(i) <- 0;
                    let speed = speed_control graph i car_array dt in
                    new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
                else if stop_array.(i) = 1 then (* Si la voiture est à l'arrêt mais va reparit la prochaine ittération, la voiture se transforme en pad. *)
                  begin
                    stop_array.(i) <- (-1);
                    let speed = 0.0 in
                    let next_station = get_next_station stat_arr num_stations in
                    let distance = get_distance graph stat_arr next_station in
                    new_car_array.(i) <- P(speed, distance, next_station);
                  end
                else (* Si la voiture est à l'arrêt mais ne va pas reparit la prochaine ittération *)
                  begin
                    stop_array.(i) <- (stop_array.(i) - 1);
                    let speed = 0.0 in
                    new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
              end
            else
              begin
                let speed = speed_control graph i car_array dt in
                stop_array.(i) <- 0;
                new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
              end
        done;
        update_data n new_car_array;
        loop (n+1) new_car_array;
      end
    else
      begin
        Printf.printf "writing ... \n";
        write_data "output.txt";
        Printf.printf "done \n" ;
      end
  in
  update_data 0 car_array;
  loop 1 car_array;
;;


(* Implementaiton du graph à 3 sommets. *)
let graph = Array.make_matrix 3 3 (D(-1.0)) ;;

(* graph.(0).(0) <- (D(0.0));;
 * graph.(0).(1) <- (D(3000.0));;
 * graph.(0).(2) <- Indef;;
 * 
 * graph.(1).(1) <- (D(0.0));;
 * graph.(1).(0) <- Indef;;
 * graph.(1).(2) <- (D(1500.0));;
 * 
 * graph.(2).(2) <- (D(0.0));;
 * graph.(2).(0) <- (D(1000.0));;
 * graph.(2).(1) <- Indef;; *)


graph.(0).(0) <- (D(0.0));;
graph.(0).(1) <- (D(30000.0));;
graph.(0).(2) <- (D(45000.0));;

graph.(1).(1) <- (D(0.0));;
graph.(1).(0) <- (D(25000.0));;
graph.(1).(2) <- (D(15000.0));;

graph.(2).(2) <- (D(0.0));;
graph.(2).(0) <- (D(10000.0));;
graph.(2).(1) <- (D(40000.0));;

let v0_1 = V((dist_to_float (get_distance graph 0 1))/.v_max,5.0,0.0,5.0,0,1);;
(* let v0_1' = V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,0,1);; *)
disp_car v0_1;;
(* let v1_0 = V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,1,0);;
 * let v2_1 = V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,2,1);;
 * 
 * let v1_2 = V((dist_to_float (get_distance graph 1 2))/.v_max,0.0,0.0,0.0,1,2);;
 * disp_car v1_2;;
 * let v0_2 = V((dist_to_float (get_distance graph 0 2))/.v_max,0.0,0.0,0.0,0,2);;
 * disp_car v0_2;;
 * let v2_0 = V((dist_to_float (get_distance graph 2 0))/.v_max,0.0,0.0,0.0,2,0);;
 * disp_car v2_0;; *)

let car_array_init = [| v0_1 |];;
(* let car_array_init = [| v1_2 |];;
 * let car_array_init = [| v2_1 |];;
 * let car_array_init = [| v2_0 |];;
 * 
 * let car_array_init = [| v2_0; v1_2 |];;
 * let car_array_init = [| v2_0; v2_1 |];;
 * let car_array_init = [| v2_0; v1_0 |];;
 * 
 * let car_array_init = [| v1_0; v1_2 |];;
 * let car_array_init = [| v1_0; v2_1 |];;
 * 
 * let car_array_init = [| v1_2; v2_1 |];; *)

sim_terminate_on_exit_no_entry graph car_array_init dt stop_time 200;;

(* for i=0 to 2 do
 *   for j=0 to 2 do
 *     if j != i then
 *       sim_terminate_on_exit_no_entry graph [| V((dist_to_float (get_distance graph i j))/.v_max,0.0,0.0,0.0,i,j) |] dt stop_time 20000
 *   done;
 * done;; *)

(* for i=0 to 2 do
 *   for j=0 to 2 do
 *     if j != i then
 *       sim_terminate_on_exit_no_entry graph [| V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,0,1); V((dist_to_float (get_distance graph i j))/.v_max,0.0,0.0,0.0,i,j) |] dt stop_time 20000
 *   done;
 * done;; *)


