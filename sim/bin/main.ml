open Queue;;

let v_max = 100.0 ;;                (* m/s *)
let a_max  = 9.81;;                  (* m/s² *)
let stop_time = 15.0;;              (* s *)
let dt = 0.25;;                      (* s⁻¹ *)

type distance = Indef | D of float;; (* Distance in meters, distance non algebrique *)

type vehicule = P of float*distance*int | V of distance*float*float*float*int*int;;(*
                                           P -> Pad : - Vitesse actuelle (m/s)
                                                      - Distance jusqu'à la prochaine station (m)
                                                      - Prochaine station
                                           V -> Voiture: - Distance jusqu'à l'arrivée (m)
                                                         - Temps dans le circuit (s)
                                                         - Vitesse actuelle (m/s)
                                                         - Vitesse moyenne *10e6 (jusqu'à maintenant) (m/s)
                                                         - Sommet départ
                                                         - Sommet d'arivée
                                           *)

let rec fast_exp x n = match n with
  | 0 -> 1.0
  | _ -> let y = x*.x in
    if n mod 2 = 0 then
      fast_exp y (n/2)
    else
      y *. (fast_exp y (n/2))
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
let get_dist_to_dest graph car stop = match car with
  | P(_, _, _) -> Indef
  | V(_, tmps_circ, _, vit_moy, stat_dep, stat_arr) ->
    if stop = 0 then
      (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*.vit_moy))
    else
      (D(0.0))
;;

let get_dist_to_dest_idx graph index car_array stop_array = match car_array.(index) with
  | P(_, _, _) -> Indef
  | V(_, tmps_circ, _, vit_moy, stat_dep, stat_arr) ->
    if stop_array.(index) = 0 then
      (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*.vit_moy))
    else
      (D(0.0))
;;

let demanded_station pad_array station =
  let res = ref false in
  for i=0 to ((Array.length pad_array) -1) do
    if (snd pad_array.(i)) = station then
      res := true;
  done;
  !res
;;

let get_dist_to_dest_idx_entry graph index car_array pad_array stop_array = match car_array.(index) with
  | P(_, dist2dest, stat_p) ->
    if demanded_station pad_array stat_p then
      dist2dest
    else
      Indef
  | V(_, tmps_circ, _, vit_moy, stat_dep, stat_arr) ->
    if stop_array.(index) = 0 then
      (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*.vit_moy))
    else
      (D(0.0))
;;

(* Fonction qui donne la distance jusqu'à la prochaine voiture *)
(* Automobile -> voiture ou pad *)

let get_next_automobile index automobile_array = automobile_array.((index + 1) mod (Array.length automobile_array))
;;

let distance_to_next_automobile graph index automobile_array stop_array =
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
      (get_dist_to_dest graph automobile stop_array.(index)) -- ((get_dist_to_dest graph next_automobile stop_array.((index + 1) mod (Array.length automobile_array))) ++ (dist_btw_arrAUTOMOBILE_arrNEXT_AUTOMOBILE)) (* A revoir avec un dessin les distances ne sont pas algebriques *)
    | V(_,_,_,_,_,s_arr), P(_, dist_p, stat_p) ->
      (* Cas où on a une voiture et un pad devant *)
      let dist_btw_arrAUTOMOBILE_statP = get_distance graph s_arr stat_p in
      (get_dist_to_dest graph automobile stop_array.(index)) -- (dist_p ++ (dist_btw_arrAUTOMOBILE_statP))
    | P(_, dist_p, stat_p),  V(_,_,_,_,_,s_arr) ->
      (* Cas où on a un pad et une voiture devant *)
      let dist_btw_arrAUTOMOBILE_statP = get_distance graph s_arr stat_p in
      (get_dist_to_dest graph automobile stop_array.(index)) -- (dist_p ++ (dist_btw_arrAUTOMOBILE_statP))
    | P(_, dist_p1, stat_p1),  P(_, dist_p2, stat_p2) ->
      (* Cas où on a un pad et un pad devant *)
      let dist_btw_statP1_statP2 = get_distance graph stat_p1 stat_p2 in
      dist_p1 -- (dist_p2 ++ (dist_btw_statP1_statP2))
;;

(* Fonction qui donne la vitesse actuelle de la voiture *)
let speed_control graph index automobile_array dt stop_array=
  let dist_to_next_auto = distance_to_next_automobile graph index automobile_array stop_array in
  let dist_to_dest  = get_dist_to_dest_idx graph index automobile_array stop_array in
  let next_auto = get_next_automobile index automobile_array in
  let auto = automobile_array.(index) in

  (* let get_impact_time v1 v2 distance = (dist_to_int distance)/(abs (v1-v2)) in *)
  let get_max_speed_no_impact v2 distance = Float.min v_max (Float.max (((dist_to_float distance) -. 2.0)/.dt) v2) in (* 2 m distance min *)
  let get_new_speed speed max_speed = Float.min max_speed (speed +. a_max*.dt) in

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

let speed_control_entry graph index automobile_array dt stop_array dist_to_dest =
  let dist_to_next_auto = distance_to_next_automobile graph index automobile_array stop_array in
  (* Printf.printf "Dist to dest: %f %d \n" (dist_to_float dist_to_dest) index;  *)
  let next_auto = get_next_automobile index automobile_array in
  let auto = automobile_array.(index) in

  (* let get_impact_time v1 v2 distance = (dist_to_int distance)/(abs (v1-v2)) in *)
  let get_max_speed_no_impact v1 v2 distance = Float.min v_max (Float.max (((dist_to_float distance) -. 5.0 -. v1*.dt)/.dt) v2) in (* 2 m distance min *)

  let get_new_speed speed max_speed =
    (* Printf.printf "New speed: %f \n" max_speed; *)
    if max_speed >= (speed +. a_max*.dt) then
      (speed +. a_max*.dt)
    else if max_speed <= (speed -. a_max*.dt) then
      begin
        (* Printf.printf "Danger \n"; *)
        (speed -. a_max*.dt)
      end
    else
      max_speed
  in

  (* let get_new_speed speed max_speed = Float.min (Float.abs (speed -. max_speed)) (speed +. a_max*.dt) in0 *)
  if dist_to_dest >> dist_to_next_auto then
    begin
      match auto, next_auto with
      | V(_,_,v1,_,_,_), P(v2, _, _) -> get_new_speed v1 (get_max_speed_no_impact v1 v2 dist_to_next_auto)
      | V(_,_,v1,_,_,_), V(_,_,v2,_,_,_) -> get_new_speed v1 (get_max_speed_no_impact v1 v2 dist_to_next_auto)
      | P(v1, _, _), V(_,_,v2,_,_,_) -> get_new_speed v1 (get_max_speed_no_impact v1 v2 dist_to_next_auto)
      | P(v1, _, _), P(v2, _, _) -> get_new_speed v1 (get_max_speed_no_impact v1 v2 dist_to_next_auto)
    end
  else
    begin
      match auto with
      | V(_,_,v1,_,_,_)-> get_new_speed v1 (get_max_speed_no_impact v1 0.0 dist_to_dest)
      | P(v1, _, _) -> get_new_speed v1 (get_max_speed_no_impact v1 0.0 dist_to_dest)
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
  | V(dist_to_dest, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
    print_string "----------------------------------------------------------"; print_newline ();
    print_string "                        Voiture :                         "; print_newline ();
    print_string "----------------------------------------------------------"; print_newline ();
    print_newline ();
    print_string "Distance jusqu'à destination (s): "; print_float (dist_to_float dist_to_dest); print_newline ();
    print_string "Temps dans le circuit (s): "; print_float tmps_circ; print_newline ();
    print_string "Vitesse actuelle (m/s): "; print_float vit_act; print_newline ();
    print_string "Vitesse moyenne x10^6 (m/s): "; print_float vit_moy; print_newline ();
    print_string "Station de départ: "; print_int stat_dep; print_newline ();
    print_string "Station d'arrivé: "; print_int stat_arr; print_newline ();
    print_string "=========================================================="; print_newline ();
;;

(* (\* On calcule chaque itération à un intervale de dt *\)
 * let sim_terminate_on_exit_no_entry graph car_array dt stop_time timeout =
 *   (\* C'est la fonction principale, elle s'arrête quand il n'y a plus de voitures dans le tableau car_array. A chaque ittération, elle met à jour les paramètres de chaqu'une des voitures et les transforme en pads si beusion est. *\)
 *   (\* Les valeurs de chaque voiture sont stockés dans 6 collones de la matrice data. Pour ne pas avoir de problèmes de redimensionnement, on prend comme dimension la somme des temps optimaux * 2 divisé par dt. L'hypothèse est que nous ne pouvons pas aire pire que 2*le tmps opt.*\)
 *   let get_dimensions num_cars car_array =
 *     let rec aux acc i =
 *       if i < num_cars then
 *         match car_array.(i) with
 *         | P(_, _, _) -> aux acc (i+1)
 *         | V(tmps_opt, _, _, _, _, _) -> aux (acc +. tmps_opt) (i+1)
 *       else acc
 *     in int_of_float (4.0*.(aux 0.0 0)/.dt)
 *   in
 * 
 *   let num_cars = Array.length car_array in
 *   let stop_array = Array.make num_cars (-1) in (\* Tableau qui contient le # de tours que un vehicule s'arrête. *\)
 *   let num_stations = Array.length graph in
 *   let dim = min (get_dimensions num_cars car_array) timeout in
 *   let data = Array.make_matrix dim (6*num_cars + 1) 0.0 in (\* 6 colonnes pour les voitures et 1 pour le # d'ittération. Cette approche est très coûteuse en mémoire vive car il faut garder en mémoire toutes les données. Il faut l'optimiser ou faire un data dump après n opérations, n une valeur à trouver empiriquement. *\)
 * 
 *   let terminate_loop car_array =
 *     let rec aux acc i =
 *       if (i < num_cars)&&acc then
 *         match car_array.(i) with
 *         | P(_,_,_) -> aux acc (i+1)
 *         | V(_, _, _, _, _, _) -> aux false (i+1)
 *       else acc
 *     in aux true 0
 *   in
 * 
 *   let update_data iter car_array =
 *     data.(iter).(6*num_cars) <- 0.0;
 *     for i=0 to (num_cars - 1) do
 *       match car_array.(i) with
 *       | P(vit_act, dist_p, stat_p) ->
 *         data.(iter).(6*i) <- vit_act;
 *         data.(iter).(6*i+1) <- (dist_to_float dist_p);
 *         data.(iter).(6*i+2) <- float_of_int stat_p;
 *         data.(iter).(6*i+3) <- (-1.0);
 *         data.(iter).(6*i+4) <- (-1.0);
 *         data.(iter).(6*i+5) <- (-1.0);
 *       | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
 *         data.(iter).(6*i) <- tmps_opt;
 *         data.(iter).(6*i+1) <- tmps_circ;
 *         data.(iter).(6*i+2) <- vit_act;
 *         data.(iter).(6*i+3) <- vit_moy;
 *         data.(iter).(6*i+4) <- float_of_int stat_dep;
 *         data.(iter).(6*i+5) <- float_of_int stat_arr;
 *     done;
 *   in
 * 
 *   let write_data file =
 *     let oc = open_out file in
 *     (\* Writing colmn names to file *\)
 *     for car = 0 to num_cars-1 do
 *       Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_optimal_time" car "Current_pad_speed" car;
 *       Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_circuit_time" car "Distance_to_destination_pad" car;
 *       Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_current_speed" car "Next_station_pad" car;
 *       Printf.fprintf oc "%s_%d," "Vehicle_average_speed" car;
 *       Printf.fprintf oc "%s_%d," "Vehicle_departure_station" car;
 *       Printf.fprintf oc "%s_%d," "Vehicle_arrival_station" car;
 *     done;
 *     Printf.fprintf oc "%s\n" "Iteration";
 * 
 *     (\* Writing data to file *\)
 *     for iter = 0 to dim -1 do
 *       for col = 0 to (6*num_cars - 1) do
 *         if not (((col mod 6) = 4)||((col mod 6) = 5)) then (
 *           Printf.fprintf oc "%f," data.(iter).(col);
 *         )
 *         else
 *           Printf.fprintf oc "%d," (int_of_float data.(iter).(col));
 *       done;
 *       Printf.fprintf oc "%d\n" iter;
 *     done;
 *     close_out oc;
 *   in
 * 
 *   let rec loop n car_array =
 *     if (not (terminate_loop car_array)) &&(n < timeout) then
 *       begin
 *         let new_car_array = Array.copy car_array in
 *         (\* On met à jour les données de chaque voiture. *\)
 *         for i=0 to (num_cars-1) do
 *           match new_car_array.(i) with
 *           | P(_, dist_p, stat_p) ->
 *             Printf.printf "PAD/n";
 *             let speed = speed_control graph i car_array dt stop_array in
 *             let dist = dist_p -- (D(speed*.dt)) in (\* Ici interviennent les défauts de la discretisation, on considére que la voiture arrive à la vitesse demandé instantanement. C'est pour ça que le speed control doit être bein fait.  *\)
 *             if (dist_to_float dist) < 2.0 then
 *               let next_station = get_next_station stat_p num_stations in
 *               new_car_array.(i) <- P(speed, dist, next_station);
 *             else
 *               let next_station = stat_p in
 *               new_car_array.(i) <- P(speed, dist, next_station);
 *           | V(dist_to_dest, tmps_circ, speed, vit_moy, stat_dep, stat_arr) ->
 *             let new_dist_to_dest = dist_to_dest -- (D(speed*.dt)) in
 *             if ((dist_to_float dist_to_dest) < 10.0)&&(stop_array.(i) <> (-1) ) then (\* stop array = -1 si la voiture s'est déjà arrêté. Attention aux erreurs de discretisation, il faudrait peut être un intervalle car la voiture pourrait ne pas s'arrêter exactement à la station *\)
 *               begin
 *                 if stop_array.(i) = 0 then (\* Si la voiture ne s'est pas encore arrêté. et doit s'arrêter*\)
 *                   begin
 *                     Printf.printf "arrêt \n";
 *                     let res = tmps_circ in
 *                     Printf.printf "ratio: %f %d_%d \n" res stat_dep stat_arr;
 *                     stop_array.(i) <- int_of_float (stop_time /. dt);
 *                     let speed = 0.0 in
 *                     new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
 *                   end
 *                 else if stop_array.(i) = (-1) then (\* Si la voiture doit repartir *\)
 *                   begin
 *                     Printf.printf "depart \n";
 *                     stop_array.(i) <- 0;
 *                     let speed = speed_control graph i car_array dt stop_array in
 *                     new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
 *                   end
 *                 else if stop_array.(i) = 1 then (\* Si la voiture est à l'arrêt mais va reparit la prochaine ittération, la voiture se transforme en pad. *\)
 *                   begin
 *                     stop_array.(i) <- (-1);
 *                     let speed = 0.0 in
 *                     let next_station = get_next_station stat_arr num_stations in
 *                     let distance = get_distance graph stat_arr next_station in
 *                     new_car_array.(i) <- P(speed, distance, next_station);
 *                   end
 *                 else (\* Si la voiture est à l'arrêt mais ne va pas reparit la prochaine ittération *\)
 *                   begin
 *                     stop_array.(i) <- (stop_array.(i) - 1);
 *                     let speed = 0.0 in
 *                     new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
 *                   end
 *               end
 *             else
 *               begin
 *                 let speed = speed_control graph i car_array dt stop_array in
 *                 stop_array.(i) <- 0;
 *                 new_car_array.(i) <- V(tmps_opt, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
 *               end
 *         done;
 *         update_data n new_car_array;
 *         loop (n+1) new_car_array;
 *       end
 *     else
 *       begin
 *         Printf.printf "writing ... \n";
 *         write_data "output.txt";
 *         Printf.printf "done \n" ;
 *       end
 *   in
 *   update_data 0 car_array;
 *   loop 1 car_array;
 * ;; *)

let print_array array =
  Array.iter (fun x ->
    Printf.printf " |%d %d| " (fst x) (snd x)
    ) array;
  print_newline ();
;;


let print_array2 array =
  Array.iter (fun x ->
      Printf.printf "| %d |" x)
    array;
  print_newline ();
;;

let array_mem (a, b) array=
  let res = ref false in
  Array.iter (fun item ->
    let a',b' = item in
    if (a'= a)&&(b'=b) then
      res := true;
    ) array;
  !res
;;

let sim_terminate_on_exit_with_entry graph car_array entry_queue pad_array dt stop_time timeout max_cars =
  (* C'est la fonction principale, elle s'arrête quand il n'y a plus de voitures dans le tableau car_array. A chaque ittération, elle met à jour les paramètres de chaqu'une des voitures et les transforme en pads si beusion est. *)
  (* Les valeurs de chaque voiture sont stockés dans 6 collones de la matrice data. Pour ne pas avoir de problèmes de redimensionnement, on prend comme dimension la somme des temps optimaux * 2 divisé par dt. L'hypothèse est que nous ne pouvons pas aire pire que 2*le tmps opt.*)
  (* let get_dimensions num_cars car_array =
   *   let rec aux acc i =
   *     if i < num_cars then
   *       match car_array.(i) with
   *       | P(_, _, _) -> aux (acc +. stop_time) (i+1)
   *       | V(tmps_opt, _, _, _, _, _) -> aux (acc +. tmps_opt) (i+1)
   *     else acc
   *   in (\* int_of_float (10.0*.(aux 0.0 0)/.dt) *\) 20000
   * in *)

  let num_cars = Array.length car_array in
  let stop_array = Array.make num_cars (-1) in (* Tableau qui contient le # de tours que un vehicule s'arrête. *)
  let num_stations = Array.length graph in
  (* let dim = min (get_dimensions num_cars car_array) timeout in *)
  (* let dim = timeout in
   * let data = Array.make_matrix dim (6*num_cars + 1) 0.0 in (\* 6 colonnes pour les voitures et 1 pour le # d'ittération. Cette approche est très coûteuse en mémoire vive car il faut garder en mémoire toutes les données. Il faut l'optimiser ou faire un data dump après n opérations, n une valeur à trouver empiriquement. *\) *)

  let terminate_loop car_array =
    let rec aux acc i =
      if (i < num_cars)&&acc then
        match car_array.(i) with
        | P(_,_,_) -> aux acc (i+1)
        | V(_, _, _, _, _, _) -> aux false (i+1)
      else acc
    in (aux true 0)
  in

  (* let update_data iter car_array =
   *   data.(iter).(6*num_cars) <- 0.0;
   *   for i=0 to (num_cars - 1) do
   *     match car_array.(i) with
   *     | P(vit_act, dist_p, stat_p) ->
   *       data.(iter).(6*i) <- vit_act;
   *       data.(iter).(6*i+1) <- (dist_to_float dist_p);
   *       data.(iter).(6*i+2) <- float_of_int stat_p;
   *       data.(iter).(6*i+3) <- (-1.0);
   *       data.(iter).(6*i+4) <- (-1.0);
   *       data.(iter).(6*i+5) <- (-1.0);
   *     | V(dist_to_dest, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
   *       data.(iter).(6*i) <- dist_to_float dist_to_dest;
   *       data.(iter).(6*i+1) <- tmps_circ;
   *       data.(iter).(6*i+2) <- vit_act;
   *       data.(iter).(6*i+3) <- vit_moy;
   *       data.(iter).(6*i+4) <- float_of_int stat_dep;
   *       data.(iter).(6*i+5) <- float_of_int stat_arr;
   *   done;
   * in
   * 
   * let write_data file =
   *   let oc = open_out file in
   *   (\* Writing colmn names to file *\)
   *   for car = 0 to num_cars-1 do
   *     Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_optimal_time" car "Current_pad_speed" car;
   *     Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_circuit_time" car "Distance_to_destination_pad" car;
   *     Printf.fprintf oc "%s_%d/%s_%d," "Vehicle_current_speed" car "Next_station_pad" car;
   *     Printf.fprintf oc "%s_%d," "Vehicle_average_speed" car;
   *     Printf.fprintf oc "%s_%d," "Vehicle_departure_station" car;
   *     Printf.fprintf oc "%s_%d," "Vehicle_arrival_station" car;
   *   done;
   *   Printf.fprintf oc "%s\n" "Iteration";
   * 
   *   (\* Writing data to file *\)
   *   for iter = 0 to dim -1 do
   *     for col = 0 to (6*num_cars - 1) do
   *       if not (((col mod 6) = 4)||((col mod 6) = 5)) then (
   *         Printf.fprintf oc "%f," data.(iter).(col);
   *       )
   *       else
   *         Printf.fprintf oc "%d," (int_of_float data.(iter).(col));
   *     done;
   *     Printf.fprintf oc "%d\n" iter;
   *   done;
   *   close_out oc;
   * in *)

  let get_number_of_available_pads car_array =
    let num = ref 0 in
    Array.iter (fun car ->
      match car with
      | P(_,_,_) -> incr num;
      | V(_, _, _, _, _, _) ->  num := (!num);
      ) car_array;
    !num
  in

  let rec find_new_car car_queue stat_p =
    let station, car, id = Queue.peek car_queue in
    if station = stat_p then
      car, id
    else
      find_new_car car_queue stat_p
  in
  let get_total_waiting_time_neg stop_array =
    let res = ref 0 in
    Array.iter (fun time ->
        if time < 0 then
          res := (!res) + time;
      )stop_array;
    (!res)
  in
  let get_total_waiting_time_pos stop_array =
    let res = ref 0 in
    Array.iter (fun time ->
        if time > 0 then
          begin
            res := (!res) + time;
          end
      )stop_array;
    (!res)
  in
  let time_array = Array.make max_cars 0.0 in
  let rec loop n car_array entry_queue pad_array car_queue =
    if (not (terminate_loop car_array))&&(n < timeout)||(not (Queue.is_empty entry_queue)) then
      begin
        let new_car_array = Array.copy car_array in
        let new_pad_array = Array.copy pad_array in
        let new_entry_queue = Queue.copy entry_queue in
        let num_available_pads = get_number_of_available_pads new_car_array in
        let new_car_queue = Queue.copy car_queue in

        if not (Queue.is_empty new_entry_queue) then
          begin
            for i=0 to (num_available_pads-1) do
              let time, station, id, new_car = Queue.pop new_entry_queue in
              (* Printf.printf "%d %d %f\n" station id time; *)

              Queue.push (station, new_car, id) new_car_queue;
              (* Demande d'entrée dans le circuit *)
              if (time < ((float_of_int n)*.dt)) then
                Array.iteri (fun idx car ->
                    match car with
                    | P(_,_,_) ->
                      if (not (array_mem (id, station) new_pad_array))&&(new_pad_array.(idx) = (-1, -1)) then
                        begin
                          new_pad_array.(idx) <- (id, station);
                        end
                    | V(_, _, _, _, _, _) -> new_pad_array.(idx) <- (-1, -1)
                  ) car_array;
            done;
            (* Printf.printf "Init: ";
             * print_array new_pad_array; *)
          end;
        (* On met à jour les données de chaque voiture. *)
        for i=0 to (num_cars-1) do
          match new_car_array.(i) with
          | P(c_speed, dist_p, stat_p) ->
            if (demanded_station new_pad_array stat_p) then
              begin
                let dist = (D((dist_to_float dist_p) -.  c_speed*.dt)) in (* Ici interviennent les défauts de la discretisation, on considére que la voiture arrive à la vitesse demandé instantanement. C'est pour ça que le speed control doit être bein fait.  *)
                let speed = speed_control_entry graph i car_array  dt stop_array dist_p in
                if ((dist_to_float dist) < 10.0)then
                  begin
                    (* Printf.printf "Pad to Car \n"; *)
                    let new_car, id = find_new_car new_car_queue stat_p in
                    new_car_array.(i) <- new_car;
                    stop_array.(i) <-  ( -(int_of_float (stop_time /. dt)) + (get_total_waiting_time_neg stop_array));
                    (* disp_car new_car; *)
                    (* Printf.printf "Stopping time: %d \n" stop_array.(i); *)
                    (* Remove car from car queue *)
                    let tmpcq = Queue.create () in
                    Queue.iter (fun info ->
                        if not (info = (stat_p, new_car, id)) then
                          Queue.push info tmpcq;
                      ) new_car_queue;
                    Queue.clear new_car_queue;
                    Queue.transfer tmpcq new_car_queue;

                    (* Remove demanded station from pad array *)
                    Array.iteri (fun i info ->
                        if info = (id, stat_p) then
                          new_pad_array.(i) <- (-1, -1);
                      ) new_pad_array;
                    (* Printf.printf "Removed |%d %d|: " id stat_p;
                     * print_array new_pad_array; *)
                  end
                else
                  begin
                    (* Printf.printf "Demanded station, arriving: %f %f \n" (dist_to_float dist) speed ; *)
                    new_car_array.(i) <- (P(speed, dist, stat_p))
                  end
              end
            else
              begin
                let speed = speed_control_entry graph i car_array  dt stop_array Indef in
                let dist = dist_p -- (D(speed*.dt)) in (* Ici interviennent les défauts de la discretisation, on considére que la voiture arrive à la vitesse demandé instantanement. C'est pour ça que le speed control doit être bein fait.  *)
                if stop_array.(i) = 0 then
                  begin
                    (* Printf.printf "Depart pad \n"; *)
                    let next_station = get_next_station stat_p num_stations in
                    stop_array.(i) <- 1;
                    let speed = speed_control_entry graph i car_array  dt stop_array Indef in
                    new_car_array.(i) <- P(speed,  (get_distance graph stat_p next_station), next_station);
                  end
                else if (dist_to_float dist) < 10.0 then
                  begin
                    let next_station = get_next_station stat_p num_stations in
                    (* Printf.printf "Changing pad station: %d %d \n" stat_p next_station; *)
                    new_car_array.(i) <- P(speed,  (get_distance graph stat_p next_station), next_station);
                  end
                else
                  begin
                    let next_station = stat_p in
                    let speed = speed_control_entry graph i car_array dt stop_array Indef in
                    new_car_array.(i) <- P(speed, dist, next_station);
                  end
              end
          | V(old_dist_to_dest, tmps_circ, speed, vit_moy, stat_dep, stat_arr) ->
            let dist_to_dest = old_dist_to_dest -- (D(speed*.dt)) in
            if ((dist_to_float dist_to_dest) < 10.0) then (* stop array = -1 si la voiture s'est déjà arrêté. Attention aux erreurs de discretisation, il faudrait peut être un intervalle car la voiture pourrait ne pas s'arrêter exactement à la station *)
              begin
                if (stop_array.(i) = 0) then (* Si la voiture ne s'est pas encore arrêté. et doit s'arrêter*)
                  begin
                    (* Printf.printf "Arrêt voiture \n"; *)
                    stop_array.(i) <- ((int_of_float (stop_time /. dt)) + (get_total_waiting_time_pos stop_array));
                    (* Printf.printf "Waiting time : %d \n" (get_total_waiting_time_pos stop_array); *)
                    let speed = 0.0 in
                    new_car_array.(i) <- V(dist_to_dest, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
                else if (stop_array.(i) = 1) then (* Si la voiture est à l'arrêt mais va reparit la prochaine ittération, la voiture se transforme en pad. *)
                  begin
                    Printf.printf "Car to Pad \n";
                    (* Printf.printf "Temps circuit %d_%d: %f \n" stat_dep stat_arr tmps_circ; *)
                    time_array.(i) <- tmps_circ;
                    stop_array.(i) <- 0;
                    let speed = 0.0 in
                    let next_station = get_next_station stat_arr num_stations in
                    let distance = get_distance graph stat_arr next_station in
                    new_car_array.(i) <- P(speed, distance, next_station);
                  end
                else if stop_array.(i) > 0 then (* Si la voiture est à l'arrêt mais ne va pas reparit la prochaine ittération *)
                  begin
                    stop_array.(i) <- (stop_array.(i) - 1);
                    let speed = 0.0 in
                    new_car_array.(i) <- V(dist_to_dest, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
                  end
              end
            else if stop_array.(i) = (-1) then
              begin
                (* Printf.printf "Depart  voiture\n"; *)
                stop_array.(i) <- 0;
                let speed = speed_control graph i car_array dt stop_array in
                new_car_array.(i) <- V(dist_to_dest, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
              end
            else if stop_array.(i) < 0 then (* Si la voiture est à l'arrêt mais ne va pas reparit la prochaine ittération *)
              begin
                stop_array.(i) <- (stop_array.(i) + 1);
                let speed = 0.0 in
                new_car_array.(i) <- V(dist_to_dest, tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
              end
            else
              begin
                let speed = speed_control_entry graph i car_array dt stop_array dist_to_dest in
                new_car_array.(i) <- V(dist_to_dest , tmps_circ+.dt, speed, vit_moy*.((float_of_int n)/.(float_of_int (n+1))) +. speed/.(float_of_int (n+1)), stat_dep, stat_arr);
              end
        done;
        (* update_data n new_car_array; *)
        (* print_array2 stop_array; *)
        loop (n+1) new_car_array new_entry_queue new_pad_array new_car_queue;
      end
    else
      begin
        Printf.printf "writing ... \n";
        (* write_data "output.txt"; *)
        Printf.printf "done \n" ;
      end
  in
  (* update_data 0 car_array; *)
  loop 1 car_array entry_queue pad_array (Queue.create (););
  time_array
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
graph.(0).(1) <- (D(1000.0));;
graph.(0).(2) <- (D(2500.0));;

graph.(1).(1) <- (D(0.0));;
graph.(1).(0) <- (D(2500.0));;
graph.(1).(2) <- (D(1500.0));;

graph.(2).(2) <- (D(0.0));;
graph.(2).(0) <- (D(1000.0));;
graph.(2).(1) <- (D(2000.0));;

(* (((dist_to_float (get_distance graph 0 1)) -. (fast_exp v_max 2)/.a_max)/.v_max +. 2.0*.v_max/.a_max) *)

let v0_1 = V(get_distance graph 0 1,0.0,0.0,0.0,0,1);;
let v0_2 = V(get_distance graph 0 2,0.0,0.0,0.0,0,2);;
let v1_2 = V(get_distance graph 1 2 ,0.0,0.0,0.0,1,2);;
let v1_0 = V(get_distance graph 1 0 ,0.0,0.0,0.0,1,0);;
(* let v0_1' = V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,0,1);; *)
(* disp_car v0_1;; *)
(* let v1_0 = V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,1,0);;
 * let v2_1 = V((dist_to_float (get_distance graph 0 1))/.v_max,0.0,0.0,0.0,2,1);;
 * 
 * let v1_2 = V((dist_to_float (get_distance graph 1 2))/.v_max,0.0,0.0,0.0,1,2);;
 * disp_car v1_2;;
 * let v0_2 = V((dist_to_float (get_distance graph 0 2))/.v_max,0.0,0.0,0.0,0,2);;
 * disp_car v0_2;;
 * let v2_0 = V((dist_to_float (get_distance graph 2 0))/.v_max,0.0,0.0,0.0,2,0);;
 * disp_car v2_0;; *)
 

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

let pad0 = (P(0.0, (D(0.0)) , 0));;
let pad1 = (P(0.0, (D(15.0)) , 0));;

(* let n = 7;; *)
(* let car_array_init = [| pad0; pad1 |];; *)

let n_pad n =
  let rec aux acc n i =
    if i<n then
      aux ((P(0.0, (D(i*.15.0)) , 0))::acc) n (i +. 1.0)
    else
      acc
  in aux [] n 0.0
;;

(* for n = 8 to 21 do
 *   let car_array_init = Array.of_list (List.rev (n_pad (float_of_int n))) in
 *   let pad_array = Array.make n (-1,-1) in
 *   let entry_queue = Queue.create () in
 * 
 *   for i=0 to (n-1) do
 *     Queue.add (0.0 , 0, i, v0_1) entry_queue;
 *   done;
 *   Printf.printf "Started \n";
 *   let time_array = sim_terminate_on_exit_with_entry graph car_array_init entry_queue pad_array dt stop_time 50000000 n in
 * 
 *   let get_ratio time_array opt n =
 *     let ratio = ref 0.0 in
 *     Array.iter (fun time -> ratio := (!ratio) +. opt/.time; ) time_array;
 *     (!ratio) /. n
 *   in
 * 
 *   Printf.printf "Ratio for %d cars: %f \n" n (get_ratio time_array 45.0 (float_of_int n));
 * done;; *)

let n = 15;;
let car_array_init = Array.of_list (List.rev (n_pad (float_of_int n))) ;;
let pad_array = Array.make (4*n) (-1,-1);;
let entry_queue = Queue.create ();;

for i=0 to (n-1) do
  Queue.add (0.0 , 0, 4*i, v0_1) entry_queue;
  Queue.add (0.0 , 0, 4*i+1, v0_2) entry_queue;
  Queue.add (0.0 , 1, 4*i+2, v1_2) entry_queue;
  Queue.add (0.0 , 1, 4*i+3, v1_0) entry_queue;
done;;
Printf.printf "Started \n";;
let time_array = sim_terminate_on_exit_with_entry graph car_array_init entry_queue pad_array dt stop_time 50000000 n ;;

let get_ratio time_array opt n =
  let ratio = ref 0.0 in
  Array.iter (fun time -> ratio := (!ratio) +. opt/.time; ) time_array;
  (!ratio) /. n
;;

Printf.printf "Ratio for %d cars: %f \n" (4*n) (get_ratio time_array 25.25 (float_of_int n));;
(* +. (float_of_int i) *)

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


