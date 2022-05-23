let v_max = 100 ;;                (* m/s *)
let a_max  = 5;;                  (* m/s² *)
let stop_time = 15;;              (* s *)

type distance = Indef | D of int;; (* Distance in meters, distance non algebrique *)

type vehicule = P of int*distance*int | V of int*int*int*int*int*int;;(*
                                           P -> Pad : - Vitesse actuelle (m/s)
                                                      - Distance jusqu'à la prochaine station (m)
                                                      - Prochaine station
                                           V -> Voiture: - Temps jusqu'à l'arrivée optimal deupis le depart (constant) (s)
                                                         - Temps dans le circuit (s)
                                                         - Vitesse actuelle (m/s)
                                                         - Vitesse moyenne (jusqu'à maintenant) (m/s)
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
  | D(x), D(y) -> D(x + y)
;;

let (--) d1 d2 = match d1, d2 with
  | Indef, _ -> Indef
  | _, Indef -> Indef
  | D(x), D(y) -> D(abs (x - y))
;;

let dist_to_int d = match d with
  | Indef -> max_int
  | D(x) -> x
;;

let print_dist d = match d with
  | Indef -> print_string "Non définie.";
  | D(x) -> print_int x;
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
  aux graph departure arrival (D(0));;

(* Fonction qui donne la distance jusqu'à l'arrivée *)
let get_dist_to_dest graph car = match car with
  | P(vit, dist, stat) -> Indef
  | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) -> (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*vit_moy));
;;

let get_dist_to_dest_idx graph index car_array = match car_array.(index) with
  | P(vit, dist, stat) -> Indef
  | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) -> (get_distance graph stat_dep stat_arr) -- (D(tmps_circ*vit_moy));
;;
(* Fonction qui donne la distance jusqu'à la prochaine voiture *)
(* Automobile -> voiture ou pad *)

let get_next_automobile index automobile_array = automobile_array.((index + 1) mod (Array.length automobile_array))
;;

let rec distance_to_next_automobile graph index automobile_array =
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
      dist_p1 -- (dist_p1 ++ (dist_btw_statP1_statP2))
;;



(* Fonction qui donne la vitesse actuelle de la voiture *)
let speed_control graph index automobile_array dt =
  let free_distance = distance_to_next_automobile graph index automobile_array in
  5;;

(* Fonction qui affiche une voiture *)
let disp_car car = match car with
  | P(vit, dist, stat) ->
    print_string "----------------------------------------------------------"; print_newline ();
    print_string "                          Pad :                           "; print_newline ();
    print_string "----------------------------------------------------------"; print_newline ();
    print_newline ();
    print_string "Vitesse actuelle (m/s): "; print_int vit; print_newline ();
    print_string "Distance jusqu'à prochaine station (m): "; print_dist dist ; print_newline ();
    print_string "Prochaine station : "; print_int stat; print_newline ();
    print_string "=========================================================="; print_newline ();
  | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
    print_string "----------------------------------------------------------"; print_newline ();
    print_string "                        Voiture :                         "; print_newline ();
    print_string "----------------------------------------------------------"; print_newline ();
    print_newline ();
    print_string "Temps optimal (s): "; print_int tmps_opt; print_newline ();
    print_string "Temps dans le circuit (s): "; print_int tmps_circ; print_newline ();
    print_string "Vitesse actuelle (m/s): "; print_int vit_act; print_newline ();
    print_string "Vitesse moyenne (m/s): "; print_int vit_moy; print_newline ();
    print_string "Station de départ: "; print_int stat_dep; print_newline ();
    print_string "Station d'arrivé: "; print_int stat_arr; print_newline ();
    print_string "=========================================================="; print_newline ();
;;

(* Implementaiton du graph à 3 sommets. *)
let graph = Array.make_matrix 3 3 (D(-1)) ;;

let v1_2 = V((dist_to_int (get_distance graph 1 2))/v_max,0,0,0,1,2);;
let v2_3 = V((dist_to_int (get_distance graph 2 3))/v_max,0,0,0,2,3);;
let v1_3 = V((dist_to_int (get_distance graph 1 3))/v_max,0,0,0,1,3);;
let v3_1 = V((dist_to_int (get_distance graph 3 1))/v_max,0,0,0,3,1);;

let car_array = [| v1_2; v2_3; v1_3; v3_1 |];;

(* On calcule chaque itération à un intervale de dt *)
let sim_terminate_on_exit_no_entry graph car_array dt stop_time =
  (* C'est la fonction principale, elle s'arrête quand il n'y a plus de voitures dans le tableau car_array. A chaque ittération, elle met à jour les paramètres de chaqu'une des voitures et les transforme en pads si beusion est. *)
  (* Les valeurs de chaque voiture sont stockés dans 6 collones de la matrice data. Pour ne pas avoir de problèmes de redimensionnement, on prend comme dimension la somme des temps optimaux * 2 divisé par dt. L'hypothèse est que nous ne pouvons pas aire pire que 2*le tmps opt.*)

  let get_dimensions num_cars car_array =
    let rec aux acc i =
      if i < num_cars then
        match car_array.(i) with
        | P(vit_act, dist_p, stat_p) -> aux acc (i+1)
        | V(tmps_opt, _, _, _, _, _) -> aux (acc + tmps_opt) (i+1)
      else acc
    in 2*(aux 0 0)
  in

  let num_cars = Array.length car_array in
  let stop_array = Array.make num_cars 0 in (* Tableau qui contient le # de tours que un vehicule s'arrête. *)
  let num_stations = Array.length graph in
  let dim = get_dimensions num_cars car_array in
  let data = Array.make_matrix dim (6*num_cars + 1) 0 in (* 6 colonnes pour les voitures et 1 pour le # d'ittération. Cette approche est très coûteuse en mémoire vive car il faut garder en mémoire toutes les données. Il faut l'optimiser ou faire un data dump après n opérations, n une valeur à trouver empiriquement. *)

  let terminate_loop =
    let rec aux acc i =
      if (i < num_cars)&&acc then
        match car_array.(i) with
        | P(_,_,_) -> aux (true&&acc) (i+1)
        | V(_, _, _, _, _, _) -> aux (false&&acc) (i+1)
      else acc
    in aux true 0
  in

  let update_data iter =
    data.(iter).(6*num_cars) <- 0;
    for i=0 to (num_cars - 1) do
      match car_array.(i) with
      | P(vit_act, dist_p, stat_p) ->
        data.(iter).(6*i) <- vit_act;
        data.(iter).(6*i+1) <- (dist_to_int dist_p);
        data.(iter).(6*i+2) <- stat_p;
        data.(iter).(6*i+3) <- (-1);
        data.(iter).(6*i+4) <- (-1);
        data.(iter).(6*i+5) <- (-1);
      | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
        data.(iter).(6*i) <- tmps_opt;
        data.(iter).(6*i+1) <- tmps_circ;
        data.(iter).(6*i+2) <- vit_act;
        data.(iter).(6*i+3) <- vit_moy;
        data.(iter).(6*i+4) <- stat_dep;
        data.(iter).(6*i+5) <- stat_arr;
    done;
  in

  let write_data file =
    (* Write message to file *)
    let oc = open_out file in
    (* create or truncate file, return channel *)
    Printf.fprintf oc "%s\\n" message;
    for car = 0 to num_cars do

    for iter = 0 to dim do      (* Ittering over each line *)

    close_out oc;

  let rec loop n =
    if not terminate_loop then
      begin
        (* On met à jour les données de chaque voiture. *)
        for i=0 to (num_cars-1) do
          match car_array.(i) with
          | P(vit_act, dist_p, stat_p) ->
            let speed = speed_control graph i car_array dt in
            let dist = dist_p -- (D(speed*dt)) in (* Ici interviennent les défauts de la discretisation, on considére que la voiture arrive à la vitesse demandé instantanement. C'est pour ça que le speed control doit être bein fait.  *)
            if (dist_to_int dist) = 0 then
              let next_station = get_next_station i num_stations in
              car_array.(i) <- P(speed, dist, next_station);
            else
              let next_station = stat_p in
              car_array.(i) <- P(speed, dist, next_station);
          | V(tmps_opt, tmps_circ, vit_act, vit_moy, stat_dep, stat_arr) ->
            let dist_to_dest = get_dist_to_dest_idx graph i car_array in
            if ((dist_to_int dist_to_dest) = 10)&&(stop_array.(i) <> (-1) ) then (* stop array = -1 si la voiture s'est déjà arrêté. Attention aux erreurs de discretisation, il faudrait peut être un intervalle car la voiture pourrait ne pas s'arrêter exactement à la station *)
              begin
                if stop_array.(i) = 0 then (* Si la voiture ne s'est pas encore arrêté. *)
                  begin
                    stop_array.(i) <- stop_time / dt;
                    let speed = 0 in
                    car_array.(i) <- V(tmps_opt, tmps_circ+dt, 0, vit_moy*n/(n+1) + (fast_exp 2 (n-1))*speed/n*(n+1), stat_dep, stat_arr);
                  end
                else if stop_array.(i) = (-1) then (* Si la voiture doit repartir *)
                  begin
                    stop_array.(i) <- 0;
                    let speed = speed_control graph i car_array dt in
                    car_array.(i) <- V(tmps_opt, tmps_circ+dt, speed, vit_moy*n/(n+1) + (fast_exp 2 (n-1))*speed/n*(n+1), stat_dep, stat_arr)
                  end
                else if stop_array.(i) = 1 then             (* Si la voiture est à l'arrêt mais va reparit la prochaine ittération, la voiture se transforme en pad. *)
                  begin
                    stop_array.(i) <- (-1);
                    let speed = 0 in
                    let next_station = get_next_station stat_arr num_stations in
                    let distance = get_distance graph stat_arr next_station in
                    car_array.(i) <- P(speed, distance, next_station);
                  end
                else              (* Si la voiture est à l'arrêt mais ne va pas reparit la prochaine ittération *)
                  begin
                    stop_array.(i) <- (stop_array.(i) - 1);
                    let speed = 0 in
                    car_array.(i) <- V(tmps_opt, tmps_circ+dt, 0, vit_moy*n/(n+1) + (fast_exp 2 (n-1))*speed/n*(n+1), stat_dep, stat_arr)
                  end
              end
        done;
      end
    else
      



graph;;
disp_car v2_3;;

o
