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
