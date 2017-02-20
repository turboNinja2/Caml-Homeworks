open Plplot
module P = Plot

let max_length = 1. ;;

(* Toy data *)
let chessboard_boundary x y = if ((mod_float (x+.10.)  0.5) -. 0.25) *. ((mod_float (y+.10.) 0.5) -. 0.25) > 0. then 1. else ~-.1. ;;

let pi = atan 1.0 *. 4.0 ;;

let xpts = 100 ;;              (* Data points in x *)
let ypts = 100 ;;             (* Data points in y *)

let alt = [|60.0; 20.0|];;


let az = [|30.0; 60.0|];;

let title = "Original decision boundary";;


let cmap1_init gray =
  let i = [|0.0; 1.0|] in (* left and right boundaries *)

  let h, l, s =
    if gray then (
      [|0.0; 0.0|], (* hue -- low: red (arbitrary if s=0) *)
                    (* hue -- high: red (arbitrary if s=0) *)
      [|0.5; 1.0|], (* lightness -- low: half-dark *)
                    (* lightness -- high: light *)
      [|0.0; 0.0|]  (* minimum saturation *)
                    (* minimum saturation *)
    )
    else (
      [|240.0; 0.0|], (* blue -> green -> yellow -> *)
                      (* -> red *)
      [|0.6; 0.6|],
      [|0.8; 0.8|]
    )
  in

  plscmap1n 256;
  plscmap1l false i h l s None;
  ();;

let levels = 10;;

(*--------------------------------------------------------------------------*\
 * Does a series of 3-d plots for a given data set, with different
 * viewing options in each plot.
\*--------------------------------------------------------------------------*)
let () =
  let nlevel = levels in

  (* Allocate data structures *)
  let z = Array.make_matrix xpts ypts 0.0 in

  let x =
    Array.init xpts (
      fun i -> (float_of_int (i - (xpts / 2)) /. float_of_int (xpts / 2)) 
    ) in

  let y =
    Array.init ypts (
      fun i -> float_of_int (i - (ypts / 2)) /. float_of_int (ypts / 2)
    ) in

  for i = 0 to xpts - 1 do
    let xx = x.(i) in
    for j = 0 to ypts - 1 do
      let yy = y.(j) in
      z.(i).(j) <- chessboard_boundary xx yy ;
    done
  done;


  let zmax, zmin = plMinMax2dGrid z in
  let step = (zmax -. zmin) /. float_of_int (nlevel + 1) in
  let clevel =
    Array.init nlevel (fun i -> zmin +. step +. step *. float_of_int i)
  in

  pllightsource 1.0 1.0 1.0;

    for ifshade = 3 to 3 do
      plinit (); (* Initialize plplot *)

      pladv 0;
      plvpor 0.0 1.0 0.0 0.9;
      plwind (-1.0) 1.0 (-0.9) 1.1;
      plcol0 3;
      plmtex "t" 1.0 0.5 0.5 title;
      plcol0 1;
      plw3d 1.0 1.0 1.0 (-1.0) 1.0 (-1.0) 1.0 zmin zmax alt.(0) az.(0);

      plbox3 "bnstu" "x axis" 0.0 0
             "bnstu" "y axis" 0.0 0
             "bcdmnstuv" "z axis" 0.0 0;
      plcol0 2;

      match ifshade with
          0 -> (* diffuse light surface plot *)
            cmap1_init true;
            plsurf3d x y z  [PL_DIFFUSE] [||];
        | 1 -> (* magnitude colored plot *)
            cmap1_init false;
            plsurf3d x y z [PL_MAG_COLOR] [||];
        | 2 -> (* magnitude colored plot with faceted
                  squares *)
            cmap1_init false;
            plsurf3d x y z [PL_MAG_COLOR; PL_FACETED] [||];
        | _ -> (* magnitude colored plot with contours *)
            cmap1_init false;
            plsurf3d x y z [PL_MAG_COLOR; PL_SURF_CONT; PL_BASE_CONT] clevel;
      plend (); (* Clean up *)
    done;
  ()

