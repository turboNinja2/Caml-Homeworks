(*1-Questions préliminaires*)

let affiche_vect data =
	let n = (vect_length data) - 1 in
	for i = 1 to n do 
		print_int data.(i);
		print_string" ";
	done;
	print_newline();
;;

let affiche_rev data =
	let n = (vect_length data) - 1 in
	for i = n downto 0 do 
		if data.(i) = 0 then () else begin print_int data.(i); print_string" " end;
	done;
	print_newline();
;;

(*2-Enumeration des partitions*)

let reset data z = 
for i = z to vect_length data - 1 do data.(i)<-0; 
done;;

let partitions_m m n =
	let data = make_vect (n+1) 0 in
	data.(0)<-m;
	let n0 = n in
	let rec part_m n k =
		if k < n0 then ( reset data (k+1);
			for i = data.(k) downto 1 do (*C'est pour cette ligne, qui assure la décroissance des partitions, que data.(0) doit contenir m*)
				data.(k+1)<-i;
				if (n-i = 0) then affiche_vect data
				else if n-i < 0 then () 
				else (part_m (n-i) (succ k) );
			done; )
		else()
	in part_m n 0 ;;

partitions_m 3 7;;

let partitions n =
	partitions_m n n
;;

partitions 6;;

(*3-Dénombrement*)

let p_n n= 
	let rec p = function
	|(0,_) -> 1
	|(_,0) -> 0	
	|(n,m) -> if n>=m 
	  then p (n,m-1) + p (n-m, m)
	  else p (n,n)
in p (n,n);;

p_n 6;;

(*4-Enumérations particulières*)

let rec part_filtre n n0 k data filtre =
if k < n0 then ( reset data (k+1);
	for i = (data.(k)-1) downto 1 do  	
		data.(k+1)<-i;
		if (n-i = 0 && (filtre (data))) then (affiche_vect data; data.(k+1)<-0)
		else if n-i < 0 then () 
		else part_filtre (n-i) n0 (succ k) data filtre;
	done; )
else()
;;

let partitions_distinctes n =
	let datatrue data = true in (*C'est la fonction filtre qui ne sert à rien*)
	let data = make_vect (n+1) 0 in
	data.(0)<-(n+1);
	part_filtre n n 0 data datatrue
;;

partitions_distinctes 11;;

let impairs data =
	let l = vect_length data - 1 in
	let bool = ref true in
	for i = 1 to l do
		if data.(i) = 0 then () else bool:=((data.(i) land 1 = 1) && !bool);
	done;
!bool
;;

let partitions_distinctes_impaires n =
	let data = make_vect (n+1) 0 in
	data.(0)<-(n+1);
	part_filtre n n 0 data impairs ;
;;

partitions_distinctes_impaires 11;;

(*5-Application : Comment rendre la monnaie*)

let parts_dans_w n w =
	let wlength = vect_length w in
	let data = make_vect (n+1) 0 in
	let n0 = n in
	let rec part_w n k antecedant =
	if k < n0 then reset data (k+1); match n with
		|0->affiche_vect data;
		|_->for i = antecedant to wlength-1 do (*Cette ligne fait appel au fait que w soit supposé trié par ordre décroissant*)
			data.(k)<-w.(i);
			if (n-w.(i)) >= 0 then part_w (n-w.(i)) (succ k) i else() done;
in part_w n 1 0;
;;

parts_dans_w 11 [|10;5;2;1|];;

let monnaie w n =
let data = make_vect (n+1) 0 in
let wlength = vect_length w in
let wa = make_vect wlength 0 and wb = make_vect wlength 0 in
let n0 = n in
for i = 0 to wlength - 1 do
match w.(i) with (a,b) -> wa.(i)<-a; wb.(i)<-b;
done; (*On projette le vecteur w sur wa et wb.*)
let rec part_m n k antecedant =
	if k < n0 then begin reset data k; match n with
		|0->affiche_vect data;
		|_->for i = antecedant to wlength-1 do
			match (wa.(i), wb.(i)>0) with
				|(a,true)->(data.(k)<-wa.(i);wb.(i)<-wb.(i)-1; if n >= 0 then part_m (n-a) (succ k) i;wb.(i)<-wb.(i)+1;)
				|(_,false)->();
			done; end
in part_m n 1 0;;			
			
monnaie [|(10,2);(5,3);(2,1);(1,3)|] 17;;

(*
affiche_vect : int vect -> unit = <fun>
#affiche_rev : int vect -> unit = <fun>
#reset : int vect -> int -> unit = <fun>
#partitions_m : int -> int -> unit = <fun>
#3 3 1 0 0 0 0 
3 2 2 0 0 0 0 
3 2 1 1 0 0 0 
3 1 1 1 1 0 0 
2 2 2 1 0 0 0 
2 2 1 1 1 0 0 
2 1 1 1 1 1 0 
1 1 1 1 1 1 1 
- : unit = ()
#partitions : int -> unit = <fun>
#6 0 0 0 0 0 
5 1 0 0 0 0 
4 2 0 0 0 0 
4 1 1 0 0 0 
3 3 0 0 0 0 
3 2 1 0 0 0 
3 1 1 1 0 0 
2 2 2 0 0 0 
2 2 1 1 0 0 
2 1 1 1 1 0 
1 1 1 1 1 1 
- : unit = ()
#p_n : int -> int = <fun>
#- : int = 11
#part_filtre : int -> int -> int -> int vect -> (int vect -> bool) -> unit =
 <fun>
#partitions_distinctes : int -> unit = <fun>
#11 0 0 0 0 0 0 0 0 0 0 
10 1 0 0 0 0 0 0 0 0 0 
9 2 0 0 0 0 0 0 0 0 0 
8 3 0 0 0 0 0 0 0 0 0 
8 2 1 0 0 0 0 0 0 0 0 
7 4 0 0 0 0 0 0 0 0 0 
7 3 1 0 0 0 0 0 0 0 0 
6 5 0 0 0 0 0 0 0 0 0 
6 4 1 0 0 0 0 0 0 0 0 
6 3 2 0 0 0 0 0 0 0 0 
5 4 2 0 0 0 0 0 0 0 0 
5 3 2 1 0 0 0 0 0 0 0 
- : unit = ()
#impairs : int vect -> bool = <fun>
#partitions_distinctes_impaires : int -> unit = <fun>
#11 0 0 0 0 0 0 0 0 0 0 
7 3 1 0 0 0 0 0 0 0 0 
- : unit = ()
#parts_dans_w : int -> int vect -> unit = <fun>
#10 1 0 0 0 0 0 0 0 0 0 
5 5 1 0 0 0 0 0 0 0 0 
5 2 2 2 0 0 0 0 0 0 0 
5 2 2 1 1 0 0 0 0 0 0 
5 2 1 1 1 1 0 0 0 0 0 
5 1 1 1 1 1 1 0 0 0 0 
2 2 2 2 2 1 0 0 0 0 0 
2 2 2 2 1 1 1 0 0 0 0 
2 2 2 1 1 1 1 1 0 0 0 
2 2 1 1 1 1 1 1 1 0 0 
2 1 1 1 1 1 1 1 1 1 0 
1 1 1 1 1 1 1 1 1 1 1 
- : unit = ()
#monnaie : (int * int) vect -> int -> unit = <fun>
#10 5 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
10 5 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 
5 5 5 2 0 0 0 0 0 0 0 0 0 0 0 0 0 
5 5 5 1 1 0 0 0 0 0 0 0 0 0 0 0 0 
- : unit = ()
*)
