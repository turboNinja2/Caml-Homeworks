let n_reines n =

let nn = ref 1 in
for i = 1 to n do nn := !nn * n done; (*C�est n^n, le nombre de facons diff�rentes de placer n reines sur un �chiquier de taille n*)

(*Affiche l��chiquier*)

let disp tab = 
	for k = 0 to vect_length tab - 1 do
		for i = 0 to vect_length tab - 1 do
			print_int tab.(i).(k);
		done;
	print_newline();
	done;
print_newline();
in

(*On d�finit maintenant une proc�dure qui �crit un nombre inf�rieur � n^n en base n puis qui le converti sous la forme d�une matrice, o� l�ordonn�e correspond au coefficient devant le terme en 4^i, ou i est l�abscisse�;
0100
0001
1000
0010, cette matrice repr�sente par exemple 1*4^0 + 3*4^1 + 0*4^2 + 3*4^3 *)


let nbase a n = 
	let a = ref a in
	let cong = ref 0 in
	let matrice = make_vect n [||] in
	for i = 0 to n-1 do matrice.(i) <- make_vect n 0; done;
		for i = 1 to n do 
			cong := !a mod n;
			matrice.(i-1).(!cong) <- 1 ;
			a := (!a - !cong)/n;
		done;
matrice
in

(*On v�rifie maintenant que le tableau obtenu � l�aide de la proc�dure pr�c�dente est bon*)

let is_good tab =
let global = ref 0 in
let i = ref 0 in
while (!i < n && !global = 0) do
	let indic = ref 0 in
	for j = 0 to n - 1 do
		if tab.(!i).(j) = 1 then incr indic;
		if tab.(j).(!i) = 1 then incr indic;
	done;
	incr i;
	if !indic > 2 then incr global;
done;
let i = ref 0 in
while (!i < n && !global = 0) do
	let indic = ref 0 in
	for j = 0 to n - 1 - !i do
		if tab.(j).(j + !i) = 1 then incr indic;
	done;
	incr i;
	if !indic > 1 then incr global;
done;
let i = ref 0 in	
while (!i < n && !global = 0) do
	let indic = ref 0 in
	for j = 0 to n - 1 - !i do
		if tab.(j+ !i).(j) = 1 then incr indic;
	done;
	incr i;
	if !indic > 1 then incr global;
done;
let i = ref 0 in
while (!i < n && !global = 0) do
	let indic = ref 0 in
	for j = 0 to n - 1 - !i do
		if tab.(n-1- !i-j).(j) = 1 then incr indic;
	done;
	incr i;
	if !indic > 1 then incr global;
done;
let i = ref 0 in
while (!i < n && !global = 0) do
	let indic = ref 0 in
	for j = 0 to n - 1 - !i do
		if tab.(!i+j).(n-1-j) = 1 then incr indic;
	done;
	if !indic > 1 then incr global;
	incr i;
done;
if !global = 0 then true else false
in

let nb_solutions = ref 0 in
for i = n to !nn do
	let dames = nbase i n in
	let b = is_good dames in
	if b then disp dames;
	if b then incr nb_solutions;
done;
let nb_solutions = !nb_solutions in
print_string "Il y a "^nb_solutions^" solutions.";
;;





let cavalier n =

let n2 = (n*n) in
let chemin = Array.make n2 0 in 

(*C'est le tableau qui va stocker les deplacements du cavalier sous la forme d'un tableau de taille n� contenant des nombres de 0 � 7 (le cavalier peut 
effectuer 8 d�placements diff�rents au maximum quelle que soit la taille de l'�chiquier) qui seront ensuite convertis en d�placement du cavalier*) 

let maxi = ref 1 in
for i = 1 to n2 do 
maxi:=!maxi*8;
done;

let maxi = !maxi in (*C'est le nombre maximum de chemins diff�rents que pourra emprunter le cavalier : 8^n2*)
print_int maxi;

(*On construit maintenant l'echiquier sous la forme d'une matrice qui ne contient au d�part que des 0*)

let echiquier = make_vect n [||] in
for i = 0 to n-1 do
echiquier.(i) <- make_vect n 0;
done;

let reset echiquier =
for i = 0 to n-1 do
	for j = 0 to n-1 do
		echiquier.(i).(j)<-0;
	done;
done;
echiquier
in	

(*On �crit une proc�dure qui affiche l'echiquier*)

let disp tab = 
for k = 0 to vect_length tab - 1 do
	for i = 0 to vect_length tab - 1 do
		print_int tab.(i).(k);
	done;
	print_newline();
	done;
print_newline();
in

(*Cette fonction va permettre au cavalier de parcourir toutes les cases de l'�chiquier lorsque l'arguement r varie de 0 � n�-1*)

let position r n = 
let a =r/n and b = r mod n in
[|a;b|]
in

(*On �crit maintenant une proc�dure qui permet de passer d'un chemin au suivant, elle correspond � additionner 1 � un nombre �crit en base n*)

let chemin_suivant tab =
tab.(0)<-tab.(0)+1;
for i = 0 to vect_length tab - 1 do
if tab.(i)>(n-1) then begin tab.(i+1)<-(tab.(i+1))+1 ; tab.(i)<-(tab.(i))-(n-1) end (*On ne risque pas de sortir du tableau car on va s'arr�ter � maxi-1*)
done;
tab
in

(*On d�finit ensuite la proc�dure qui, � partir d'un �l�ment de la liste "chemin" va renvoyer un d�placement possible du cavalier*)

let convert p = 
if p=0 then [|2;1|]
else if p=1 then [|2;-1|]
else if p=2 then [|1;2|]
else if p=3 then [|1;-2|]
else if p=4 then [|-1;2|]
else if p=5 then [|-1;-2|]
else if p=6 then [|-2;1|]
else [|-2;-1|]
in

(*On permet maintenant au cavalier de position "pos" de se d�place de "tab", ou "tab" est un �l�ment de "chemin" qui a �t� converti en d�placement du cavalier*)

let deplace_cav tab pos =
pos.(0)<-pos.(0)+tab.(0);
pos.(1)<-pos.(1)+tab.(1);
pos
in

(*On d�finit la proc�dure suivante, qui renvoie "true" si le cavalier est sur l'�chiquier, faux s'il en est sorti*)

let is_good cav =
if (cav.(0)>n-1 or cav.(0)<0) then false
else if (cav.(1)>n-1 or cav.(1)<0) then false
else true
in

for t = 0 to n2-1 do (* t va permettre au cavalier de parcourir toutes les positions initiales de l'�chiquier*)
	let cav = position t n in
	for j = 0 to maxi-1 do (*j va permettre de parcourir toutes les listes "chemins" possibles pour l'echiquier*)
		let echiquier = reset echiquier in
		let k = ref 1 and cav = position t n in
		let a = cav.(0) and b = cav.(1) in
		echiquier.(a).(b) <- 1 ;
		while ((!k <= n2-1) && is_good cav) do
			let mouvement = convert chemin.(!k) in
			let cav = deplace_cav mouvement cav in
			let a = cav.(0) and b = cav.(1) in
			incr k; 
			if ( is_good cav && echiquier.(a).(b)=0 ) 
				then echiquier.(a).(b) <- !k
				else let k = ref (n2+1) in (*Ceci permet de sortir direcement du while et de tester un autre chemin *)
			if (!k = n2) then disp echiquier;
		done;
		let chemin = chemin_suivant chemin in
		print_string"";
	done; 
done;
;;

(*Ce programme n�est cependant pas utilisable car la valeur maxi est trop grande d�s que n d�passe 4�.*)