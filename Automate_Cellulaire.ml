(*on cré un mapping de notre surface tel qu'en chaque case:
- (0,0,0,0,0)= vide
- (1, vitesse, sortie, stress, zone)= humain
- (2, x_min, x_max, y_min, y_max)=obstacle
- (3, x_min, x_max, y_min, y_max)=sortie
- (4, x_min, x_max, y_min, y_max)= porte

On conserve dans  un autre tableau les positions des humains, et celles des sorties*)
(* Implémentation de tableaux des sorties:
sorties.(zone)=[|(x, y, zone à laquelle mène la sortie)...|]
Implémentation des zones:
zones=[|([|x_min, x_max, y_min, y_max|], n° de la zone);...|]
*)

(*ajouts suivants: obstacle rond, triangle*)
open Random;;
#load "unix.cma";;
open Unix;;
#load "graphics.cma";;
open Graphics;;

let dimx=130;;
let dimy=130;;

let distance a b x y= sqrt(float_of_int((a-x)*(a-x)+(b-y)*(b-y)));;

let obstacle_valide x y mapping largeur=
    (* ici, on fait une vérification minimale, les obstacles peuvent donc se superposer*)
    let okay= ref true and increment= ref 0 in
    while !increment <> largeur do
        okay:= (!okay)&&(mapping.(x).(y + !increment)=(0,0,0,0,0))&&
            (mapping.(x+ !increment).(y )=(0,0,0,0,0))&&
            (mapping.(x+ largeur-1).(y + !increment)=(0,0,0,0,0))&&
            (mapping.(x+ !increment).(y + largeur-1)=(0,0,0,0,0));
        incr increment;
    done; !okay;;
    
let genere_obstacle_aleatoire nombre mapping x_min x_max y_min y_max=
    let obstacles=Array.make nombre (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    let cote_max= 2*(min (x_max-x_min) (y_max-y_min))/nombre in
    for i=0 to nombre-1 do 
        let largeur= ref (int(cote_max) +1) in (* on enregistre le coin en bas à gauche*)
        let x_begin= ref ((int((x_max-x_min)- !largeur)) +x_min) and 
            y_begin= ref ((int ((y_max-y_min)- !largeur)) + y_min) in
        while not (obstacle_valide (!x_begin) (!y_begin) mapping (!largeur)) do
            (* tant qu'il y a collision, on génére un autre obstacle et on le teste*)
            largeur:=(int(cote_max) +1);
            x_begin:=((int((x_max-x_min)- !largeur)) +x_min);
            y_begin:=((int ((y_max-y_min)- !largeur)) + y_min);
        done; (* on remplit la map avec les informations de l'obstacle*)
        for i= !x_begin to !x_begin+ !largeur -1 do
            for j= !y_begin to !y_begin+ !largeur -1 do
                mapping.(i).(j)<-(2, !x_begin,!x_begin + !largeur -1 , 
                    !y_begin, !y_begin + !largeur -1)
            done
        done; obstacles.(i)<-(!x_begin, !y_begin);
    done; obstacles;;

let genere_obstacle x_min x_max y_min y_max mapping=
    for i=x_min to x_max do
        for j=y_min to y_max do
            mapping.(i).(j)<-(2,x_min, x_max, y_min, y_max)
        done
    done;;
		
let genere_salle_classe_haut mapping x_min x_max y_min y_max =
    let une_unite_x= 4 + 1 in
    let une_unite_y= 2 + 1 in
    let nx=((x_max-x_min-4)/une_unite_x) and ny=((y_max-y_min-4)/une_unite_y) in
    let obstacles=Array.make (nx*ny) (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    for x=0 to nx-1 do 
        for y=0 to ny-1 do
            let deb_x=x_min+ 2 +x*une_unite_x and fin_x=x_min+ 2 +x*une_unite_x +4 in
            let deb_y=y_min+ 2 +y*une_unite_y and fin_y=y_min+ 2 +y*une_unite_y+ 2 in
            genere_obstacle deb_x (fin_x -1) deb_y (fin_y-1) mapping;
            obstacles.(((y_max-y_min-4)/une_unite_y)*x + y)<-(deb_x, deb_y);
        done
    done;
    obstacles;;

let genere_salle_classe_droite mapping x_min x_max y_min y_max =
    let une_unite_x= 2 + 1 and une_unite_y= 4 + 1 in
    let nx=((x_max-x_min-4)/une_unite_x) and ny=((y_max-y_min-4)/une_unite_y) in
    let obstacles=Array.make (nx*ny) (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    for x=0 to nx-1 do 
        for y=0 to ny-1 do 
            let deb_x=x_min+ 2 +x*une_unite_x and fin_x=x_min+ 2 +x*une_unite_x +2 in
            let deb_y=y_min+ 2 +y*une_unite_y and fin_y=y_min+ 2 +y*une_unite_y+ 4 in
            genere_obstacle deb_x (fin_x -1) deb_y (fin_y-1) mapping;
            obstacles.(((y_max-y_min-4)/une_unite_y)*x + y)<-(deb_x, deb_y);
        done
    done;
    obstacles;;
	
let amphitheatre_haut mapping x_min x_max y_min y_max =
    let une_unite_x= 2 + 1 and largeur_y=(y_max-y_min -6)/2 in
    let obstacles=Array.make (((x_max-x_min-4)/une_unite_x)*2) (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    for x=0 to ((x_max-x_min-4)/une_unite_x)-1 do
        let deb_x=x_min+ 2 +x*une_unite_x and fin_x=x_min+ 2 +x*une_unite_x +2 in
        genere_obstacle deb_x (fin_x -1) (y_min+2) (y_min+2+largeur_y) mapping;
        obstacles.(2*x )<-(deb_x, y_min+2);
        genere_obstacle deb_x (fin_x -1) (y_max-2 -largeur_y) (y_max-2) mapping;
        obstacles.(2*x +1)<-(deb_x, y_max-2 -largeur_y);
    done;
    obstacles;;
    
let amphitheatre_droite mapping x_min x_max y_min y_max =
    let une_unite_y= 2 + 1  and largeur_x=(x_max-x_min -6)/2 in
    let obstacles=Array.make (((y_max-y_min-4)/une_unite_y)*2) (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    for y=0 to ((y_max-y_min-4)/une_unite_y)-1 do
        let deb_y=y_min+ 2 +y*une_unite_y and fin_y=y_min+ 2 +y*une_unite_y +2 in
        genere_obstacle (x_min+2) (x_min+2+largeur_x) deb_y (fin_y -1) mapping;
        obstacles.(2*y )<-( x_min+2,deb_y);
        genere_obstacle (x_max-2 -largeur_x) (x_max-2) deb_y (fin_y-1) mapping;
        obstacles.(2*y +1)<-(x_max-2 -largeur_x,deb_y);
    done;
    obstacles;;

let cantine_haut mapping x_min x_max y_min y_max =
    let une_unite_x= 4 + 1 and largeur_y=(y_max-y_min -6)/2 in
    let obstacles=Array.make (((x_max-x_min-4)/une_unite_x)*2) (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    for x=0 to ((x_max-x_min-4)/une_unite_x)-1 do 
        let deb_x=x_min+ 2 +x*une_unite_x and fin_x=x_min+ 2 +x*une_unite_x +2 in
        genere_obstacle deb_x (fin_x -1) (y_min+2) (y_min+2+largeur_y) mapping;
        obstacles.(2*x )<-(deb_x, y_min+2);
        genere_obstacle deb_x (fin_x -1) (y_max-2 -largeur_y) (y_max-2) mapping;
        obstacles.(2*x +1)<-(deb_x, y_max-2 -largeur_y);
    done;
    obstacles;;
    
let cantine_droite mapping x_min x_max y_min y_max =
    let une_unite_y= 4 + 1 and largeur_x=(x_max-x_min -6)/2 in
    let obstacles=Array.make (((y_max-y_min-4)/une_unite_y)*2) (0,0) in
    (* on définit une largeur maximale afin de pouvoir générer tous nos obstacles*)
    for y=0 to ((y_max-y_min-4)/une_unite_y)-1 do
        let deb_y=y_min+ 2 +y*une_unite_y and fin_y=y_min+ 2 +y*une_unite_y +2 in
        genere_obstacle (x_min+2) (x_min+2+largeur_x) deb_y (fin_y -1) mapping;
        obstacles.(2*y )<-(x_min+2,deb_y);
        genere_obstacle (x_max-2 -largeur_x) (x_max-2) deb_y (fin_y -1) mapping;
        obstacles.(2*y +1)<-( x_max-2 -largeur_x,deb_y);
    done;
    obstacles;;

let genere_obstacle_cercle mapping centre rayon=
    for x= max 0 (centre.(0)-rayon) to min (dimx-1) (centre.(0)+ rayon) do
        for y=max 0 (centre.(1)-rayon) to min (dimx-1) (centre.(1)+ rayon) do
            if int_of_float(distance x y centre.(0) centre.(1)) <=rayon then 
                mapping.(x).(y)<- (2, centre.(0), centre.(1), rayon, -3)
        done
    done;
    (centre.(0), centre.(1));;
    
let genere_mur vecteur debut repetitions mapping=
(* on suppose le vecteur de norme 1*)
    let fin=Array.make 2 (debut.(0)) in
    fin.(0)<- debut.(0)+. vecteur.(0)*.(float_of_int repetitions);
    fin.(1)<- debut.(1)+. vecteur.(1)*.(float_of_int repetitions);
    let x_max= ref 0 and x_min=ref 0 and y_min=ref 0 and y_max= ref 0 in
    if debut.(0)<fin.(0) then begin
        x_max:= int_of_float fin.(0);
        x_min:= int_of_float debut.(0);
    end
    else begin
        x_min:= int_of_float fin.(0);
        x_max:= int_of_float debut.(0);
    end;
    if debut.(1)<fin.(1) then begin
        y_max:=int_of_float fin.(1);
        y_min:=int_of_float debut.(1);
    end
    else begin
        y_min:=int_of_float fin.(1);
        y_max:=int_of_float debut.(1);
    end;
    let v=(2, !x_min, !x_max, !y_min, !y_max) in
    for i=(max 0 (!x_min)) to (min (dimx-1) (!x_max)) do
        for j= (max 0 (!y_min)) to (min (dimy-1) (!y_max)) do
            mapping.(i).(j)<-v
        done;
    done;
    (!x_min, !y_min);;
    
let find_zone x y zones=
    let n= Array.length zones and i= ref 0 and found= ref false and zone_trouvee= ref 0 in
    while !i<n && not (!found) do
        let z, nb=zones.( (!i)) in
        if (z.(0)<=x && z.(1)>=x) && (z.(2)<=y && z.(3)>=y) then begin
            found:= true;
            zone_trouvee:= nb;
        end;
        incr i;
    done;
    !zone_trouvee;;
	
let genere_population nombre mapping zones=
    let humains=Array.make nombre (0,0, (0,0,0,0,0)) in
    for i=0 to nombre-1 do
        (* on prend une position aléatoire*)
        let x= ref (int(dimx)) and y= ref (int(dimy)) in
        (* tant que notre position tirée n'st pas une case vide, on réeffectue un tirage*)
        while mapping.(!x).(!y)<>(0,0,0,0,0) do
            x:=int(dimx);
            y:=int(dimy);
        done;
        (* on assigne des vitesses, correspondant au nombre de cases parcourue lors d'un 
        avancement, et une propensio au stress aléatoire*)
        let z= find_zone (!x) (!y) zones in
        mapping.(!x).(!y)<-(1, 3+int(3), 0, int(5)+1, z);
        humains.(i)<- (!x, !y, (0,0,0,0,0));
    done;humains;;
    
let genere_humain mapping zones x  y=
    let z=find_zone x y zones in mapping.(x).(y)<-(1,  3+int(3), 0, int(5)+1, z);
    (x,y,(0,0,0,0,0));;
	
let genere_sortie x_max x_min y_max y_min mapping zone_finale=
    let v=(3, x_min, x_max, y_min, y_max) in
    for x=x_min to x_max do
        for y=y_min to y_max do
            mapping.(x).(y)<-v
        done
    done;
    (x_min, y_min, zone_finale);;

let genere_4_sorties mapping zone_finale=
(* tirage de sortie aléatoires simpliste*)
    let tab= Array.make 4 (0,0, 0) and largeur=(int(dimx/2 -2) +2) in 
    let x_begin= int(dimx - largeur) and y_begin= int(dimy - largeur) in 
    for i= x_begin to x_begin + largeur -1 do 
        mapping.(i).(0)<-(3, x_begin, x_begin + largeur -1, 0,0);
        mapping.(i).(129)<-(3, x_begin, x_begin + largeur -1, 129,129);
    done;
    tab.(0)<-(x_begin,0, -1);
    tab.(1)<- (x_begin, 129, -1);
    for j= y_begin to y_begin + largeur -1 do
        mapping.(0).(j)<-(3,0,0, y_begin, y_begin + largeur -1);
        mapping.(129).(j)<-(3,129,129,y_begin, y_begin + largeur -1);
    done;
    tab.(2)<-(0, y_begin, -1);
    tab.(3)<- (129, y_begin, -1);
    tab;;
    
let genere_porte x_min x_max y_min y_max mapping=
    for i=x_min to x_max do
        for j=y_min to y_max do
            mapping.(i).(j)<-(4, x_min, x_max, y_min, y_max)
        done
    done;;

let table_hachage_humain humains n=
    let h= Hashtbl.create (dimx*dimy) in
    for i=0 to n-1 do
        let x,y,_= humains.(i) in
        Hashtbl.add h (x,y) i
    done;
    h;;

let melange tableau nb_elt hachage=
    for i=0 to nb_elt-1 do
        let new_ind= int(nb_elt-i) in
        let tmp= tableau.(i) in
        let x1,y1, _=tableau.(i+new_ind) and x2,y2,_=tableau.(i) in
        tableau.(i)<-tableau.(i+new_ind);
        tableau.(i+new_ind)<- tmp;
        Hashtbl.add hachage (x1,y1) i;
        Hashtbl.add hachage (x2,y2) (i+new_ind);
    done;;

let min a b=if a<b then a else b;;

let max a b = if a<b then b else a;;

let rayon_congestion=5;;

let calcule_congestion mapping x y =
    (* on prend un carré et on compte le nombre depersonne à l'intérieur, ce qui 
    nous donnera un certain niveau de congestion autour de la personne*)
    let congestion= ref 0. in
    let x_min = max 0 (x-rayon_congestion) and x_max= min (dimx-1) (x+rayon_congestion) and
    y_min= max 0 (y-rayon_congestion) and y_max= min (dimy-1) (y+ rayon_congestion) in
    let aire= float_of_int( (x_max-x_min)*(y_max-y_min)) in
    for i= x_min to x_max do
        for j= y_min to y_max do
            let type_contenu,_,_,_,_= mapping.(i).(j) in
            (* on considère que les obstacles ajoute également de la congestion dans le 
            sens qu'ils réduisent l'espace libre autour de la personne*)
            if type_contenu=1|| type_contenu=2 then congestion:= !congestion +.1.
        done
    done; (!congestion)/.aire;;

let point_sortie_plus_proche x y sortie=
    (* on cherche le point de la sortie dont la distance à la personne est la plus faible*)
    if((x<sortie.(1)) && (x>sortie.(0))) then x,sortie.(2)
    else if ((y<sortie.(3))&&(y>sortie.(2))) then sortie.(0),y
    else
        if sortie.(0)=sortie.(1) then 
            if y<=sortie.(2) then sortie.(0),sortie.(2)
            else sortie.(0),sortie.(3)
        else
            if x>=sortie.(1) then sortie.(1),sortie.(2)
            else sortie.(0),sortie.(2);;
            
let sortie_la_plus_proche mapping sorties x y nb_sorties=
    (*on parcourt les sorties, et nous cherchons la sortie la plus proche du joueur*)
    let plus_proche= ref 0 and distance_min= ref infinity in
    for i=0 to nb_sorties -1 do
        let _,x_min, x_max, y_min, y_max= sorties.(i) in
        (* on récupère le point de la sortie le plus proche de la personne*)
        let x_s,y_s= point_sortie_plus_proche x y [|x_min; x_max; y_min; y_max|] in
        let d=(distance x y x_s y_s) in
        if d< !distance_min then begin
            plus_proche:=i;
            distance_min:=d;
        end
    done;(!plus_proche);;
	
let assigne_sortie humains mapping nb_humains sorties=
    (*On assigne à chaque joueur sa sortie la plus proche*)
    for i=0 to nb_humains -1 do
        let x,y, _= humains.(i) in
        let a,b,v,e,zone_joueur= mapping.(x).(y) in
        let s_plus_proche= sortie_la_plus_proche mapping sorties.(zone_joueur) x y 
            (Array.length sorties.(zone_joueur)) in
        mapping.(x).(y)<-(a,b, s_plus_proche, e,zone_joueur)
    done;;
	
(*implémentation d'un tri rapide décroissant de 2 tableaux en fonctions de valeurs d'un seul*)
let echanger tableau i j=
    let tmp= tableau.(i) in
    tableau.(i)<-tableau.(j);
    tableau.(j)<-tmp;;
    
let partition tableau tab_ref deb fin=
    let pivot=tableau.(deb) and compteur=ref(deb) in
    for i=(deb+1) to fin do
        if(tableau.(i)>pivot) then begin
            compteur:=(!compteur)+1;
            echanger tableau i (!compteur);
            echanger tab_ref i (!compteur);
        end;
    done;
    echanger tableau (!compteur) deb;
    echanger tab_ref (!compteur) deb;
    (!compteur);;

  
let tri_rapide tableau tab_ref=
    let rec tri_rapide_bis tableau tab_ref deb fin=
        if deb<fin then begin
            let position=(partition tableau tab_ref deb fin) in
            tri_rapide_bis tableau tab_ref deb (position-1);
            tri_rapide_bis tableau tab_ref (position+1) fin;
        end;
    in
    tri_rapide_bis tableau tab_ref 0 (Array.length(tableau)-1);;
    
let distance_a_obstacle x y dir_x dir_y mapping=
    let x_pos= ref x  and y_pos= ref y and type_obj= ref 0 in
    while !x_pos<dimx && !x_pos> -1 && !y_pos<dimy && !y_pos> -1 && !type_obj<>2 do
        let t,_,_,_,_= mapping.(!x_pos).(!y_pos) in
        type_obj:= t;
        x_pos:= !x_pos + dir_x;
        y_pos:= !y_pos + dir_y;
    done; 
    if !type_obj=2 then distance  x y (!x_pos) (!y_pos)
    else (distance  x y (!x_pos) (!y_pos));;

let coeff_attractivite d_porte_o d_porte_new d_sortie_min_o d_sortie_min_new d_obstacle=
    let diff_porte= d_porte_o -. d_porte_new and
    diff_sortie= d_sortie_min_o -. d_sortie_min_new in
    let d_x=float_of_int dimx in
    let delta_obs=(d_x/.75.-.d_obstacle) in
    let coeff_neg=sqrt((d_sortie_min_new/.(1.5*.d_x))*.d_obstacle) and
    coeff_pos=sqrt((2.-.d_sortie_min_new/.(1.5*.d_x))*.delta_obs) in
    if diff_porte<0. && diff_sortie<0. then 5.*.diff_porte*.coeff_neg
    else  if diff_porte <0. then -.5.*.diff_sortie*.diff_porte*.coeff_pos
    else if diff_sortie<0. then 5.*.diff_sortie*.diff_porte*.coeff_neg
    else 5.*.diff_sortie*.diff_porte*.coeff_pos;;
    
let deplacements_possibles x y mapping sortie stress zone_finale zone_joueur zones m_d_sortie=
    (* on affecte à chaque case autour du joueur un certan score qui sera transformé 
    en probabilité*)
    let scores= Array.make 9 0. in
    let somme= ref 0. in
    let distance_actuelle= distance x y sortie.(0) sortie.(1) in
    (* cases autour du joueur*)
    let places=[|(x,y);(x,y-1);(x-1,y-1);(x-1, y);(x-1, y+1);(x+1,y);(x+1,y+1);(x,y+1);
        (x+1,y-1)|] in
    (* calcule de la congestion autour du joueur*)
    let congestion=calcule_congestion mapping x y in
    for i=1 to 8 do
        let a,b=places.(i) in
        (* on récupère la case et on vérifie qu'elle existe dans notre map*)
        if ((a<dimx )&& (b<dimy) && (a> -1 )&&( b> -1)) then begin
            let z_poss= find_zone a b zones in
            if z_poss<= zone_joueur then 
            let d_obstacle=(distance_a_obstacle x y (a-x) (b-y) mapping)/.75. in
            let d= distance a b sortie.(0) sortie.(1) in
            let type_contenu,_,_,_,_= mapping.(a).(b) in
            let coeff= coeff_attractivite distance_actuelle d m_d_sortie.(x).(y) 
                m_d_sortie.(a).(b) d_obstacle in
            (* si elle existe, on recupère la distance de la sortie la plus proche à cette
            case, et en fonction du contenu de cette case, on lui affecte différents score*)
            if type_contenu=3||(type_contenu=4 && z_poss<zone_joueur) then 
                scores.(i)<- 10000000000.
            else if type_contenu=0 then scores.(i)<-(10000.*.exp (coeff))
            else if type_contenu=1 then begin
                (* en fonction de la congestion et de la distance, notre personne pourra 
                pousser une autre personne et prendre sa place, ou alors pourra rester
                au même emplacement*)
                scores.(i)<-  congestion*.(exp coeff)*.stress;
                scores.(0)<-scores.(0) +.exp (coeff)/.100.;
            end;
            somme:= !somme +.scores.(i);
        end
    done;
    somme:= !somme +. scores.(0);
    for i=0 to 8 do
        (* on transforme nos scores en probabilités*)
        scores.(i)<- scores.(i)/.(!somme)
    done;
    (* on tri les scores du plus grand au plus petit*)
    tri_rapide scores places;
    scores, places;;

exception DansObstacle;;

let deplacement mapping x y sortie_ind stress position_sorties nb_sorties 
    zone_finale zone_joueur zones m_d_sortie=
    (* on récupère les indice de la sortie assignée*)
    let _,x_min, x_max, y_min, y_max= position_sorties.(sortie_ind) in
    (* on récupère le point de la sortie le plus proche de la personne*)
    let x_s, y_s=point_sortie_plus_proche x y [|x_min;x_max;y_min;y_max|] in
    (* on récupère les probabilitées associées aux différentes cases autour du 
    joueur et ces mêmes cases*)
    let scores, possibles=deplacements_possibles x y mapping [|x_s;y_s|] stress 
        zone_finale zone_joueur zones m_d_sortie in
    (*tirage d'une probabilité*)
    let probabilite= float 1. and i=ref 0 and s= ref scores.(0) in
    while !i<8 && probabilite>  !s && scores.(!i +1)<>0. do
        (* on parcourt les scores, tant que la somme des probabilités parcourue 
        est inférieur à celle tiré, on regarde le placement suivant*)
        s:= !s +. scores.( !i+1);
        i:= !i +1;
        (* on renvoie le placement correspondant*)
    done; possibles.(!i);;
    
let avance_humains mapping positions_h nb_h positions_s cara_sortie zones 
    zone_finale m_d_sortie hachage=
    let positions_congestion= Array.make_matrix dimx dimy 0. in
    (* on mélange notre population afin de ne pas toujours faire avancer les 
    mêmes en premier*)
    melange positions_h nb_h hachage;
    for i=0 to nb_h-1 do
        let (x,y, prec)= positions_h.(i) in
        (* on associe la position -1 -1 quand la personne est sortie*)
        if (x,y)<>(-1,-1) then begin
            let humain, vitesse, s, stress, zone_humain= mapping.(x).(y) in
            let z_h= ref zone_humain and sortie= ref s and v=ref vitesse in
            (* on enregistre la position précédente*)
            let pos=[|x;y|] in
            (* on garde en mémoire le contenu de la case sur laquelle on est*)
            let saved= ref prec in
            while !v>0 && (pos.(0), pos.(1))<>(-1,-1) do
                (* on récupère le déplacement choisi*)
                let (a,b)= deplacement mapping (pos.(0)) (pos.(1)) (!sortie) 
                    (float_of_int stress) cara_sortie.(!z_h) 
                    (Array.length cara_sortie.(zone_humain)) zone_finale (!z_h) zones
                    m_d_sortie in
                let type_obj,_,_,_,_=mapping.(a).(b) in
                (* on vérifie que la personne ne reste pas sur place et que l'endroit 
                n'est pas un obstacle*)
                if (a,b)<>((pos.(0)),(pos.(1))) && type_obj <> 2  then begin
                    if type_obj=1 then begin
                        (* si c'est un humain qui est sur la case choisie, on echange 
                        les places*)
                        let h_tmp, v_tmp, s_tmp, stress_tmp, z_tmp=mapping.(a).(b) in
                        (*on fait l'échange, et on récupère l'indice de l'humain 
                        dans les positions afin de modifier sa position dans 
                        l'enregistremet des placements*)
                        let ind_remplace= Hashtbl.find hachage (a,b) in
                        let _,_,prec_remplace= positions_h.(ind_remplace) in
                        (* On inverse les zones et les sorties assignées*)
                        mapping.(a).(b)<-(humain, vitesse, s_tmp, stress, z_tmp);
                        mapping.(pos.(0)).(pos.(1))<-(h_tmp, v_tmp, !sortie, stress_tmp, !z_h);
                        (* on met à jour la sortie et la zone du joueur*)
                        z_h:=z_tmp;
                        sortie:=s_tmp;
                        positions_h.(ind_remplace)<- (pos.(0),pos.(1), !saved);
                        (* on considère qu'un échange est une collision et ajoute de 
                        la tension dans la zone*)
                        positions_congestion.(a).(b)<-positions_congestion.(a).(b)+.5.;
                        Hashtbl.replace hachage (a,b) i;
                        Hashtbl.replace hachage (pos.(0),pos.(1)) ind_remplace;
                        saved:= prec_remplace;
                        pos.(0)<-a;
                        pos.(1)<-b;
                    end
                    else if type_obj=0 then begin
                        (* si la case est vide, on déplace la personne et on marque comme 
                        vide son ancienne case*)
                        mapping.(a).(b)<-(humain, vitesse, !sortie, stress, !z_h);
                        mapping.(pos.(0)).(pos.(1))<- !saved;
                        saved:= (0,0,0,0,0);
                        pos.(0)<-a;
                        pos.(1)<-b;
                    end
                    else if type_obj=3 then begin
                        (* sinon, notre personne est sortie*)
                        positions_h.(i)<-(-1,-1, (0,0,0,0,0));
                        mapping.(pos.(0)).(pos.(1))<- !saved;
                        pos.(0)<-  -1;
                        pos.(1)<- -1;
                    end
                    (* on enregistre la nouvelle position*)
                    else  if type_obj=4 && (!z_h)<>zone_finale then begin 
                    (* si on passe a une nouvelle_zone, on avance l'humain et on 
                    change la zone*)
                    let r, t,new_z=positions_s.(!z_h).(!sortie) in
                    let new_sortie= sortie_la_plus_proche mapping cara_sortie.(new_z) 
                        a b (Array.length cara_sortie.(new_z)) in
                    mapping.(pos.(0)).(pos.(1))<- !saved;
                    saved:= mapping.(a).(b);
                    mapping.(a).(b)<-(humain, vitesse, new_sortie, stress, new_z);
                    z_h:=new_z;
                    sortie:= new_sortie;
                    pos.(0)<-a;
                    pos.(1)<-b;
                end;
                (* on indique que la personne a avancé et on enregistre sa nouvelle 
                position si elle n'est pas sortie*)
            end;
            v:= !v -1;
            positions_h.(i)<-(pos.(0),pos.(1), !saved);
            Hashtbl.add hachage (pos.(0),pos.(1)) i;
        done;
        if (pos.(0),pos.(1))<>(-1,-1) then let x,y, _=positions_h.(i) in
            positions_congestion.(x).(y)<-positions_congestion.(x).(y) +.1.;
        end;
    done;
    positions_congestion;;

let affiche_obstacles mapping liste_obstacles nb_obstacles=
    set_color (rgb 0 0 0 ); 
    for i=0 to nb_obstacles-1 do 
        let x,y= liste_obstacles.(i) in
        let a,x_min, x_max, y_min, y_max=mapping.(x).(y) in
        (* on récupère les coins de l'obstacles et on l'affiche*)
        if y_max>0 then
            let tab=[|(5*x_min, 5*y_min);(5*x_min, 5*(y_max));(5*(x_max), 5*(y_max));
                (5*(x_max), 5*y_min)|] in
            fill_poly (tab)
        else
            fill_circle (5*x_min) (5*x_max) (5*y_min)
            (* on aura noté x_min=x; x_max= y et y_min=r*)
    done;;
    
let affiche_pop mapping population nb_humains=
    for i=0 to nb_humains-1 do
        let x,y, _=population.(i) in
        if (x,y)<>(-1,-1) then begin
            let _, v, _, s, _= mapping.(x).(y) in
            let color= rgb (50*s) 0 (50*v) in
            set_color color;
            fill_circle (5*x) (5*y) 2;
        end
    done;;
    
let affiche_sortie mapping tab_sorties cara_sortie zone_finale=
    for j=0 to (Array.length tab_sorties) -1 do
        for i=0 to (Array.length tab_sorties.(j)) -1 do 
            let x,y, z= tab_sorties.(j).(i) in
            (* on récupère les indices de la sortie, x_min, x_max, y_min et y_max*)
            if z=zone_finale then 
                set_color green
            else set_color cyan;
            let a,b, c, d, e= cara_sortie.(j).(i) in
            let x_min, x_max, y_min, y_max= 5*b, 5*c, 5*d, 5*e in
            if a=3 then
                (* on adapte la largeur de la porte en fnction de sa position*)
                if x_min=x_max && x_min=0 then 
                    fill_poly [|(x_min, y_min);(x_min,y_max);(5,y_max);(5,y_min)|]
                else if x_min=x_max && x_min>128 then 
                    fill_poly [|(x_min-5, y_min);(x_min-5,y_max);(x_min,y_max);(x_min,y_min)|]
                else if y_min=y_max && y_min =0 then 
                    fill_poly [|(x_min, 0); (x_min, 5);(x_max,5);(x_max, 0)|]
                else fill_poly [|(x_min, y_min-5); (x_min,y_min);(x_max,y_min);(x_max,y_min-5)|]
            else fill_poly [|(x_min, y_min);(x_min, (y_max+1));((x_max+1), (y_max+1));
                ((x_max+1), y_min)|];
        done
    done;;
    
let draw_sortie humains nb sorties mapping=
    set_color magenta;
    for i=0 to nb -1 do
        let x,y, _= humains.(i) in
        if (x,y)<>(-1,-1) then 
            let _,_,s,_,zone= mapping.(x).(y) in
            let xs,ys,_=sorties.(zone).(s) in
            draw_poly_line [|(5*x,5*y);(5*xs,5*ys)|]
    done;;

(* calcul et affichage des flux*)
let dessiner_image img =
    draw_image (make_image img) 0 0;;
		
let ajoute_matrices destination ajout=
    for i=0 to dimx-1 do
        for j=0 to dimy-1 do
            destination.(i).(j)<-destination.(i).(j)+. ajout.(i).(j)
        done
    done;;

let max_min_matrice matrice=
    let max=ref matrice.(0).(0) and min=ref matrice.(0).(0) in
    for i=0 to dimx-1 do
        for j=0 to dimy-1 do
            if matrice.(i).(j)> !max then max:=matrice.(i).(j)
            else if matrice.(i).(j)< !min && matrice.(i).(j)>0. then min:=matrice.(i).(j)
        done
    done; !max, !min;;
    
let couleur valeur=
    if valeur-256> 0 then
        if valeur -2*256>=0 then (valeur -2*256,0,3*256-valeur-1)
        else (0,2*256-valeur -1, valeur -256)
    else (255-valeur,255,255-valeur);;

let pi= 4.*. (atan 1.);;

let converti_matrice_couleur matrice temps=
    for i=0 to dimx -1 do
        for j=0 to dimy-1 do
            matrice.(i).(j)<- matrice.(i).(j)/.temps
        done
    done;
    let matrice_couleur= Array.make_matrix (5*dimx) (5*dimy) 0 in
    let max, min= max_min_matrice matrice in
    for i=0 to dimx -1 do
        for j=0 to dimy -1 do
            let valeur= int_of_float ((3.*.256. -.1.)*.
                (sin((atan2 (200.*.(matrice.(i).(j)-.min)) (max-.min))))) in
            let r, g, b= couleur valeur in
            for s=5*i to 5*(i+1)-1 do
                for k= 5*j to (5*(j+1))-1 do
                    if valeur>0 then matrice_couleur.(5*dimx-1 - k).(s)<- rgb r g b
                    else matrice_couleur.(5*dimx-1 - k).(s)<- rgb 255 255 255
                done
            done
        done
    done; matrice_couleur;;
    
let matrice_to_coeff matrice= 
    let matrice_coeff= Array.make_matrix (5*dimx) (5*dimy) 0. in
    let max, min= max_min_matrice matrice in
    for i=0 to dimx -1 do
        for j=0 to dimy -1 do
            let valeur= ((3.*.256. -.1.)*.
                (sin((atan2 (200.*.(matrice.(i).(j)-.min)) (max-.min))))) in
            for s=5*i to 5*(i+1)-1 do
                for k= 5*j to (5*(j+1))-1 do
                    matrice_coeff.(s).(k)<- valeur
                done
            done
        done
    done; matrice_coeff;;
	
let to_color matrice=
    let matrice_couleur= Array.make_matrix (5*dimx) (5*dimy) 0 in
    for i=0 to 5*dimx -1 do
        for j=0 to 5*dimy-1 do
            let valeur=int_of_float matrice.(i).(j) in
            let r,g,b= couleur (valeur) in
            if valeur>0 then matrice_couleur.(5*dimx-1 - j).(i)<- rgb r g b
            else matrice_couleur.(5*dimx-1 - j).(i)<- rgb 255 255 255
        done
    done; matrice_couleur;;
    
let rend_uniforme matrice =
    let matrice_coeff= matrice_to_coeff matrice in
    let new_mat= Array.make_matrix (5*dimx) (5*dimy)  0. in
    for i=0 to 5*dimx -1 do
        for j=0 to 5*dimy -1 do
            let x_min= max 0 (i-8) and x_max =min (5*dimx-1) (i+8) and
                y_min= max 0 (j-8) and y_max =min (5*dimy -1) (j+8) in
            let moy= ref 0.  and comptage= ref 0. in 
            for x=x_min to x_max do
                for y=y_min to y_max do
                    moy:= !moy +. matrice_coeff.(x).(y);
                    comptage:= !comptage +.1.;
                done
            done;
            new_mat.(i).(j)<- !moy /. !comptage;
        done
    done;(to_color new_mat);;

let affiche_zones zones=
    for i=0 to Array.length zones -1 do
        let t, z= zones.(i) in
        set_color (rgb 255 (255-10*(z)) (255-10*(z)) );
        let x1, x2, y1, y2= 5*t.(0),5*t.(1),5*t.(2), 5*t.(3) in
        fill_poly [|(x1,y1);(x1,y2);(x2,y2);(x2,y1)|];
    done;;
    
let matrice_to_coeff2 matrice max min= 
    let matrice_coeff= Array.make_matrix (5*dimx) (5*dimy) 0. in
    for i=0 to dimx -1 do
        for j=0 to dimy -1 do
            let valeur= ((3.*.256. -.1.)*.
                (sin((atan2 (500.*.(matrice.(i).(j)-.min)) (max-.min))))) in
            for s=5*i to 5*(i+1)-1 do
                for k= 5*j to (5*(j+1))-1 do
                    matrice_coeff.(s).(k)<- valeur
                done
            done
        done
    done; matrice_coeff;;
    
let rend_uniforme_2 matrice max_g m temps=
    for i=0 to dimx -1 do
        for j=0 to dimy-1 do
            matrice.(i).(j)<- matrice.(i).(j)/.temps
        done
    done;
    let matrice_coeff= matrice_to_coeff2 matrice max_g m in
    let new_mat= Array.make_matrix (5*dimx) (5*dimy)  0. in
    for i=0 to 5*dimx -1 do
        for j=0 to 5*dimy -1 do
            let x_min= max 0 (i-8) and x_max =min (5*dimx-1) (i+8) and
            y_min= max 0 (j-8) and y_max =min (5*dimy -1) (j+8) in
            let moy= ref 0.  and comptage= ref 0. in 
            for x=x_min to x_max do
                for y=y_min to y_max do
                    moy:= !moy +. matrice_coeff.(x).(y);
                    comptage:= !comptage +.1.;
                done
            done;
            new_mat.(i).(j)<- !moy /. !comptage;
        done
    done;(to_color new_mat);;
    
(*Modélisation 1*)
let modelisation1 ()=
    let mapping= Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obstacles=Array.make 40 (0,0) in
    (*mur1*)
    obstacles.(0)<- genere_mur [|0.;-.1.|] [|58.;129.|] 20 mapping;
    obstacles.(1)<- genere_mur [|0.;-.1.|] [|58.;100.|] 14 mapping;
    obstacles.(2)<- genere_mur [|1.;0.|] [|0.;85.|] 58 mapping;
    (*mur 2*)
    obstacles.(3)<- genere_mur [|1.;0.|] [|0.;60.|] 58 mapping;
    obstacles.(4)<- genere_mur [|0.;-.1.|] [|58.;60.|] 30 mapping;
    obstacles.(5)<- genere_mur [|0.;-.1.|] [|58.;20.|] 20 mapping;
    (*mur 3*)
    obstacles.(6)<- genere_mur [|0.;1.|] [|70.;0.|] 20 mapping;
    obstacles.(7)<- genere_mur [|0.;1.|] [|70.;30.|] 30 mapping;
    obstacles.(8)<- genere_mur [|1.;0.|] [|70.;60.|] (129-70) mapping;
    (*mur 4*)
    obstacles.(9)<- genere_mur [|-.1.; 0.|] [|129.; 85.|] (129-70-1) mapping;
    obstacles.(10)<- genere_mur [|0.; 1.|] [|70.; 85.|] 15 mapping;
    obstacles.(11)<- genere_mur [|0.; 1.|] [|70.; 109.|] 20 mapping;
    (*enregistrement des différentes zones*)
    let zones=[| ([|0;129; 60;85|], 0);([|60;70;0;129|], 0);([|0; 57; 0; 59|], 1);
        ([|71; 129; 0; 59|], 2);([|71; 129; 86; 129|], 3);([|0; 57; 86; 129|], 4)|] in
    (*création des sorties*)
    let sorties=Array.make_matrix 5 4 (-1,-1, -1) in
    (*sorties d'évacuation*)
    sorties.(0).(0)<-genere_sortie 68 60 129 129 mapping (-1);
    sorties.(0).(1)<-genere_sortie 68 60 0 0 mapping (-1);
    sorties.(0).(2)<-genere_sortie 0 0 80 64 mapping (-1);
    sorties.(0).(3)<-genere_sortie 129 129 80 64 mapping (-1);
    (*sorties des pièces*)
    sorties.(4)<-[|(58,101,0)|];
    genere_porte 58 58 101 108 mapping;
    sorties.(1)<-[|(58,29,0)|];
    genere_porte 58 58 21 29 mapping;
    sorties.(2)<-[|(70,21,0)|];
    genere_porte 70 70 21 29 mapping;
    sorties.(3)<-[|(70,101,0)|];
    genere_porte 70 70 101 108 mapping;
    let population=genere_population 2000 mapping zones in
    let cara_sortie= [|[|mapping.(68).(129);mapping.(68).(0);mapping.(0).(80);
        mapping.(129).(80)|];[|mapping.(58).(29)|];[|mapping.(70).(21)|];
        [|mapping.(70).(101)|];[|mapping.(58).(101)|]|] in
    ( mapping, obstacles, zones, sorties, cara_sortie, population);;

let z_f= 0 ;;
(*fin modélisation 1*)

(*Modélisation 2*)
let modelisation2 ()=
    let mapping= Array.make_matrix dimx dimy (0,0,0,0,0) in
    let t1=genere_salle_classe_haut mapping 0 58 85 129 in
    (*Zone 2*)
    let t2=genere_salle_classe_haut mapping 0 58 0 60  in
    (*zone 3*)
    let t3=genere_salle_classe_haut mapping 70 129 0 60 in
    (*zone 4*)
    let t4=genere_salle_classe_haut mapping 70 129 85 129  in
    (*on récupère le nombre d'obstacles*)
    let n1=Array.length t1 and n2=Array.length t2 and n3=Array.length t3 and 
    n4=Array.length t4 in
    let obstacles=Array.make (12+ n1+n2+n3+n4) (0,0) in
    (*mur1*)
    obstacles.(1)<- genere_mur [|0.;-.1.|] [|58.;129.|] 43 mapping;
    obstacles.(2)<- genere_mur [|1.;0.|] [|0.;85.|] 20 mapping;
    obstacles.(0)<- genere_mur [|1.;0.|] [|30.;85.|] 28 mapping;
    (*mur 2*)
    obstacles.(3)<- genere_mur [|1.;0.|] [|0.;60.|] 58 mapping;
    obstacles.(4)<- genere_mur [|0.;-.1.|] [|58.;60.|] 30 mapping;
    obstacles.(5)<- genere_mur [|0.;-.1.|] [|58.;20.|] 20 mapping;
    (*mur 3*)
    obstacles.(6)<- genere_mur [|0.;1.|] [|70.;0.|] 60 mapping;
    obstacles.(7)<- genere_mur [|1.;0.|] [|70.;60.|] 20 mapping;
    obstacles.(8)<- genere_mur [|1.;0.|] [|100.;60.|] 29 mapping;
    (*mur 4*)
    obstacles.(9)<- genere_mur [|-.1.; 0.|] [|129.; 85.|] (129-70-1) mapping;
    obstacles.(10)<- genere_mur [|0.; 1.|] [|70.; 85.|] 15 mapping;
    obstacles.(11)<- genere_mur [|0.; 1.|] [|70.; 109.|] 20 mapping;
    (*enregistrement des différentes zones*)
    let zones=[|([|0;129; 60;85|], 0);([|60;70;0;129|], 0);([|0; 57; 0; 59|], 1); 
        ([|71; 129; 0; 59|], 2);([|71; 129; 86; 129|], 3);([|0; 57; 86; 129|], 4)|] in
    (* on génère des obstacles dans les zones*)
    (*zone 1*)
    let a_jouter_obs=[|t1;t2;t3;t4|] in
    let lims=[|0;n1;n1+n2;n1+n2+n3|] and nbs=[|n1;n2;n3;n4|] in
    for i=0 to 3 do
        for j=0 to nbs.(i)-1 do
            obstacles.(12 +lims.(i)+ j)<- a_jouter_obs.(i).(j)
        done
    done;
    (*création des sorties*)
    let sorties=Array.make_matrix 5 4 (-1,-1, -1) in
    (*sorties d'évacuation*)
    sorties.(0).(0)<-genere_sortie 68 60 129 129 mapping (-1);
    sorties.(0).(1)<-genere_sortie 68 60 0 0 mapping (-1);
    sorties.(0).(2)<-genere_sortie 0 0 80 64 mapping (-1);
    sorties.(0).(3)<-genere_sortie 129 129 80 64 mapping (-1);
    (*sorties des pièces*)
    (*sortie zone 0*)
    sorties.(4)<-[|(21,85,0)|];
    genere_porte 21 29 85 85 mapping;
    (* sortie zone 2*)
    sorties.(1)<-[|(58,29,0)|];
    genere_porte 58 58 21 29 mapping;
    (*sortie zone 3*)
    sorties.(2)<-[|(91,60,0)|];
    genere_porte 91 99 60 60 mapping;
    (*sortie zone 4*)
    sorties.(3)<-[|(70,101,0)|];
    genere_porte 70 70 101 108 mapping;
    let cara_sortie= [|[|mapping.(68).(129);mapping.(68).(0);mapping.(0).(80);
        mapping.(129).(80)|];[|mapping.(58).(29)|];[|mapping.(91).(60)|];
        [|mapping.(70).(101)|];[|mapping.(21).(85)|]|] in
    let population=genere_population 2000 mapping zones in
    (mapping, obstacles, zones, sorties, cara_sortie, population);;
(*fin modélisation 2*)

(* modelisation 3*)
let modelisation3 ()=
    let mapping= Array.make_matrix dimx dimy (0,0,0,0,0) in
    let t1=[|genere_obstacle_cercle mapping [|25;90|] 1|] in
    (*Zone 2*)
    let t2=[|genere_mur [|0.;1.|] [|52. ;17.|] 17 mapping|] in
    (*zone 3*)
    let t3=amphitheatre_droite mapping 70 129 0 60  in
    (*zone 4*)
    let t4=cantine_haut mapping 75 129 85 129  in
    let n1=Array.length t1 and n2=Array.length t2 and n3=Array.length t3 and 
    n4=Array.length t4 in
    let obstacles=Array.make (16+ n1+n2+n3+n4) (0,0) in
    (*mur1*)
    obstacles.(1)<- genere_mur [|0.;-.1.|] [|58.;129.|] 43 mapping;
    obstacles.(2)<- genere_mur [|1.;0.|] [|0.;85.|] 20 mapping;
    obstacles.(0)<- genere_mur [|1.;0.|] [|30.;85.|] 28 mapping;
    (*mur 2*)
    obstacles.(3)<- genere_mur [|1.;0.|] [|0.;60.|] 58 mapping;
    obstacles.(4)<- genere_mur [|0.;-.1.|] [|58.;60.|] 30 mapping;
    obstacles.(5)<- genere_mur [|0.;-.1.|] [|58.;20.|] 20 mapping;
    (*mur 3*)
    obstacles.(6)<- genere_mur [|0.;1.|] [|70.;0.|] 60 mapping;
    obstacles.(7)<- genere_mur [|1.;0.|] [|70.;60.|] 20 mapping;
    obstacles.(8)<- genere_mur [|1.;0.|] [|100.;60.|] 29 mapping;
    (*mur 4*)
    obstacles.(9)<- genere_mur [|-.1.; 0.|] [|129.; 85.|] (129-70-1) mapping;
    obstacles.(10)<- genere_mur [|0.; 1.|] [|70.; 85.|] 15 mapping;
    obstacles.(11)<- genere_mur [|0.; 1.|] [|70.; 109.|] 20 mapping;
    (*pièce 5*)
    obstacles.(12)<- genere_mur [|-.1.; 0.|] [|38.; 105.|] 6 mapping;
    obstacles.(13)<- genere_mur [|1.; 0.|] [|19.; 105.|] 6 mapping;
    obstacles.(14)<- genere_mur [|0.; 1.|] [|19.; 106.|] (129-105-1) mapping;
    obstacles.(15)<- genere_mur [|0.; 1.|] [|38.; 106.|] (129-105-1) mapping;
    (*enregistrement des différentes zones*)
    let zones=[|([|0;129; 60;85|], 0);([|58;70;0;129|], 0);([|0; 57; 0; 59|], 1); 
        ([|71; 129; 0; 59|], 2);([|71; 129; 86; 129|], 3);([|0; 19; 86; 129|], 4);
        ([|38; 57; 86; 129|], 4);([|0; 57; 86; 105|], 4);([|20; 37; 106; 129|], 5)|] in
    (* on génère des obstacles dans les zones*)
    (*zone 1*)
    let a_jouter_obs=[|t1;t2;t3;t4|] in
    let lims=[|0;n1;n1+n2;n1+n2+n3|] and nbs=[|n1;n2;n3;n4|] in
    for i=0 to 3 do
        for j=0 to nbs.(i)-1 do
            obstacles.(16 +lims.(i)+ j)<- a_jouter_obs.(i).(j)
        done
    done;
    (*création des sorties*)
    let sorties=Array.make_matrix 6 4 (-1,-1, -1) in
    (*sorties d'évacuation*)
    sorties.(0).(0)<-genere_sortie 68 60 129 129 mapping (-1);
    sorties.(0).(1)<-genere_sortie 68 60 0 0 mapping (-1);
    sorties.(0).(2)<-genere_sortie 0 0 80 64 mapping (-1);
    sorties.(0).(3)<-genere_sortie 129 129 80 64 mapping (-1);
    (*sorties des pièces*)
    (*sortie zone 4*)
    sorties.(4)<-[|(21,85,0)|];
    genere_porte 21 29 85 85 mapping;
    (* sortie zone 1*)
    sorties.(1)<-[|(58,29,0)|];
    genere_porte 58 58 21 29 mapping;
    (*sortie zone 2*)
    sorties.(2)<-[|(91,60,0)|];
    genere_porte 91 99 60 60 mapping;
    (*sortie zone 3*)
    sorties.(3)<-[|(70,101,0)|];
    genere_porte 70 70 101 108 mapping;
    (*sortie zone 5*)
    sorties.(5)<-[|(26,105,4)|];
    genere_porte 26 31 105 105 mapping;
    let population= genere_population 2000 mapping zones in
    let cara_sortie= [|[|mapping.(68).(129);mapping.(68).(0);mapping.(0).(80);
        mapping.(129).(80)|];[|mapping.(58).(29)|];[|mapping.(91).(60)|];
        [|mapping.(70).(101)|];[|mapping.(21).(85)|]; [|mapping.(26).(105)|]|] in
    (mapping, obstacles, zones, sorties, cara_sortie, population);;
	
(*1 cases=~ 30cm -> 1m=~ 3 cases= 15pixels; 1case-> 5pixelsx5pixels
on dispose donc d'environ un espace de 43m*43m*)

(* fonctions de remplissage correct de salle*)
(*genere_humain mapping zones x  y*)
let rempli_salle_classe_haut mapping zones x_min x_max y_min y_max =
    let une_unite_x= 4 + 1 and une_unite_y= 2 + 1 in
    let nx=((x_max-x_min-4)/une_unite_x) and ny=((y_max-y_min-4)/une_unite_y) in
    let humains=Array.make (nx*ny*2) (0,0, (0,0,0,0,0)) in
    for x=0 to nx-1 do 
        for y=0 to ny-1 do
            let deb_x=x_min+ 2 +x*une_unite_x and fin_x=x_min+ 2 +x*une_unite_x +4 in
            let deb_y=y_min+ 1 +y*une_unite_y in
            humains.(x*2+2*y*nx)<-genere_humain mapping zones (deb_x+1)  deb_y;
            humains.(x*2+2*y*nx+1)<-genere_humain mapping zones (fin_x-2)  deb_y;
        done
    done;humains;;

let rempli_salle_classe_droite mapping  zones x_min x_max y_min y_max =
    let une_unite_x= 2 + 1 and une_unite_y= 4 + 1 in
    let nx=((x_max-x_min-4)/une_unite_x) and ny=((y_max-y_min-4)/une_unite_y) in
    let humains=Array.make (nx*ny*2) (0,0,(0,0,0,0,0)) in
    for x=0 to nx-1 do 
        for y=0 to ny-1 do
            let deb_x=x_min+ 2 +x*une_unite_x in
            let deb_y=y_min+ 2 +y*une_unite_y and fin_y=y_min+ 2 +y*une_unite_y+ 4 in
            humains.(y*2+2*x*ny)<-genere_humain mapping zones (deb_x-1) (deb_y +1);
            humains.(y*2+2*x*ny+1)<-genere_humain mapping zones (deb_x-1) (fin_y-2);
        done
    done;humains;;
    
let rempli_amphitheatre_haut mapping zones x_min x_max y_min y_max =
    let une_unite_x= 2 + 1 and largeur_y=(y_max-y_min -6)/2 in
    let nb_h=largeur_y-2 in
    let nx=((x_max-x_min-4)/une_unite_x) in
    let humains=Array.make (nx*2*nb_h) (0,0,(0,0,0,0,0)) in
    for x=0 to nx-1 do
        let deb_x=x_min+ 4 +x*une_unite_x in
        for y= 0 to nb_h-1 do 
            humains.(2*y + x*2*nb_h)<-genere_humain mapping zones deb_x (y_min+2 + y*2);
            humains.(2*y + x*2*nb_h+1)<-genere_humain mapping zones deb_x (y_max-3 -y*2);
        done
    done; humains;;

let rempli_amphitheatre_droite mapping zones x_min x_max y_min y_max =
    let une_unite_y= 2 + 1 and largeur_x=(x_max-x_min -6)/2 in
    let nb_h=largeur_x-4 in
    let ny=((y_max-y_min-4)/une_unite_y) in
    let humains=Array.make (ny*2*nb_h) (0,0,(0,0,0,0,0)) in
    for y=0 to ny-1 do
        let deb_y=y_min +1 +y*une_unite_y  in
        for x=0 to nb_h-1 do 
            humains.(2*x+y*2*nb_h)<- genere_humain mapping zones (x_min +3 + x*2) deb_y;
            humains.(2*x+y*2*nb_h +1)<-genere_humain mapping zones (x_max-3 -x*2) deb_y;
        done
    done; humains;;
    
let rempli_cantine_haut mapping zones x_min x_max y_min y_max =
    let une_unite_x= 4 + 1 and largeur_y=(y_max-y_min -6)/2 in
    let nx=((x_max-x_min-4)/une_unite_x) in
    let nb_h=largeur_y -7 in
    let humains=Array.make (nx*4*nb_h) (0,0, (0,0,0,0,0)) in
    for x=0 to ((x_max-x_min-4)/une_unite_x)-1 do 
        let deb_x=x_min+ 2 +x*une_unite_x and fin_x=x_min+ 2 +x*une_unite_x +2 in
        for y=0 to nb_h-1 do
            humains.(4*y+x*4*nb_h)<- genere_humain mapping zones deb_x (y_min +3 + y*2);
            humains.(4*y+x*4*nb_h+1)<- genere_humain mapping zones fin_x (y_min +3 + y*2);
            humains.(4*y+x*4*nb_h+2)<- genere_humain mapping zones deb_x (y_max-3 -y*2);
            humains.(4*y+x*4*nb_h+3)<- genere_humain mapping zones fin_x (y_max-3 -y*2);
        done
    done; humains;;

let rempli_cantine_droite mapping zones x_min x_max y_min y_max =
    let une_unite_y= 4 + 1 and largeur_x=(x_max-x_min -6)/2 in
    let ny=((y_max-y_min-4)/une_unite_y) in
    let nb_h=largeur_x -7 in
    let humains=Array.make (ny*4*nb_h) (0,0, (0,0,0,0,0)) in
    for y=0 to ((y_max-y_min-4)/une_unite_y)-1 do 
        let deb_y=y_min+ 1 +y*une_unite_y and fin_y=y_min+ 2 +y*une_unite_y +2 in
        for x=0 to nb_h-1 do 
            humains.(4*x+y*4*nb_h)<- genere_humain mapping zones (x_min +3 + x*2) deb_y;
            humains.(4*x+y*4*nb_h+1)<- genere_humain mapping zones (x_min +3 + x*2) fin_y;
            humains.(4*x+y*4*nb_h+2)<- genere_humain mapping zones (x_max-3 -x*2) deb_y;
            humains.(4*x+y*4*nb_h+3)<- genere_humain mapping zones (x_max-3 -x*2) fin_y;
        done
    done; humains;;
(*fin des fonctions de remplissage correct de salles*)

(*fonction de remplissage aléatoire d'un etablissement*)
let genere_pop_aleatoire_etablissement zones_interdites mapping zones nombre=
    let humains=Array.make nombre (0,0, (0,0,0,0,0)) in
    for i=0 to nombre-1 do
        (* on prend une position aléatoire*)
        let x= ref (int(dimx)) and y= ref (int(dimy)) in 
        let z= ref ( find_zone (!x) (!y) zones) in 
        (* tant que notre position tirée n'st pas une case vide, on réeffectue un tirage*)
        while mapping.(!x).(!y)<>(0,0,0,0,0) || (List.mem (!z) zones_interdites) do
            x:=int(dimx);
            y:=int(dimy);
            z:= ( find_zone (!x) (!y) zones);
        done;
        (* on assigne des vitesses, correspondant au nombre de cases parcourue lors 
        d'un avancement, et une propension au stress aléatoire*)
        mapping.(!x).(!y)<-(1,  3+int(3), 0, int(5)+1, !z);
        humains.(i)<- (!x, !y, (0,0,0,0,0));
        done;humains;;

let genere_pop_aleatoire_dans_zone zones_voulues mapping zones nombre=
    let humains=Array.make nombre (0,0, (0,0,0,0,0)) in
    for i=0 to nombre-1 do
        (* on prend une position aléatoire*)
        let x= ref (int(dimx)) and y= ref (int(dimy)) in 
        let z= ref ( find_zone (!x) (!y) zones) in
        (* tant que notre position tirée n'st pas une case vide, on réeffectue un tirage*)
        while mapping.(!x).(!y)<>(0,0,0,0,0) || not (List.mem (!z) zones_voulues) do
            x:=int(dimx);
            y:=int(dimy);
            z:= ( find_zone (!x) (!y) zones);
        done;
        (* on assigne des vitesses, correspondant au nombre de cases parcourue 
        lors d'un avancement, et une propension au stress aléatoire*)
        mapping.(!x).(!y)<-(1,  3+int(3), 0, int(5)+1, !z);
        humains.(i)<- (!x, !y, (0,0,0,0,0));
        done;humains;;

let combine_tableaux tableaux=
    let n_tab=Array.length tableaux  in
    let n= Array.fold_left (fun a b-> a+(Array.length b)) 0 tableaux in
    let indices= Array.make (Array.length tableaux) 0 in
    let n_ind= ref 0 in
    for i= 0 to n_tab -2 do 
        n_ind := !n_ind + Array.length tableaux.(i);
        indices.(i+1)<- !n_ind;
    done;
    let nouveau_tableau =Array.make n tableaux.(0).(0) in
    for i=0 to n_tab -1 do
        for j=0 to Array.length tableaux.(i) -1 do
            nouveau_tableau.(indices.(i)+j)<- tableaux.(i).(j)
        done
    done; nouveau_tableau;;

(*Simulation d'un établissement scolaire*)
let metre=3;;

let  mur_etablissement_scolaire mapping murs= 
    (*avec notre simulation,on aura 69 murs + un obstacle*)
    (*genere_obstacle x_min x_max y_min y_max mapping*)
    let nb_murs=Array.length murs in
    let obstacles= Array.make nb_murs (0,0) in
    for i=0 to nb_murs-1 do 
        let x_min,x_max, y_min, y_max= murs.(i) in
        obstacles.(i)<-(x_min, y_min);
        genere_obstacle x_min x_max y_min y_max mapping;
    done; obstacles;;
    
(*portes et sorties*)
let genere_portes_sorties_etablissement_1 mapping=
    let sorties= Array.make_matrix 21 4 (0,0,0) and 
    cara_s= Array.make_matrix 21 4 (0,0,0,0,0) in
    (*zone 0*)
    sorties.(0)<-[| genere_sortie (3*metre) 1 0 0 mapping (-1);
        genere_sortie 0 0 (3*metre) 0 mapping (-1)|];
    cara_s.(0)<-[|mapping.(1).(0);mapping.(0).(0)|];
    (*zone 1*)
    genere_porte (3*metre +1) (4*metre -1) (9*metre) (9*metre) mapping;
    genere_porte (10*metre +1) (13*metre -1) (10*metre) (10*metre) mapping;
    genere_porte (33*metre) (35*metre -1) (10*metre) (10*metre) mapping;
    sorties.(1)<-[|((3*metre +1),(9*metre), 0);((10*metre +1),(10*metre), 0);
        ((33*metre +1),(10*metre),0)|];
    cara_s.(1)<-[|mapping.(3*metre +1).(9*metre);mapping.(10*metre +1).(10*metre);
        mapping.(33*metre +1).(10*metre)|];
    (*zone 2*)
    genere_porte (12*metre+1) (14*metre -1) (15*metre) (15*metre) mapping;
    genere_porte (39*metre) (39*metre) (12*metre+3) (14*metre-3) mapping;
    sorties.(2)<-[|(12*metre+1,15*metre,1); (39*metre,12*metre+3,1)|];
    cara_s.(2)<-[|mapping.(12*metre+1).(15*metre);mapping.(39*metre).(12*metre+3)|];
    (*zone 3*)
    genere_porte (metre+1) (2*metre-1) (11*metre) (11*metre) mapping;
    sorties.(3)<-[|(metre+1, 11*metre, 1)|];
    cara_s.(3)<-[|mapping.(metre+1).(11*metre)|];
    (*zone 4*)
    genere_porte (7*metre+2) (10*metre-2) (12*metre) (12*metre) mapping;
    sorties.(4)<-[|(7*metre+2, 12*metre, 1)|];
    cara_s.(4)<-[|mapping.(7*metre+2).(12*metre)|];
    (*zone 5*)
    genere_porte (3*metre+1) (5*metre-1) (22*metre) (22*metre) mapping;
    sorties.(5)<-[|(3*metre+1, 22*metre, 1)|];
    cara_s.(5)<-[|mapping.(3*metre+1).(22*metre)|];
    (*zone 6*)
    genere_porte (14*metre) (14*metre) (3*metre+1) (4*metre-1) mapping;
    sorties.(6)<-[|(14*metre, 3*metre+1, 0)|];
    cara_s.(6)<-[|mapping.(14*metre).(3*metre+1)|];
    (*zone 7*)
    genere_porte (18*metre+1) (19*metre-1) (12*metre) (12*metre) mapping;
    genere_porte (23*metre+1) (24*metre-1) (12*metre) (12*metre) mapping;
    sorties.(7)<-[|(18*metre+1, 12*metre, 1);(23*metre +1, 12*metre, 1)|];
    cara_s.(7)<-[|mapping.(18*metre+1).(12*metre);mapping.(23*metre+1).(12*metre)|];
    (*zone 8*)
    genere_porte (26*metre+1) (27*metre-1) (12*metre) (12*metre) mapping;
    genere_porte (31*metre+1) (32*metre-1) (12*metre) (12*metre) mapping;
    sorties.(8)<-[|(26*metre+1, 12*metre, 1);(31*metre +1, 12*metre, 1)|];
    cara_s.(8)<-[|mapping.(26*metre+1).(12*metre);mapping.(31*metre+1).(12*metre)|];
    (*zone 9*)
    genere_porte (35*metre) (35*metre) (metre+1) (2*metre-1) mapping;
    genere_porte (35*metre) (35*metre) (4*metre+1) (5*metre-1) mapping;
    genere_porte (35*metre) (35*metre) (6*metre+1) (8*metre-1) mapping;
    genere_porte (41*metre+1) (42*metre-1) (10*metre) (10*metre) mapping;
    sorties.(9)<-[|(35*metre,metre+1, 0);(35*metre,4*metre+1, 0);(35*metre,6*metre+1, 0);
        (41*metre+1,10*metre, 1)|];
    cara_s.(9)<-[|mapping.(35*metre).(metre+1);mapping.(35*metre).(4*metre+1);
        mapping.(35*metre).(6*metre+1);mapping.(41*metre+1).(10*metre)|];
    (*zone 10*)
    genere_porte (41*metre) (41*metre) (15*metre+1) (16*metre-1) mapping;
    genere_porte (41*metre) (41*metre) (18*metre+1) (19*metre-1) mapping;
    sorties.(10)<-[|(41*metre,15*metre+1, 1);(41*metre,18*metre+1, 1)|];
    cara_s.(10)<-[|mapping.(41*metre).(15*metre+1);mapping.(41*metre).(18*metre+1)|];
    (*zone 11*)
    genere_porte (38*metre) (38*metre) (28*metre+1) (29*metre-1) mapping;
    sorties.(11)<-[|(38*metre, 28*metre+1, 1)|];
    cara_s.(11)<-[|mapping.(38*metre).(28*metre+1)|];
    (*zone 12*)
    genere_porte (38*metre) (38*metre) (35*metre+1) (36*metre-1) mapping;
    sorties.(12)<-[|(38*metre, 35*metre+1, 1)|];
    cara_s.(12)<-[|mapping.(38*metre).(35*metre+1)|];
    (*zone 13*)
    genere_porte (38*metre) (38*metre) (38*metre+1) (39*metre-1) mapping;
    sorties.(13)<-[|(38*metre, 38*metre+1, 1)|];
    cara_s.(13)<-[|mapping.(38*metre).(38*metre+1)|];
    (*zone 14*)
    genere_porte (40*metre) (40*metre) (40*metre+1) (41*metre-1) mapping;
    sorties.(14)<-[|(40*metre, 40*metre+1, 1)|];
    cara_s.(14)<-[|mapping.(40*metre).(40*metre+1)|];
    (*zone 15*)
    genere_porte (40*metre) (40*metre) (36*metre+1) (37*metre-1) mapping;
    sorties.(15)<-[|(40*metre, 36*metre+1, 1)|];
    cara_s.(15)<-[|mapping.(40*metre).(36*metre+1)|];
    (*zone 16*)
    genere_porte (40*metre) (40*metre) (34*metre+1) (35*metre-1) mapping;
    sorties.(16)<-[|(40*metre, 34*metre+1, 1)|];
    cara_s.(16)<-[|mapping.(40*metre).(34*metre+1)|];
    (*zone 17*)
    genere_porte (40*metre) (40*metre) (29*metre+3) (31*metre-3) mapping;
    sorties.(17)<-[|(40*metre, 29*metre+3, 1)|];
    cara_s.(17)<-[|mapping.(40*metre).(29*metre+3)|];
    (*zone 18*)
    genere_porte (40*metre) (40*metre) (27*metre+3) (29*metre-3) mapping;
    sorties.(18)<-[|(40*metre, 27*metre+3, 1)|];
    cara_s.(18)<-[|mapping.(40*metre).(27*metre+3)|];
    (*zone 19*)
    genere_porte (40*metre) (40*metre) (24*metre+1) (25*metre-1) mapping;
    sorties.(19)<-[|(40*metre, 24*metre+1, 1)|];
    cara_s.(19)<-[|mapping.(40*metre).(24*metre+1)|];
    (*zone 20*)
    genere_porte (3*metre) (3*metre) (18*metre+1) (19*metre-1) mapping;
    sorties.(20)<-[|(3*metre, 18*metre+1, 1)|];
    cara_s.(20)<-[|mapping.(3*metre).(18*metre+1)|];
    (sorties, cara_s);;

(*murs*)
let obs_et1= [|(*cour*)
                (3*metre, 9*metre, 3*metre, 7*metre);
                (*CDI*)
                (14*metre, 14*metre, 2*metre ,3*metre);
                (14*metre, 14*metre, 4*metre ,10*metre-1);
                (14*metre+1, 23*metre, 2*metre ,2*metre);
                (23*metre, 23*metre, 2*metre +1 ,10*metre-1);
                (*sport*)
                (35*metre, 35*metre, 0*metre ,1*metre);
                (35*metre, 35*metre, 2*metre ,4*metre);
                (35*metre, 35*metre, 5*metre ,6*metre);
                (35*metre, 35*metre, 8*metre ,10*metre-1);
                (*cantine*)
                (0, 3*metre-1, 22*metre ,22*metre);
                (5*metre+1, 12*metre-1, 22*metre ,22*metre);
                (0, 12*metre-1, 39*metre ,39*metre);
                (12*metre, 12*metre, 22*metre ,39*metre);
                (*cafet*)
                (0, 3*metre-1, 16*metre ,16*metre);
                (*salles de classes 4murs*)
                (17*metre, 17*metre, 12*metre +1 ,20*metre -1);
                (25*metre, 25*metre, 12*metre +1 ,20*metre -1);
                (33*metre, 33*metre, 12*metre +1 ,20*metre -1);
                (17*metre, 33*metre, 20*metre  ,20*metre);
                (* salle de classe 1 mur*)
                (35*metre, 35*metre, 14*metre +1 ,22*metre -1);
                (*amphithéâtres*)
                (29*metre, 29*metre, 22*metre +1 ,43*metre );
                (29*metre+1, 38*metre-1, 37*metre  ,37*metre);
                (29*metre+1, 38*metre-1, 30*metre  ,30*metre);
                (*salles de colle*)
                (40*metre+1, 43*metre, 39*metre  ,39*metre);
                (40*metre+1, 43*metre, 35*metre  ,35*metre);
                (40*metre+1, 43*metre, 31*metre  ,31*metre);
                (40*metre+1, 43*metre, 29*metre  ,29*metre);
                (40*metre+1, 43*metre, 27*metre  ,27*metre);
                (*murs "en haut/gauche" du couloir*)
                (0, metre, 11*metre, 11*metre);
                (*porte cafet*)
                (2*metre, 3*metre-1, 11*metre  ,11*metre);
                (3*metre, 3*metre, 11*metre  ,18*metre);
                (*porte zone 20*)
                (3*metre, 3*metre, 19*metre  ,22*metre);
                (5*metre, 5*metre, 12*metre  ,22*metre);
                (5*metre+1, 7*metre+1, 12*metre  ,12*metre);
                (*porte chapelle*)
                (10*metre-1, 12*metre-1, 12*metre  ,12*metre);
                (12*metre, 12*metre, 12*metre ,22*metre-1);
                (*porte cour de sport*)
                (14*metre, 14*metre, 12*metre +1 ,15*metre);
                (14*metre, 18*metre, 12*metre  ,12*metre);
                (*porte 1 salle 1*)
                (19*metre, 23*metre, 12*metre  ,12*metre);
                (*porte 2 salle 1*)
                (24*metre, 26*metre, 12*metre  ,12*metre);
                (*porte 1 salle 2*)
                (27*metre, 31*metre, 12*metre  ,12*metre);
                (*porte 2 salle 2*)
                (32*metre, 39*metre-1, 12*metre  ,12*metre);
                (39*metre, 39*metre, 12*metre  ,12*metre+2);
                (*sortie cour*)
                (39*metre, 39*metre, 14*metre -2 ,14*metre-1);
                (35*metre, 41*metre-1, 14*metre  ,14*metre);
                (41*metre, 41*metre, 14*metre  ,15*metre);
                (*porte 1 salle 3*)
                (41*metre, 41*metre, 15*metre  ,18*metre);
                (* porte 2 salle 3*)
                (41*metre, 41*metre, 19*metre  ,20*metre-1);
                (40*metre+1, 41*metre, 20*metre  ,20*metre);
                (40*metre, 40*metre, 20*metre  ,21*metre-1);
                (39*metre+1, 40*metre, 21*metre  ,21*metre);
                (39*metre, 39*metre, 21*metre  ,22*metre-1);
                (29*metre, 39*metre, 22*metre  ,22*metre);
                (38*metre, 38*metre, 22*metre +1  ,28*metre);
                (*porte amphi 1*)
                (38*metre, 38*metre, 29*metre  ,35*metre);
                (*porte amphi 2*)
                (38*metre, 38*metre, 36*metre  ,38*metre);
                (*porte amphi 3*)
                (38*metre, 38*metre, 39*metre  ,43*metre);
                (*on repart de l'autre côté*)
                (40*metre  ,40*metre, 41*metre, 43*metre);
                (*porte salle de colle 4*)
                (40*metre  ,40*metre, 37*metre, 40*metre);
                (*porte salle de colle 3*)
                (40*metre  ,40*metre, 35*metre, 36*metre);
                (*porte salle de colle 2*)
                (40*metre  ,40*metre,31*metre-2, 34*metre);
                (*porte toilette 2*)
                (40*metre  ,40*metre,29*metre-2, 29*metre+2);
                (*porte toilette 1*)
                (40*metre  ,40*metre,25*metre, 27*metre+2);
                (*porte salle de colle 1*)
                (40*metre  ,40*metre,23*metre+1, 24*metre);
                (40*metre, 43*metre, 23*metre  ,23*metre);
                (42*metre, 43*metre, 10*metre  ,10*metre);
                (*porte accès sport*)
                (35*metre, 41*metre, 10*metre  ,10*metre);
                (*porte accès cour*)
                (13*metre, 33*metre, 10*metre  ,10*metre);
                (*porte accès cour*)
                (5*metre+1, 11*metre, 10*metre  ,10*metre);
                (5*metre, 5*metre, 9*metre+1  ,10*metre);
                (4*metre, 5*metre, 9*metre  ,9*metre);
                (*porte acces cantine/cour*)
                (0, 3*metre, 9*metre  ,9*metre)|];;

(*zones*)
let zones_et1 = [| (*zone 0*)
                    ([|0; 14*metre -1; 0;9*metre |],0);
                    ([|5*metre ; 14*metre; 0;10*metre|],0);
                    ([|14*metre ; 35*metre; 0;2*metre|],0);
                    ([|23*metre +1; 35*metre; 0;10*metre|],0);
                    (*zone 1*)
                    ([|0; 5*metre; 9*metre +1;11*metre|],1);
                    ([|3*metre; 5*metre; 9*metre +1;22*metre|],1);
                    ([|3*metre; 43*metre ; 10*metre +1;12*metre|],1);
                    ([|12*metre; 14*metre; 10*metre +1;15*metre|],1);
                    ([|39*metre; 43*metre ; 10*metre ;14*metre|],1);
                    ([|41*metre; 43*metre ; 10*metre ;21*metre |],1);
                    ([|40*metre; 43*metre ; 20*metre;23*metre|],1);
                    ([|39*metre; 41*metre ; 21*metre ;23*metre|],1);
                    ([|38*metre; 40*metre ; 22*metre ;43*metre |],1);
                    (*zone 2*)
                    ([|0; 29*metre-1 ; 39*metre +1; 43*metre|],2);
                    ([|12*metre +1; 29*metre-1 ; 20*metre +1; 43*metre|],2);
                    ([|28*metre +1; 35*metre-1 ; 20*metre +1; 22*metre-1|],2);
                    ([|33*metre +1; 35*metre-1 ; 12*metre +1; 22*metre-1|],2);
                    ([|33*metre +1; 39*metre-1 ; 12*metre +1; 14*metre-1|],2);
                    ([|12*metre +1; 17*metre-1 ; 15*metre +1; 20*metre +1|],2);
                    ([|14*metre +1; 17*metre-1 ; 12*metre +1; 15*metre +1|],2);
                    (*zone 3*)
                    ([|0; 3*metre-1 ; 11*metre +1; 16*metre-1|],3);
                    (*zone 4*)
                    ([|5*metre +1; 12*metre-1 ; 12*metre +1; 22*metre-1|],4);
                    (*zone 5*)
                    ([|0; 12*metre-1 ; 22*metre +1; 39*metre-1|],5);
                    (*zone 6*)
                    ([|14*metre +1; 23*metre-1 ; 2*metre +1; 10*metre-1|],6);
                    (*zone 7*)
                    ([|17*metre +1; 25*metre-1 ; 12*metre +1; 20*metre-1|],7);
                    (*zone 8*)
                    ([|25*metre +1; 33*metre-1 ; 12*metre +1; 20*metre-1|],8);
                    (*zone 9*)
                    ([|35*metre +1; 43*metre ; 0; 10*metre-1|],9);
                    (*zone 10*)
                    ([|35*metre +1; 41*metre-1 ; 14*metre +1; 20*metre-1|],10);
                    ([|35*metre +1; 40*metre-1 ; 20*metre-1 ; 21*metre-1|],10);
                    ([|35*metre +1; 39*metre-1 ; 21*metre-1 ; 22*metre-1|],10);
                    (*zone 11*)
                    ([|29*metre +1; 38*metre-1 ; 22*metre +1; 30*metre-1|],11);
                    (*zone 12*)
                    ([|29*metre +1; 38*metre-1 ; 30*metre +1; 37*metre-1|],12);
                    (*zone 13*)
                    ([|29*metre +1; 38*metre-1 ; 37*metre +1; 43*metre|],13);
                    (*zone 14*)
                    ([|40*metre +1; 43*metre ; 39*metre +1; 43*metre|],14);
                    (*zone 15*)
                    ([|40*metre +1; 43*metre ; 35*metre +1; 39*metre-1|],15);
                    (*zone 16*)
                    ([|40*metre +1; 43*metre ; 31*metre +1; 35*metre-1|],16);
                    (*zone 17*)
                    ([|40*metre +1; 43*metre ; 29*metre +1; 31*metre-1|],17);
                    (*zone 18*)
                    ([|40*metre +1; 43*metre ; 27*metre +1; 29*metre-1|],18);
                    (*zone 19*)
                    ([|40*metre +1; 43*metre ; 23*metre +1; 27*metre-1|],19);
                    (*zone 20*)
                    ([|0; 3*metre-1 ; 16*metre +1; 22*metre-1|],20)|] ;;

let simulation_etablissement_1 nb_additionnel=
    let m=Array.make_matrix 130 130 (0,0,0,0,0) in
    (*on récupère les murs et les sorties*)
    let obs= mur_etablissement_scolaire m obs_et1 in
    let sorties, cara_sorties=genere_portes_sorties_etablissement_1 m in
    (*obstacles spécifiques à certaines salles*)
    (*salle1*)
    let obs_salle1_tables= genere_salle_classe_droite m (17*metre +1) (25*metre -1) 
        (12*metre +1) (21*metre ) in
    let obs_salle_1prof= [|(25*metre -3,20*metre -7)|] in
    genere_obstacle (25*metre -4) (25*metre -3) (20*metre -7) (20*metre -3)  m;
    (*salle2*)
    let obs_salle2_tables= genere_salle_classe_droite m (25*metre +1) (33*metre -1) 
        (12*metre +1) (21*metre ) in
    let obs_salle_2prof= [|(33*metre -3,20*metre -7)|] in
    genere_obstacle (33*metre -4) (33*metre -3) (20*metre -7) (20*metre -3)  m;
    (*cantine*)
    let obs_cantine=cantine_droite m 0 (12*metre-1) (23*metre +1) (40*metre-1) in
    (*chapelle*)
    let obs_chapelle= amphitheatre_droite m (5*metre +1) (12*metre-1) (12*metre +1) 
        (21*metre-1) in
    genere_obstacle (7*metre +2) (10*metre -2) (20*metre +1) (21*metre)  m;
    let obs_autel=[|(7*metre +2,20*metre +1)|] in
    (*salle 3*)
    let obs_salle3_tables= genere_salle_classe_haut m (35*metre +1) (41*metre+2) 
        (14*metre +1) (21*metre-1) in
    let obs_salle3_prof=[|(35*metre +2, 21*metre-2)|] in
    genere_obstacle (35*metre+2) (39*metre -2) (21*metre-2) (21*metre)  m;
    (*amphithéâtre 1*)
    let obs_amphi1=amphitheatre_droite m (29*metre +1) (38*metre-1) (22*metre +1) 
        (30*metre-1) in
    let obs_amphi1_prof=[|(32*metre, 30*metre -3)|] in
    genere_obstacle (32*metre) (35*metre) ( 30*metre -3) ( 30*metre -2)  m;
    (*amphithéâtre 2*)
    let obs_amphi2=amphitheatre_droite m (29*metre +1) (38*metre-1) (30*metre +1) 
        (37*metre-1) in
    let obs_amphi2_prof=[|(32*metre, 37*metre -3)|] in
    genere_obstacle (32*metre) (35*metre) ( 37*metre -3) ( 37*metre -2)  m;
    (*amphithéâtre 3*)
    let obs_amphi3=amphitheatre_haut m (30*metre ) (39*metre-2) (37*metre +1) (43*metre) in
    let obs_amphi3_prof=[|(29*metre+2, 39*metre-2)|] in
    genere_obstacle (29*metre+2) (29*metre+3) ( 39*metre-2 ) ( 42*metre )  m;
    (*salle de colle 4*)
    let obs_colle4=[|(40*metre +3, 39*metre +2)|] in
    genere_obstacle (40*metre +3) (43*metre-2) ( 39*metre+2 ) ( 39*metre+4 )  m;
    (*salle de colle 3*)
    let obs_colle3=[|(40*metre +3, 35*metre +2)|] in
    genere_obstacle (40*metre +3) (43*metre-2) ( 35*metre+2 ) ( 35*metre+4 )  m;
    (*salle de colle 2*)
    let obs_colle2=[|(40*metre +3, 31*metre +2)|] in
    genere_obstacle (40*metre +3) (43*metre-2) ( 31*metre+2 ) ( 31*metre+4 )  m;
    (*toilettes*)
    let obs_toilettes=[|(43*metre -2,29*metre+2);(40*metre+2, 29*metre+1);
        (43*metre -2,27*metre+2 );(40*metre+2, 29*metre-2)|] in
    genere_obstacle (43*metre -2) (43*metre) ( 29*metre+2 ) ( 31*metre-2 )  m;
    genere_obstacle (40*metre+2) (40*metre+4) ( 29*metre+1 ) ( 29*metre+2 )  m;
    genere_obstacle (43*metre -2) (43*metre) ( 27*metre+2 ) ( 29*metre-2 )  m;
    genere_obstacle (40*metre+2) (40*metre+4) ( 29*metre-2 ) ( 29*metre-1 )  m;
    (*salle de colle 1*)
    let obs_colle1=[|(40*metre +3, 23*metre +2)|] in
    genere_obstacle (40*metre +3) (43*metre-2) ( 23*metre+2 ) ( 23*metre+4 )  m;
    (*salle de sport*)
    let obs_sport=[|(36*metre +1,metre);(36*metre +1,3*metre+2);(36*metre +1,6*metre+1); 
                        genere_obstacle_cercle m [|40*metre+2;metre +2|] 2;
                        genere_obstacle_cercle m [|40*metre+2;4*metre |] 2;
                        genere_obstacle_cercle m [|40*metre+2;9*metre -2|] 2;
                        genere_obstacle_cercle m [|40*metre+2;6*metre|] 2;
                        (43*metre-2,metre);(43*metre-2,5*metre)|] in
    genere_obstacle (36*metre +1) (39*metre +1) metre (3*metre) m;
    genere_obstacle (36*metre +1) (39*metre +1) (3*metre+2) (5*metre+2) m;
    genere_obstacle (36*metre +1) (39*metre +1) (6*metre+1) (8*metre+1) m;
    genere_obstacle (43*metre-2) (43*metre) (1*metre) (3*metre) m;
    genere_obstacle (43*metre-2) (43*metre) (5*metre) (7*metre) m;
    (*cafet*)
    let obs_cafet=[|(0,12*metre);(0,14*metre)|] in
    genere_obstacle 0 2 (12*metre) (14*metre -2) m;
    genere_obstacle 0 2 (14*metre) (16*metre-2) m;
    (*parking *)
    genere_obstacle (25*metre ) (27*metre +2) (7*metre) (10*metre-1) m;
    genere_obstacle (28*metre ) (30*metre +1) (6*metre+1) (10*metre-1)  m;
    genere_obstacle (31*metre) (33*metre -1 ) (6*metre+2) (10*metre-1) m;
    let obs_parking=[|(25*metre ,7*metre);(28*metre,6*metre+1);(31*metre,6*metre+2)|] in
    (*CDI*)
    genere_obstacle (14*metre+1) (14*metre +2) (5*metre) (6*metre+1) m;
    genere_obstacle (14*metre+1) (14*metre +2) (7*metre-1) (8*metre) m;
    genere_obstacle (14*metre+1) (14*metre +2) (8*metre+1) (9*metre+2) m;
    genere_obstacle (17*metre-1) (21*metre) (3*metre) (3*metre+2) m;
    genere_obstacle (22*metre) (22*metre+2) (2*metre+1) (5*metre) m;
    genere_obstacle (16*metre+2) (17*metre+2) (4*metre+1) (5*metre) m;
    genere_obstacle (18*metre) (19*metre) (4*metre+1) (5*metre) m;
    genere_obstacle (19*metre+1) (20*metre+1) (4*metre+1) (5*metre) m;
    genere_obstacle (16*metre-1) (16*metre-1) (6*metre-1) (7*metre-1) m;
    genere_obstacle (16*metre-1) (16*metre-1) (8*metre-1) (9*metre-1) m;
    genere_obstacle (16*metre) (22*metre) (9*metre-2) (9*metre-1) m;
    genere_obstacle (16*metre) (22*metre) (6*metre-1) (6*metre) m;
    genere_obstacle (17*metre) (22*metre) (7*metre) (8*metre-2) m;
    genere_obstacle  (16*metre-1) (17*metre +2) (10*metre-1) (10*metre-1) m;
    genere_obstacle  (18*metre) (20*metre) (10*metre-1) (10*metre-1) m;
    genere_obstacle  (20*metre+1) (22*metre +1) (10*metre-1) (10*metre-1) m;
    let obs_cdi=[|(14*metre+1,6*metre);(14*metre+1,7*metre-1);(14*metre+1,8*metre+1);
                    (17*metre-1,3*metre);(22*metre,2*metre+1);(17*metre-1,4*metre+1);
                    (18*metre,4*metre+1);(19*metre+1,4*metre+1);(16*metre-1,6*metre-1);
                    (16*metre-1,8*metre-1);(16*metre,9*metre-2);(16*metre,6*metre-1);
                    (17*metre,7*metre);(16*metre-1,10*metre-1);(18*metre,10*metre-1);
                    (20*metre+1,10*metre-1)|] in
    (*humains*)
    (*salle de classe 1*)
    let h_s1=rempli_salle_classe_droite m  zones_et1 (17*metre +1) (25*metre -1) 
        (12*metre +1) (21*metre ) in
    (*les professeurs/ personnes dirigeant la salle*)
    let h_profs=[|genere_humain m zones_et1 (25*metre -1) ( 19*metre);
                    genere_humain m zones_et1 (33*metre -1) ( 15*metre);
                    genere_humain m zones_et1 (37*metre +1) ( 21*metre+1);
                    genere_humain m zones_et1 (34*metre -1) ( 30*metre-1);
                    genere_humain m zones_et1 (32*metre -1) ( 37*metre-1);
                    genere_humain m zones_et1 (29*metre +1) ( 41*metre-1);
                    genere_humain m zones_et1 (9*metre -1) ( 22*metre-2)|] in
    (*le CDI*)
    let h_cdi=[| genere_humain m zones_et1 (15*metre ) (5*metre);
                    genere_humain m zones_et1 (15*metre ) (6*metre -1);
                    genere_humain m zones_et1 (15*metre ) (7*metre -1);
                    genere_humain m zones_et1 (15*metre ) (7*metre+1);
                    genere_humain m zones_et1 (15*metre ) (8*metre+1);
                    genere_humain m zones_et1 (15*metre ) (9*metre);
                    (*tables*)
                    genere_humain m zones_et1 (17*metre) (6*metre-2);
                    genere_humain m zones_et1 (19*metre -1) (6*metre-2);
                    genere_humain m zones_et1 (20*metre -1) (6*metre-2);
                    genere_humain m zones_et1 (17*metre) (4*metre);
                    genere_humain m zones_et1 (19*metre -1) (4*metre);
                    genere_humain m zones_et1 (20*metre -1) (4*metre);
                    (*canapes*)
                    genere_humain m zones_et1 (17*metre) (3*metre-1);
                    genere_humain m zones_et1 (18*metre-1) (3*metre-1);
                    genere_humain m zones_et1 (18*metre+1) (3*metre-1);
                    genere_humain m zones_et1 (19*metre) (3*metre-1);
                    genere_humain m zones_et1 (19*metre+2) (3*metre-1);
                    genere_humain m zones_et1 (20*metre+1) (3*metre-1);
                    (*responsables*)
                    genere_humain m zones_et1 (16*metre+1) (7*metre-2);
                    genere_humain m zones_et1 (17*metre) (7*metre-2);
                    genere_humain m zones_et1 (18*metre-1) (7*metre-2);
                    genere_humain m zones_et1 (18*metre+1) (7*metre-2);
                    genere_humain m zones_et1 (19*metre) (7*metre-2);
                    genere_humain m zones_et1 (20*metre-1) (7*metre-2);
                    genere_humain m zones_et1 (20*metre+1) (7*metre-2);
                    genere_humain m zones_et1 (21*metre) (7*metre-2);
                    (*info bas*)
                    genere_humain m zones_et1 (16*metre+2) (8*metre);
                    genere_humain m zones_et1 (17*metre+1) (8*metre);
                    genere_humain m zones_et1 (18*metre) (8*metre);
                    genere_humain m zones_et1 (18*metre+2) (8*metre);
                    genere_humain m zones_et1 (19*metre+1) (8*metre);
                    genere_humain m zones_et1 (20*metre) (8*metre);
                    genere_humain m zones_et1 (20*metre+2) (8*metre);
                    genere_humain m zones_et1 (21*metre+1) (8*metre);
                    (*info haut*)
                    genere_humain m zones_et1 (17*metre+2) (8*metre-1);
                    genere_humain m zones_et1 (17*metre+1) (7*metre-1);
                    genere_humain m zones_et1 (17*metre+4) (8*metre-1);
                    genere_humain m zones_et1 (18*metre) (7*metre-1);
                    genere_humain m zones_et1 (18*metre+3) (8*metre-1);
                    genere_humain m zones_et1 (18*metre+2) (7*metre-1);
                    genere_humain m zones_et1 (19*metre+2) (8*metre-1);
                    genere_humain m zones_et1 (19*metre+1) (7*metre-1);
                    genere_humain m zones_et1 (19*metre+4) (8*metre-1);
                    genere_humain m zones_et1 (20*metre) (7*metre-1);
                    genere_humain m zones_et1 (20*metre+3) (8*metre-1);
                    genere_humain m zones_et1 (20*metre+2) (7*metre-1);
                    (*info milieu*)|] in
    (* humains salle de colle*)
    let h_sc=[|genere_humain m zones_et1 (40*metre +5) ( 39*metre+1);
                genere_humain m zones_et1 (40*metre +4) ( 35*metre+1);
                genere_humain m zones_et1 (40*metre +6) ( 35*metre+1);
                genere_humain m zones_et1 (40*metre +4) ( 36*metre+2);
                genere_humain m zones_et1 (40*metre +6) ( 36*metre+2);
                genere_humain m zones_et1 (40*metre +5) ( 31*metre+1);
                genere_humain m zones_et1 (40*metre +5) ( 23*metre+1);
                genere_humain m zones_et1 (40*metre +1) ( 41*metre+1);
                genere_humain m zones_et1 (43*metre -1) ( 42*metre+1);
                genere_humain m zones_et1 (40*metre +5) ( 43*metre-1);
                genere_humain m zones_et1 (40*metre +1) ( 37*metre+1);
                genere_humain m zones_et1 (43*metre -1) ( 38*metre+1);
                genere_humain m zones_et1 (40*metre +5) ( 39*metre-1);
                genere_humain m zones_et1 (40*metre +1) ( 33*metre+1);
                genere_humain m zones_et1 (43*metre -1) ( 34*metre+1);
                genere_humain m zones_et1 (40*metre +5) ( 35*metre-1);
                genere_humain m zones_et1 (40*metre +1) ( 25*metre+1);
                genere_humain m zones_et1 (43*metre -1) ( 26*metre+1);
                genere_humain m zones_et1 (40*metre +5) ( 27*metre-1)|] in
    (*amphithéâtre 3*)
    let h_a3=rempli_amphitheatre_haut m zones_et1 (30*metre ) (39*metre-2) 
        (37*metre +1) (43*metre) in
    (*amphithéâtre 2*)
    let h_a2=rempli_amphitheatre_droite m zones_et1 (29*metre +1) (38*metre-1) 
        (30*metre +1) (37*metre-1) in
    (*amphithéâtre 1*)
    let h_a1=rempli_amphitheatre_droite m zones_et1 (29*metre +1) (38*metre-1) 
        (22*metre +1) (30*metre-1) in
    (*salle de classe 3*)
    let h_s3=rempli_salle_classe_haut m zones_et1 (35*metre +1) (41*metre+2) 
        (14*metre +1) (21*metre-1) in
    (*chapelle*)
    let h_chapelle=rempli_amphitheatre_droite m zones_et1 (5*metre +1) (12*metre-1) 
        (12*metre +1) (21*metre-1) in
    (*cantine*)
    let h_cantine=rempli_cantine_droite m zones_et1 0 (12*metre-1) (23*metre +1) (40*metre-1) in
    (*salle de classe 2*)
    let h_s2=rempli_salle_classe_droite m  zones_et1 (25*metre +1) (33*metre -1) 
        (12*metre +1) (21*metre ) in
    (* humain a rajouter*)
    let h_aditionel=
        genere_pop_aleatoire_etablissement (5::4::6::7::8::10::11::12::13::14::15::16::19::[]) 
        m zones_et1 nb_additionnel in
    let obstacles=combine_tableaux [|obs ; obs_salle1_tables ; obs_salle_1prof ;
        obs_salle2_tables; obs_salle_2prof ; obs_cantine ; obs_chapelle ; 
        obs_autel ; obs_salle3_tables ; obs_salle3_prof; obs_amphi1 ; 
        obs_amphi1_prof ; obs_colle4 ; obs_colle3 ; obs_colle2 ; obs_toilettes;
        obs_colle1 ; obs_sport ; obs_cafet ; obs_parking ; obs_cdi ; obs_amphi2 ; 
        obs_amphi2_prof; obs_amphi3 ; obs_amphi3_prof|] in
    let population= combine_tableaux [|h_s1;h_profs;h_cdi;h_sc;h_a3;h_a2;h_a1;h_s3;
        h_chapelle;h_cantine;h_s2;h_aditionel|] in
    (m, obstacles, zones_et1, sorties, cara_sorties, population);;

(*simulation pour obtenir les différentes congestions*)
let zones_obstacles=[|([|0; 43*metre; 6*metre; 7*metre|],0);
                        ([|0; 43*metre; 19*metre; 20*metre|],0);
                        ([|0; 43*metre; 32*metre; 33*metre|],0);
                        ([|0; 43*metre; 39*metre; 43*metre|],0);
                        (*1ère rangée*)
                        ([|0; 6*metre-1; 0; 6*metre-1|],36);
                        ([|6*metre +1; 12*metre-1; 0; 6*metre-1|],37);
                        ([|12*metre +1; 18*metre-1; 0; 6*metre-1|],38);
                        ([|18*metre +1; 24*metre-1; 0; 6*metre-1|],39);
                        ([|24*metre +1; 30*metre-1; 0; 6*metre-1|],40);
                        ([|30*metre +1; 36*metre-1; 0; 6*metre-1|],41);
                        ([|36*metre +1; 42*metre-1; 0; 6*metre-1|],42);
                        (*2ème rangée*)
                        ([|0; 6*metre-1;7*metre+1; 13*metre-1|],29);
                        ([|6*metre +1; 12*metre-1; 7*metre+1; 13*metre-1|],30);
                        ([|12*metre +1; 18*metre-1; 7*metre+1; 13*metre-1|],31);
                        ([|18*metre +1; 24*metre-1; 7*metre+1; 13*metre-1|],32);
                        ([|24*metre +1; 30*metre-1; 7*metre+1; 13*metre-1|],33);
                        ([|30*metre +1; 36*metre-1; 7*metre+1; 13*metre-1|],34);
                        ([|36*metre +1; 42*metre-1; 7*metre+1; 13*metre-1|],35);
                        (*3ème rangée*)
                        ([|0; 6*metre-1;13*metre+1; 19*metre-1|],22);
                        ([|6*metre +1; 12*metre-1; 13*metre+1; 19*metre-1|],23);
                        ([|12*metre +1; 18*metre-1;13*metre+1; 19*metre-1|],24);
                        ([|18*metre +1; 24*metre-1; 13*metre+1; 19*metre-1|],25);
                        ([|24*metre +1; 30*metre-1; 13*metre+1; 19*metre-1|],26);
                        ([|30*metre +1; 36*metre-1; 13*metre+1; 19*metre-1|],27);
                        ([|36*metre +1; 42*metre-1; 13*metre+1; 19*metre-1|],28);
                        (*4ème rangée*)
                        ([|0; 6*metre-1;20*metre+1; 26*metre-1|],15);
                        ([|6*metre +1; 12*metre-1; 20*metre+1; 26*metre-1|],16);
                        ([|12*metre +1; 18*metre-1;20*metre+1; 26*metre-1|],17);
                        ([|18*metre +1; 24*metre-1; 20*metre+1; 26*metre-1|],18);
                        ([|24*metre +1; 30*metre-1; 20*metre+1; 26*metre-1|],19);
                        ([|30*metre +1; 36*metre-1; 20*metre+1; 26*metre-1|],20);
                        ([|36*metre +1; 42*metre-1; 20*metre+1; 26*metre-1|],21);
                        (*5ème rangée*)
                        ([|0; 6*metre-1;26*metre+1; 32*metre-1|],8);
                        ([|6*metre +1; 12*metre-1; 26*metre+1; 32*metre-1|],9);
                        ([|12*metre +1; 18*metre-1;26*metre+1; 32*metre-1|],10);
                        ([|18*metre +1; 24*metre-1; 26*metre+1; 32*metre-1|],11);
                        ([|24*metre +1; 30*metre-1; 26*metre+1; 32*metre-1|],12);
                        ([|30*metre +1; 36*metre-1; 26*metre+1; 32*metre-1|],13);
                        ([|36*metre +1; 42*metre-1; 26*metre+1; 32*metre-1|],14);
                        (*6ème rangée*)
                        ([|0; 6*metre-1; 33*metre+1; 39*metre-1|],1);
                        ([|6*metre +1; 12*metre-1; 33*metre+1; 39*metre-1|],2);
                        ([|12*metre +1; 18*metre-1; 33*metre+1; 39*metre-1|],3);
                        ([|18*metre +1; 24*metre-1; 33*metre+1; 39*metre-1|],4);
                        ([|24*metre +1; 30*metre-1; 33*metre+1; 39*metre-1|],5);
                        ([|30*metre +1; 36*metre-1; 33*metre+1; 39*metre-1|],6);
                        ([|36*metre +1; 42*metre-1; 33*metre+1; 39*metre-1|],7)|];;
                        
let murs_simu_obstacles=[| (0,2*metre, 6*metre, 6*metre);
                            (0,2*metre, 7*metre, 7*metre);
                            (0,2*metre, 19*metre, 19*metre);
                            (0,2*metre, 20*metre, 20*metre);
                            (0,2*metre, 32*metre, 32*metre);
                            (0,2*metre, 33*metre,33*metre);

                            (4*metre,8*metre, 6*metre, 6*metre);
                            (4*metre,8*metre, 7*metre, 7*metre);
                            (4*metre,8*metre, 19*metre, 19*metre);
                            (4*metre,8*metre, 20*metre, 20*metre);
                            (4*metre,8*metre, 32*metre, 32*metre);
                            (4*metre,8*metre, 33*metre,33*metre);

                            (9*metre,14*metre, 6*metre, 6*metre);
                            (9*metre,14*metre, 7*metre, 7*metre);
                            (9*metre,14*metre, 19*metre, 19*metre);
                            (9*metre,14*metre, 20*metre, 20*metre);
                            (9*metre,14*metre, 32*metre, 32*metre);
                            (9*metre,14*metre, 33*metre,33*metre);

                            (17*metre,20*metre, 6*metre, 6*metre);
                            (17*metre,20*metre, 7*metre, 7*metre);
                            (17*metre,20*metre, 19*metre, 19*metre);
                            (17*metre,20*metre, 20*metre, 20*metre);
                            (17*metre,20*metre, 32*metre, 32*metre);
                            (17*metre,20*metre, 33*metre,33*metre);
                            (22*metre,26*metre, 6*metre, 6*metre);
                            (22*metre,26*metre, 7*metre, 7*metre);
                            (22*metre,26*metre, 19*metre, 19*metre);
                            (22*metre,26*metre, 20*metre, 20*metre);
                            (22*metre,26*metre, 32*metre, 32*metre);
                            (22*metre,26*metre, 33*metre,33*metre);

                            (27*metre,32*metre, 6*metre, 6*metre);
                            (27*metre,32*metre, 7*metre, 7*metre);
                            (27*metre,32*metre, 19*metre, 19*metre);
                            (27*metre,32*metre, 20*metre, 20*metre);
                            (27*metre,32*metre, 32*metre, 32*metre);
                            (27*metre,32*metre, 33*metre,33*metre);

                            (35*metre,38*metre, 6*metre, 6*metre);
                            (35*metre,38*metre, 7*metre, 7*metre);
                            (35*metre,38*metre, 19*metre, 19*metre);
                            (35*metre,38*metre, 20*metre, 20*metre);
                            (35*metre,38*metre, 32*metre, 32*metre);
                            (35*metre,38*metre, 33*metre,33*metre);

                            (40*metre,42*metre, 6*metre, 6*metre);
                            (40*metre,42*metre, 7*metre, 7*metre);
                            (40*metre,42*metre, 19*metre, 19*metre);
                            (40*metre,42*metre, 20*metre, 20*metre);
                            (40*metre,42*metre, 32*metre, 32*metre);
                            (40*metre,42*metre, 33*metre,33*metre);
                            (*murs du milieux*)
                            (6*metre, 6*metre, 0, 6*metre -1);
                            (6*metre, 6*metre, 7*metre+1, 13*metre -1);
                            (6*metre, 6*metre, 13*metre+1, 19*metre -1);
                            (6*metre, 6*metre, 20*metre+1, 26*metre -1);
                            (6*metre, 6*metre, 26*metre+1, 32*metre -1);
                            (6*metre, 6*metre, 33*metre+1, 39*metre-1);

                            (12*metre, 12*metre, 0, 6*metre -1);
                            (12*metre, 12*metre, 7*metre+1, 13*metre -1);
                            (12*metre, 12*metre, 13*metre+1, 19*metre -1);
                            (12*metre, 12*metre, 20*metre+1, 26*metre -1);
                            (12*metre, 12*metre, 26*metre+1, 32*metre -1);
                            (12*metre, 12*metre, 33*metre+1,39*metre-1);

                            (18*metre, 18*metre, 0, 6*metre -1);
                            (18*metre, 18*metre, 7*metre+1, 13*metre -1);
                            (18*metre, 18*metre, 13*metre+1, 19*metre -1);
                            (18*metre, 18*metre, 20*metre+1, 26*metre -1);
                            (18*metre, 18*metre, 26*metre+1, 32*metre -1);
                            (18*metre, 18*metre, 33*metre+1, 39*metre-1);

                            (24*metre, 24*metre, 0, 6*metre -1);
                            (24*metre, 24*metre, 7*metre+1, 13*metre -1);
                            (24*metre, 24*metre, 13*metre+1, 19*metre -1);
                            (24*metre, 24*metre, 20*metre+1, 26*metre -1);
                            (24*metre, 24*metre, 26*metre+1, 32*metre -1);
                            (24*metre, 24*metre, 33*metre+1,39*metre-1);

                            (30*metre, 30*metre, 0, 6*metre -1);
                            (30*metre, 30*metre, 7*metre+1, 13*metre -1);
                            (30*metre, 30*metre, 13*metre+1, 19*metre -1);
                            (30*metre, 30*metre, 20*metre+1, 26*metre -1);
                            (30*metre, 30*metre, 26*metre+1, 32*metre -1);
                            (30*metre, 30*metre, 33*metre+1, 39*metre-1);

                            (36*metre, 36*metre, 0, 6*metre -1);
                            (36*metre, 36*metre, 7*metre+1, 13*metre -1);
                            (36*metre, 36*metre, 13*metre+1, 19*metre -1);
                            (36*metre, 36*metre, 20*metre+1, 26*metre -1);
                            (36*metre, 36*metre, 26*metre+1, 32*metre -1);
                            (36*metre, 36*metre, 33*metre+1, 39*metre-1);

                            (42*metre, 42*metre, 0, 6*metre -1);
                            (42*metre, 42*metre, 7*metre+1, 19*metre -1);
                            (42*metre, 42*metre, 20*metre+1, 32*metre -1);
                            (42*metre, 42*metre, 33*metre+1, 39*metre-1);
                            (*séparation*)
                            (0, 42*metre -1, 13*metre, 13*metre);
                            (0, 42*metre -1, 26*metre, 26*metre);
                            (0, 42*metre -1, 39*metre, 39*metre)|];;
                            
let portes_obstacles=[| [|(0, 43*metre, 6*metre +1, 7*metre -1);
                            (0, 43*metre, 19*metre +1, 20*metre -1);
                            (0, 43*metre, 32*metre +1, 33*metre -1)|];
                            (*1ère rangée*)
                            [|(2*metre+1, 4*metre-1, 33*metre, 33*metre)|];
                            [|(8*metre+1, 9*metre-1, 33*metre, 33*metre)|];
                            [|(14*metre+1, 17*metre-1, 33*metre, 33*metre)|];
                            [|(20*metre+1, 22*metre-1, 33*metre, 33*metre)|];
                            [|(26*metre+1, 27*metre-1, 33*metre, 33*metre)|];
                            [|(32*metre+1, 35*metre-1, 33*metre, 33*metre)|];
                            [|(38*metre+1, 40*metre-1, 33*metre, 33*metre)|];
                            (*2ème rangée*)
                            [|(2*metre+1, 4*metre-1, 32*metre, 32*metre)|];
                            [|(8*metre+1, 9*metre-1, 32*metre, 32*metre)|];
                            [|(14*metre+1, 17*metre-1, 32*metre, 32*metre)|];
                            [|(20*metre+1, 22*metre-1, 32*metre, 32*metre)|];
                            [|(26*metre+1, 27*metre-1, 32*metre, 32*metre)|];
                            [|(32*metre+1, 35*metre-1, 32*metre, 32*metre)|];
                            [|(38*metre+1, 40*metre-1, 32*metre, 32*metre)|];
                            (*3ème rangée*)
                            [|(2*metre+1, 4*metre-1, 20*metre, 20*metre)|];
                            [|(8*metre+1, 9*metre-1, 20*metre, 20*metre)|];
                            [|(14*metre+1, 17*metre-1, 20*metre, 20*metre)|];
                            [|(20*metre+1, 22*metre-1, 20*metre, 20*metre)|];
                            [|(26*metre+1, 27*metre-1, 20*metre, 20*metre)|];
                            [|(32*metre+1, 35*metre-1, 20*metre, 20*metre)|];
                            [|(38*metre+1, 40*metre-1, 20*metre, 20*metre)|];
                            (*4ème rangée*)
                            [|(2*metre+1, 4*metre-1, 19*metre, 19*metre)|];
                            [|(8*metre+1, 9*metre-1, 19*metre, 19*metre)|];
                            [|(14*metre+1, 17*metre-1, 19*metre, 19*metre)|];
                            [|(20*metre+1, 22*metre-1, 19*metre, 19*metre)|];
                            [|(26*metre+1, 27*metre-1, 19*metre, 19*metre)|];
                            [|(32*metre+1, 35*metre-1, 19*metre, 19*metre)|];
                            [|(38*metre+1, 40*metre-1, 19*metre, 19*metre)|];
                            (*5ème rangée*)
                            [|(2*metre+1, 4*metre-1, 7*metre, 7*metre)|];
                            [|(8*metre+1, 9*metre-1, 7*metre, 7*metre)|];
                            [|(14*metre+1, 17*metre-1, 7*metre, 7*metre)|];
                            [|(20*metre+1, 22*metre-1, 7*metre, 7*metre)|];
                            [|(26*metre+1, 27*metre-1, 7*metre, 7*metre)|];
                            [|(32*metre+1, 35*metre-1, 7*metre, 7*metre)|];
                            [|(38*metre+1, 40*metre-1, 7*metre, 7*metre)|];
                            (*6ème rangée*)
                            [|(2*metre+1, 4*metre-1, 6*metre, 6*metre)|];
                            [|(8*metre+1, 9*metre-1, 6*metre, 6*metre)|];
                            [|(14*metre+1, 17*metre-1, 6*metre, 6*metre)|];
                            [|(20*metre+1, 22*metre-1, 6*metre, 6*metre)|];
                            [|(26*metre+1, 27*metre-1, 6*metre, 6*metre)|];
                            [|(32*metre+1, 35*metre-1, 6*metre, 6*metre)|];
                            [|(38*metre+1, 40*metre-1, 6*metre, 6*metre)|]|];;
                            
let genere_porte_sortie_simu_obs sorties mapping=
    let n=Array.length sorties in
    let s= Array.make_matrix n 4 (0,0,0) and 
    cara_s= Array.make_matrix n 4 (0,0,0,0,0) in
    (*zone 0*)
    let xm0, xM0, ym0, yM0=sorties.(0).(0) in
    let xm1, xM1, ym1, yM1=sorties.(0).(1) in
    let xm2, xM2, ym2, yM2=sorties.(0).(2) in
    s.(0)<-[| genere_sortie xM0 xm0 yM0 ym0 mapping (-1);
        genere_sortie xM1 xm1 yM1 ym1 mapping (-1);
        genere_sortie xM2 xm2 yM2 ym2 mapping (-1)|];
    cara_s.(0)<-[|mapping.(xm0).(ym0);mapping.(xm1).(ym1);
        mapping.(xm2).(ym2)|];
    for i=1 to n-1 do 
        (* on aura toujours 1 sortie par pièce*)
        let xm, xM, ym, yM = sorties.(i).(0) in
        genere_porte xm xM ym yM mapping;
        s.(i)<-[|(xm,ym, 0)|];
        cara_s.(i)<-[|mapping.(xm).(ym)|];
    done;
    (s, cara_s);;
    
let simu_obstacles_poteaux()=
    let m= Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs_murs=mur_etablissement_scolaire m murs_simu_obstacles in
    let s, c_s= genere_porte_sortie_simu_obs portes_obstacles m in
    let obs_zones=[| genere_obstacle_cercle m [|3*metre;6*metre -3|] 1;
                        genere_obstacle_cercle m [|9*metre-1;6*metre -3|] 1;
                        genere_obstacle_cercle m [|15*metre;6*metre -3|] 1;
                        (*on élargit*)
                        genere_obstacle_cercle m [|21*metre-1;6*metre -4|] 2;
                        genere_obstacle_cercle m [|27*metre-1;6*metre -4|] 2;
                        genere_obstacle_cercle m [|33*metre-1;6*metre -4|] 2;
                        (*on réélargit*)
                        genere_obstacle_cercle m [|39*metre;6*metre -5|] 3;
                        (*on recule*)
                        genere_obstacle_cercle m [|3*metre;7*metre +4|] 1;
                        genere_obstacle_cercle m [|9*metre-1;7*metre +4|] 1;
                        genere_obstacle_cercle m [|15*metre;7*metre +4|] 1;
                        (*on élargit*)
                        genere_obstacle_cercle m [|21*metre-1;7*metre +5|] 2;
                        genere_obstacle_cercle m [|27*metre-1;7*metre +5|] 2;
                        genere_obstacle_cercle m [|33*metre-1;7*metre +5|] 2;
                        (*on réélargit*)
                        genere_obstacle_cercle m [|39*metre;7*metre +6|] 3;
                        (*on recule*)
                        genere_obstacle_cercle m [|3*metre;19*metre -5|] 1;
                        genere_obstacle_cercle m [|9*metre-1;19*metre -5|] 1;
                        genere_obstacle_cercle m [|15*metre;19*metre -5|] 1;
                        (*on élargit*)
                        genere_obstacle_cercle m [|21*metre-1;19*metre -6|] 2;
                        genere_obstacle_cercle m [|27*metre-1;19*metre -6|] 2;
                        genere_obstacle_cercle m [|33*metre-1;19*metre -6|] 2;
                        (*on réélargit*)
                        genere_obstacle_cercle m [|39*metre;19*metre -7|] 3;
                        
                        genere_obstacle_cercle m [|3*metre;20*metre +6|] 4;
                        genere_obstacle_cercle m [|9*metre-1;20*metre +6|] 4;
                        genere_obstacle_cercle m [|15*metre;20*metre +6|] 4;
                        (*on élargit*)
                        genere_obstacle_cercle m [|21*metre-1;20*metre +7|] 5;
                        genere_obstacle_cercle m [|27*metre-1;20*metre +7|] 5;
                        genere_obstacle_cercle m [|33*metre-1;20*metre +7|] 5;
                        (*on réélargit*)
                        genere_obstacle_cercle m [|39*metre;20*metre +8|] 6;
                        (*on recule*)
                        genere_obstacle_cercle m [|3*metre;32*metre -7|] 4;
                        genere_obstacle_cercle m [|9*metre-1;32*metre -7|] 4;
                        genere_obstacle_cercle m [|15*metre;32*metre -7|] 4;
                        (*on élargit*)
                        genere_obstacle_cercle m [|21*metre-1;32*metre -8|] 5;
                        genere_obstacle_cercle m [|27*metre-1;32*metre -8|] 5;
                        genere_obstacle_cercle m [|33*metre-1;32*metre -8|] 5;
                        (*on réélargit*)
                        genere_obstacle_cercle m [|39*metre;32*metre -9|] 6;
                        (*on recule*)
                        genere_obstacle_cercle m [|3*metre;33*metre +8|] 4;
                        genere_obstacle_cercle m [|9*metre-1;33*metre +8|] 4;
                        genere_obstacle_cercle m [|15*metre;33*metre +8|] 4;
                        (*on élargit*)
                        genere_obstacle_cercle m [|21*metre-1;33*metre +9|] 5;
                        genere_obstacle_cercle m [|27*metre-1;33*metre +9|] 5;
                        genere_obstacle_cercle m [|33*metre-1;33*metre +9|] 5;
                        (*on réélargit*)
                        genere_obstacle_cercle m [|39*metre;33*metre +10|] 6|] in
    let obs_total=combine_tableaux [| obs_murs; obs_zones|] in
    let z=zones_obstacles in
    let h= Array.make 42 [||] in
    Array.iteri (fun i x -> h.(i)<-genere_pop_aleatoire_dans_zone ((i+1)::[]) m z  50) h;
    let population=combine_tableaux h in
    (m, obs_total,z, s, c_s, population);;
    
let simu_obstacles_panneau()=
    let m= Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs_murs=mur_etablissement_scolaire m murs_simu_obstacles in
    let s, c_s= genere_porte_sortie_simu_obs portes_obstacles m in
    let obs_zones=[| (3*metre, 3*metre+1, 6*metre -2, 6*metre -2);
                        (9*metre-1, 9*metre, 6*metre -2, 6*metre -2);
                        (15*metre, 15*metre+1, 6*metre -2, 6*metre -2);
                        (*on élargit*)
                        (21*metre-1, 21*metre+1, 6*metre -2, 6*metre -2);
                        (27*metre-2, 27*metre, 6*metre -2, 6*metre -2);
                        (33*metre-1, 33*metre+1, 6*metre -2, 6*metre -2);
                        (*on réélargit*)
                        (39*metre-1, 39*metre+2, 6*metre -2, 6*metre -2);
                        (*on recule*)
                        (3*metre, 3*metre+1, 7*metre +3, 7*metre +3);
                        (9*metre-1, 9*metre, 7*metre +3, 7*metre +3);
                        (15*metre, 15*metre+1, 7*metre +3, 7*metre +3);
                        (*on élargit*)
                        (21*metre-1, 21*metre+1, 7*metre +3, 7*metre +3);
                        (27*metre-2, 27*metre, 7*metre +3, 7*metre +3);
                        (33*metre-1, 33*metre+1, 7*metre +3, 7*metre +3);
                        (*on réélargit*)
                        (39*metre-1, 39*metre+2, 7*metre +3, 7*metre +3);
                        (*on recule*)
                        (3*metre, 3*metre+1, 19*metre -4, 19*metre -4);
                        (9*metre-1, 9*metre, 19*metre -4, 19*metre -4);
                        (15*metre, 15*metre+1,19*metre -4, 19*metre -4);
                        (*on élargit*)
                        (21*metre-1, 21*metre+1, 19*metre -4, 19*metre -4);
                        (27*metre-2, 27*metre, 19*metre -4, 19*metre -4);
                        (33*metre-1, 33*metre+1, 19*metre -4, 19*metre -4);
                        (*on réélargit*)
                        (39*metre-1, 39*metre+1, 19*metre -4, 19*metre -4);

                        (3*metre-2, 3*metre+1, 20*metre +2, 20*metre +2);
                        (9*metre-3, 9*metre+1, 20*metre +2, 20*metre +2);
                        (15*metre-2, 15*metre+2, 20*metre +2, 20*metre +2);
                        (*on élargit*)
                        (21*metre-2, 21*metre+3, 20*metre +2, 20*metre +2);
                        (27*metre-3, 27*metre+2, 20*metre +2, 20*metre +2);
                        (33*metre-2, 33*metre+3, 20*metre +2, 20*metre +2);
                        (*on réélargit*)
                        (39*metre-3, 39*metre+3, 20*metre +2, 20*metre +2);
                        (*on recule*)
                        (3*metre-2, 3*metre+2, 32*metre -3, 32*metre -3);
                        (9*metre-3, 9*metre+1, 32*metre -3, 32*metre -3);
                        (15*metre-2, 15*metre+2, 32*metre -3, 32*metre -3);
                        (*on élargit*)
                        (21*metre-2, 21*metre+3, 32*metre -3, 32*metre -3);
                        (27*metre-3, 27*metre+2, 32*metre -3, 32*metre -3);
                        (33*metre-2, 33*metre+3, 32*metre -3, 32*metre -3);
                        (*on réélargit*)
                        (39*metre-3, 39*metre+3, 32*metre -3, 32*metre -3);
                        (*on recule*)
                        (3*metre-2, 3*metre+2, 33*metre +4, 33*metre +4);
                        (9*metre-3, 9*metre+1, 33*metre +4, 33*metre +4);
                        (15*metre-2, 15*metre+2, 33*metre +4, 33*metre +4);
                        (*on élargit*)
                        (21*metre-2, 21*metre+3, 33*metre +4, 33*metre +4);
                        (27*metre-3, 27*metre+2, 33*metre +4, 33*metre +4);
                        (33*metre-2, 33*metre+3, 33*metre +4, 33*metre +4);
                        (*on réélargit*)
                        (39*metre-3, 39*metre+3, 33*metre +4, 33*metre +4)|] in
    let obs_total=combine_tableaux [| obs_murs; mur_etablissement_scolaire m obs_zones|] in
    let z=zones_obstacles in
    let h= Array.make 42 [||] in
    Array.iteri (fun i x -> h.(i)<-genere_pop_aleatoire_dans_zone ((i+1)::[]) m z  50) h;
    let population=combine_tableaux h in
    (m, obs_total,z, s, c_s, population);;
(*fin des simulations pour l'observation des congestions*)

(*simulations pour observer les fluidification*)
let murs_fluidification = [| (*milieu*)
                                (0, 6*metre -1, 0, 0);
                                (0, 6*metre -1, 6*metre, 6*metre);
                                (0, 6*metre -1, 12*metre, 12*metre);
                                (0, 6*metre -1, 18*metre, 18*metre);
                                (0, 6*metre -1, 24*metre, 24*metre);
                                (0, 6*metre -1, 30*metre, 30*metre);
                                (0, 6*metre -1, 36*metre, 36*metre);
                                (0, 6*metre -1, 42*metre, 43*metre);

                                (14*metre+1, 20*metre -1, 0, 0);
                                (14*metre+1, 20*metre -1, 6*metre, 6*metre);
                                (14*metre+1, 20*metre -1, 12*metre, 12*metre);
                                (14*metre+1, 20*metre -1, 18*metre, 18*metre);
                                (14*metre+1, 20*metre -1, 24*metre, 24*metre);
                                (14*metre+1, 20*metre -1, 30*metre, 30*metre);
                                (14*metre+1, 20*metre -1, 36*metre, 36*metre);
                                (14*metre+1, 20*metre -1, 42*metre, 43*metre);

                                (28*metre+1, 34*metre -1, 0, 0);
                                (28*metre+1, 34*metre -1, 6*metre, 6*metre);
                                (28*metre+1, 34*metre -1, 12*metre, 12*metre);
                                (28*metre+1, 34*metre -1, 18*metre, 18*metre);
                                (28*metre+1, 34*metre -1, 24*metre, 24*metre);
                                (28*metre+1, 34*metre -1, 30*metre, 30*metre);
                                (28*metre+1, 34*metre -1, 36*metre, 36*metre);
                                (28*metre+1, 34*metre -1, 42*metre, 43*metre);
                                (*portes*)
                                (6*metre, 6*metre, 0, 2*metre);
                                (6*metre, 6*metre, 4*metre, 7*metre);
                                (6*metre, 6*metre, 10*metre, 15*metre);
                                (6*metre, 6*metre, 16*metre, 20*metre);
                                (6*metre, 6*metre, 22*metre, 25*metre);
                                (6*metre, 6*metre, 28*metre, 33*metre);
                                (6*metre, 6*metre, 34*metre, 38*metre);
                                (6*metre, 6*metre, 40*metre, 43*metre);

                                (20*metre, 20*metre, 0, 2*metre);
                                (20*metre, 20*metre, 4*metre, 7*metre);
                                (20*metre, 20*metre, 10*metre, 15*metre);
                                (20*metre, 20*metre, 16*metre, 20*metre);
                                (20*metre, 20*metre, 22*metre, 25*metre);
                                (20*metre, 20*metre, 28*metre, 33*metre);
                                (20*metre, 20*metre, 34*metre, 38*metre);
                                (20*metre, 20*metre, 40*metre, 43*metre);
                                (34*metre, 34*metre, 0, 2*metre);
                                (34*metre, 34*metre, 4*metre, 7*metre);
                                (34*metre, 34*metre, 10*metre, 15*metre);
                                (34*metre, 34*metre, 16*metre, 20*metre);
                                (34*metre, 34*metre, 22*metre, 25*metre);
                                (34*metre, 34*metre, 28*metre, 33*metre);
                                (34*metre, 34*metre, 34*metre, 38*metre);
                                (34*metre, 34*metre, 40*metre, 43*metre);

                                (*séparation*)
                                (14*metre, 14*metre, 0, 43*metre);
                                (28*metre, 28*metre, 0, 43*metre)|];;
                                
let sorties_fluidification = [| (*zone 0*)
                                [|(14*metre -1, 14*metre -1, 0, 43*metre);
                                (28*metre -1, 28*metre -1, 0, 43*metre);
                                (42*metre -1, 42*metre -1, 0, 43*metre)|];
                                (*1ère rangée*)
                                [|(6*metre, 6*metre, 2*metre +1, 4*metre -1)|];
                                [|(6*metre, 6*metre, 7*metre +1, 10*metre -1)|];
                                [|(6*metre, 6*metre, 15*metre +1, 16*metre -1)|];
                                [|(6*metre, 6*metre, 20*metre +1, 22*metre -1)|];
                                [|(6*metre, 6*metre, 25*metre +1, 28*metre -1)|];
                                [|(6*metre, 6*metre, 33*metre +1, 34*metre -1)|];
                                [|(6*metre, 6*metre, 38*metre +1, 40*metre -1)|];
                                (*2ème rangée*)
                                [|(20*metre, 20*metre, 2*metre +1, 4*metre -1)|];
                                [|(20*metre, 20*metre, 7*metre +1, 10*metre -1)|];
                                [|(20*metre, 20*metre, 15*metre +1, 16*metre -1)|];
                                [|(20*metre, 20*metre, 20*metre +1, 22*metre -1)|];
                                [|(20*metre, 20*metre, 25*metre +1, 28*metre -1)|];
                                [|(20*metre, 20*metre, 33*metre +1, 34*metre -1)|];
                                [|(20*metre, 20*metre, 38*metre +1, 40*metre -1)|];
                                (*3ème rangée*)
                                [|(34*metre, 34*metre, 2*metre +1, 4*metre -1)|];
                                [|(34*metre, 34*metre, 7*metre +1, 10*metre -1)|];
                                [|(34*metre, 34*metre, 15*metre +1, 16*metre -1)|];
                                [|(34*metre, 34*metre, 20*metre +1, 22*metre -1)|];
                                [|(34*metre, 34*metre, 25*metre +1, 28*metre -1)|];
                                [|(34*metre, 34*metre, 33*metre +1, 34*metre -1)|];
                                [|(34*metre, 34*metre, 38*metre +1, 40*metre -1)|]|];;
                                
let panneau_fluidification1= [| (6*metre -2, 6*metre -2, 3*metre, 3*metre +1);
                                (6*metre -2, 6*metre -2, 8*metre, 8*metre +1);
                                (6*metre -2, 6*metre -2, 15*metre, 15*metre +1);
                                (*on élargit*)
                                (6*metre -2, 6*metre -2, 20*metre+1, 21*metre );
                                (6*metre -2, 6*metre -2, 26*metre, 26*metre +2);
                                (6*metre -2, 6*metre -2, 33*metre, 34*metre-1 );
                                (*on élargit*)
                                (6*metre -2, 6*metre -2, 38*metre+1, 39*metre +1);
                                (*on recule*)
                                (20*metre -3, 20*metre -3, 3*metre, 3*metre +1);
                                (20*metre -3, 20*metre -3, 8*metre, 8*metre +1);
                                (20*metre -3, 20*metre -3, 15*metre, 15*metre +1);
                                (*on élargit*)
                                (20*metre -3, 20*metre -3, 20*metre+1, 21*metre );
                                (20*metre -3, 20*metre -3, 26*metre, 27*metre -1);
                                (20*metre -3, 20*metre -3, 33*metre, 34*metre -1);
                                (*on élargit*)
                                (20*metre -3, 20*metre -3, 38*metre+1, 39*metre +1);
                                (*on recule*)
                                (34*metre -4, 34*metre -4, 3*metre, 3*metre +1);
                                (34*metre -4, 34*metre -4, 8*metre, 8*metre +1);
                                (34*metre -4, 34*metre -4, 15*metre, 15*metre +1);
                                (*on élargit*)
                                (34*metre -4, 34*metre -4, 20*metre+1, 21*metre );
                                (34*metre -4, 34*metre -4, 26*metre, 27*metre -1);
                                (34*metre -4, 34*metre -4, 33*metre, 34*metre -1);
                                (*on élargit*)
                                (34*metre -4, 34*metre -4, 38*metre+1, 39*metre +1)|];;
                                
let panneau_fluidification2= [| (6*metre -2, 6*metre -2, 2*metre +1, 4*metre -1);
                                (6*metre -2, 6*metre -2, 7*metre+2, 10*metre -3);
                                (6*metre -2, 6*metre -2, 15*metre, 16*metre +1);
                                (*on élargit*)
                                (6*metre -2, 6*metre -2, 20*metre, 22*metre -1);
                                (6*metre -2, 6*metre -2, 25*metre+2, 27*metre +1);
                                (6*metre -2, 6*metre -2, 33*metre-1, 34*metre +1);
                                (*on élargit*)
                                (6*metre -2, 6*metre -2, 38*metre, 40*metre );
                                (*on recule*)
                                (20*metre -3, 20*metre -3,2*metre +1, 4*metre -1);
                                (20*metre -3, 20*metre -3,7*metre+2, 10*metre -3);
                                (20*metre -3, 20*metre -3, 15*metre, 16*metre +1);
                                (*on élargit*)
                                (20*metre -3, 20*metre -3,  20*metre, 22*metre -1);
                                (20*metre -3, 20*metre -3, 25*metre+2, 27*metre +1);
                                (20*metre -3, 20*metre -3, 33*metre-1, 34*metre +1);
                                (*on élargit*)
                                (20*metre -3, 20*metre -3,  38*metre, 40*metre );
                                (*on recule*)
                                (34*metre -4, 34*metre -4, 2*metre +1, 4*metre -1);
                                (34*metre -4, 34*metre -4, 7*metre+2, 10*metre -3);
                                (34*metre -4, 34*metre -4, 15*metre, 16*metre +1);
                                (*on élargit*)
                                (34*metre -4, 34*metre -4,  20*metre, 22*metre -1);
                                (34*metre -4, 34*metre -4, 25*metre+2, 27*metre +1);
                                (34*metre -4, 34*metre -4, 33*metre-1, 34*metre +1);
                                (*on élargit*)
                                (34*metre -4, 34*metre -4,  38*metre, 40*metre )|];;
                                
let genere_poteaux_car m poteaux =
    let n=Array.length poteaux in
    let tab_obstacles= Array.make n (0,0) in
    for i=0 to n-1 do
        let x,y,r=poteaux.(i) in
        tab_obstacles.(i)<- genere_obstacle_cercle m [|x;y|] r
    done;
    tab_obstacles;;
    
let poteau_fluidification1 = [| (6*metre -3, 3*metre, 1);
                                (6*metre -3, 8*metre+1, 1);
                                (6*metre -3, 15*metre+1, 1);
                                (*on élargit*)
                                (6*metre -4, 21*metre, 2);
                                (6*metre -4, 26*metre+1, 2);
                                (6*metre -4, 33*metre+1, 2);
                                (*on élargit*)
                                (6*metre -5, 39*metre, 3);
                                (*on recule*)
                                (20*metre -4, 3*metre, 1);
                                (20*metre -4, 8*metre+1, 1);
                                (20*metre -4, 15*metre+1, 1);
                                (*on élargit*)
                                (20*metre -5, 21*metre, 2);
                                (20*metre -5, 26*metre+1, 2);
                                (20*metre -5, 33*metre+1, 2);
                                (*on élargit*)
                                (20*metre -6, 39*metre, 3);
                                (*on recule*)
                                (34*metre -5, 3*metre, 1);
                                (34*metre -5, 8*metre+1, 1);
                                (34*metre -5, 15*metre+1, 1);
                                (*on élargit*)
                                (34*metre -6, 21*metre, 2);
                                (34*metre -6, 26*metre+1, 2);
                                (34*metre -6, 33*metre+1, 2);
                                (*on élargit*)
                                (34*metre -6, 39*metre, 3)|];;
                                
let poteau_fluidification2 = [| (6*metre -6, 3*metre, 4);
                                (6*metre -6, 8*metre+1, 4);
                                (6*metre -6, 15*metre+1, 4);
                                (*on élargit*)
                                (6*metre -7, 21*metre, 5);
                                (6*metre -7, 26*metre+1, 5);
                                (6*metre -7, 33*metre+1, 5);
                                (*on élargit*)
                                (6*metre -8, 39*metre, 6);
                                (*on recule*)
                                (20*metre -7, 3*metre, 4);
                                (20*metre -7, 8*metre+1, 4);
                                (20*metre -7, 15*metre+1, 4);
                                (*on élargit*)
                                (20*metre -8, 21*metre, 5);
                                (20*metre -8, 26*metre+1, 5);
                                (20*metre -8, 33*metre+1, 5);
                                (*on élargit*)
                                (20*metre -9, 39*metre, 6);
                                (*on recule*)
                                (34*metre -8, 3*metre, 4);
                                (34*metre -8, 8*metre+1, 4);
                                (34*metre -8, 15*metre+1, 4);
                                (*on élargit*)
                                (34*metre -9, 21*metre, 5);
                                (34*metre -9, 26*metre+1, 5);
                                (34*metre -9, 33*metre+1, 5);
                                (*on élargit*)
                                (34*metre -10, 39*metre, 6)|];;
                                
let zones_fluidification= [| (*zone 0*)
                            ([|6*metre; 14*metre; 0; 43*metre|],0);
                            ([|20*metre; 28*metre; 0; 43*metre|],0);
                            ([|34*metre; 43*metre; 0; 43*metre|],0);
                            (*1ère rangée*)
                            ([|0; 6*metre-1; 0; 6*metre-1|],1);
                            ([|0; 6*metre-1; 6*metre+1; 12*metre-1|],2);
                            ([|0; 6*metre-1; 12*metre+1; 18*metre-1|],3);
                            ([|0; 6*metre-1; 18*metre+1; 24*metre-1|],4);
                            ([|0; 6*metre-1; 24*metre+1; 30*metre-1|],5);
                            ([|0; 6*metre-1; 30*metre+1; 36*metre-1|],6);
                            ([|0; 6*metre-1; 36*metre+1; 42*metre-1|],7);
                            (*2ème rangée*)
                            ([|14*metre+1; 20*metre-1; 0; 6*metre-1|],8);
                            ([|14*metre+1; 20*metre-1; 6*metre+1; 12*metre-1|],9);
                            ([|14*metre+1; 20*metre-1; 12*metre+1; 18*metre-1|],10);
                            ([|14*metre+1; 20*metre-1; 18*metre+1; 24*metre-1|],11);
                            ([|14*metre+1; 20*metre-1; 24*metre+1; 30*metre-1|],12);
                            ([|14*metre+1; 20*metre-1; 30*metre+1; 36*metre-1|],13);
                            ([|14*metre+1; 20*metre-1; 36*metre+1; 42*metre-1|],14);
                            (*3ème rangée*)
                            ([|28*metre+1; 34*metre-1; 0; 6*metre-1|],15);
                            ([|28*metre+1; 34*metre-1; 6*metre+1; 12*metre-1|],16);
                            ([|28*metre+1; 34*metre-1; 12*metre+1; 18*metre-1|],17);
                            ([|28*metre+1; 34*metre-1; 18*metre+1; 24*metre-1|],18);
                            ([|28*metre+1; 34*metre-1; 24*metre+1; 30*metre-1|],19);
                            ([|28*metre+1; 34*metre-1; 30*metre+1; 36*metre-1|],20);
                            ([|28*metre+1; 34*metre-1; 36*metre+1; 42*metre-1|],21)|];;
                            
let simu_fluidification_panneau1 ()=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        mur_etablissement_scolaire m panneau_fluidification1|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let h= Array.make 21 [||] in
    Array.iteri (fun i x -> h.(i)<-genere_pop_aleatoire_dans_zone ((i+1)::[]) m z  50) h;
    let population=combine_tableaux h in
    (m, obs,z, s, c_s, population);;
	
let simu_fluidification_panneau2 ()=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        mur_etablissement_scolaire m panneau_fluidification2|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let h= Array.make 21 [||] in
    Array.iteri (fun i x -> h.(i)<-genere_pop_aleatoire_dans_zone ((i+1)::[]) m z  50) h;
    let population=combine_tableaux h in
    (m, obs,z, s, c_s, population);;
    
let simu_fluidification_poteau1 ()=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        genere_poteaux_car m poteau_fluidification1|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let h= Array.make 21 [||] in
    Array.iteri (fun i x -> h.(i)<-genere_pop_aleatoire_dans_zone ((i+1)::[]) m z  50) h;
    let population=combine_tableaux h in
    (m, obs,z, s, c_s, population);;

let simu_fluidification_poteau2 ()=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        genere_poteaux_car m poteau_fluidification2|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let h= Array.make 21 [||] in
    Array.iteri (fun i x -> h.(i)<-genere_pop_aleatoire_dans_zone ((i+1)::[]) m z  50) h;
    let population=combine_tableaux h in
    (m, obs,z, s, c_s, population);;
    
(*remplissage d'une seule zone*)
let simu_fluidification_panneau1_z k=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        mur_etablissement_scolaire m panneau_fluidification1|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let population=genere_pop_aleatoire_dans_zone (k::[]) m z  50 in
    (m, obs,z, s, c_s, population);;
	
let simu_fluidification_panneau2_z k=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        mur_etablissement_scolaire m panneau_fluidification2|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let population=genere_pop_aleatoire_dans_zone (k::[]) m z  50 in
    (m, obs,z, s, c_s, population);;
    
let simu_fluidification_poteau1_z k=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        genere_poteaux_car m poteau_fluidification1|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let population=genere_pop_aleatoire_dans_zone (k::[]) m z  50 in
    (m, obs,z, s, c_s, population);;

let simu_fluidification_poteau2_z k=
    let m=Array.make_matrix dimx dimy (0,0,0,0,0) in
    let obs= combine_tableaux [|mur_etablissement_scolaire m murs_fluidification;
        genere_poteaux_car m poteau_fluidification2|] in
    let s, c_s= genere_porte_sortie_simu_obs sorties_fluidification m in
    let z= zones_fluidification in
    let population=genere_pop_aleatoire_dans_zone (k::[]) m z  50 in
    (m, obs,z, s, c_s, population);;
    
(*fin des simulations pour observer les fluidification*)

(*ajout distance a sortie: on cré une matrice avec dans chaque case la distance à la sortie 
la plus proche*)
let distance_sortie sorties zones mapping obstacles=
    let matrice_distance= Array.make_matrix dimx dimy 0. in
    let vu= Array.make_matrix dimx dimy 0 in
    let file_exploration= Queue.create () in
    let file_uniformisation=Queue.create() in
    for j=0 to Array.length sorties.(0) -1 do
        let xs, ys, _= sorties.(0).(j) in
        let (_, x_min, x_max, y_min, y_max)=mapping.(xs).(ys) in
        for x= x_min to x_max do
            for y=y_min to y_max do 
                Queue.add (x,y) file_exploration;
                Queue.add (x,y) file_uniformisation;
                vu.(x).(y)<-4; (* deja exploré et c'est une sortie*)
                matrice_distance.(x).(y)<-0.;
            done;
        done;
    done;
    for i=0 to Array.length obstacles -1 do
        let xo,yo= obstacles.(i) in
        let (_, x_min, x_max, y_min, y_max)=mapping.(xo).(yo) in
        for x=x_min to x_max do
            for y=y_min to y_max do 
                vu.(x).(y)<- 3; (*marqueur obstacle*)
                matrice_distance.(x).(y)<- 5.*. (float_of_int dimx);
            done
        done
    done;
    while not (Queue.is_empty file_exploration) do
        let x0,y0= Queue.take file_exploration in
        let a_explorer= [|(x0-1,y0-1);(x0, y0-1); (x0+1, y0-1);(x0-1,y0); (x0+1, y0);
            (x0-1,y0+1);(x0, y0+1); (x0+1, y0+1)|] in
        let min=ref (5.*. (float_of_int dimx)) in
        for i=0 to 7 do
            let x,y=a_explorer.(i) in
            if (x<dimx) && (x>=0) && (y>=0) && (y<dimy) then 
                if (vu.(x).(y)=0) then begin
                    Queue.add (x,y) file_exploration;
                    Queue.add (x,y) file_uniformisation;
                    vu.(x).(y)<-1;(* 1 indique que l'élément est dans la file d'exploration
                    mais n'a pas été exploré*)
                end
                else if (vu.(x).(y)>1)&&(matrice_distance.(x).(y) +.1.< !min) then
                    min:=matrice_distance.(x).(y) +.1.
        done;
        vu.(x0).(y0)<- 2;
        matrice_distance.(x0).(y0)<- !min;
    done;
    while not (Queue.is_empty file_uniformisation) do
        let x0,y0= Queue.take file_uniformisation in
        let a_explorer= [|(x0-1,y0-1);(x0, y0-1); (x0+1, y0-1);(x0-1,y0); (x0+1, y0);
            (x0-1,y0+1);(x0, y0+1); (x0+1, y0+1)|] in
        let min=ref matrice_distance.(x0).(y0) in
        for i=0 to 7 do
            let x,y=a_explorer.(i) in
            if (x<dimx) && (x>=0) && (y>=0) && (y<dimy) then 
            if(matrice_distance.(x).(y) +.1.< !min) then min:=matrice_distance.(x).(y) +.1.
        done;
        matrice_distance.(x0).(y0)<- !min;
    done;matrice_distance;;
(*fin calcul des distances minimales à la sortie*)

let matrice_flux= Array.make_matrix dimx dimy 0. ;;
let mapping, obstacles, zones, sorties, cara_sortie, population= simulation_etablissement_1 0;;
let m_d_sortie= distance_sortie sorties zones mapping obstacles;;
let n_pop= Array.length population ;;
let table_hachage_pop= table_hachage_humain population n_pop;;
let t= ref 0. ;;
try
    assigne_sortie population mapping n_pop cara_sortie;
    open_graph "651x651+20";
    auto_synchronize false;
    while true do
        auto_synchronize false;
        clear_graph();
        affiche_zones zones;
        affiche_pop mapping population n_pop;
        affiche_sortie mapping sorties cara_sortie (-1);
        affiche_obstacles mapping obstacles (Array.length obstacles);
        t:= !t +. 1./.3.;
        let new_flux =avance_humains mapping population n_pop sorties cara_sortie zones z_f 
            m_d_sortie table_hachage_pop in
        ajoute_matrices matrice_flux new_flux;
        synchronize ();
        sleepf 0.2;
    done;
with
    |_ ->();;
!t;;
n_pop;;
open_graph "651x651+60";;
let new_m= rend_uniforme matrice_flux;;
dessiner_image new_m;;
affiche_sortie mapping sorties cara_sortie (-1);;
affiche_obstacles mapping obstacles (Array.length obstacles);;
