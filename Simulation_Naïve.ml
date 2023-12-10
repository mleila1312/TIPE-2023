#load "graphics.cma";;
open Graphics;;
(* création des types*)
type humain = {mutable position_h: float array; vitesse_desire:float; stress: float; 
    mutable congestion: float; mutable vitesse: float; mutable direction: float array; 
    rayon: float;mutable sorti:bool; mutable i_sortie: int; mutable danger: bool};;
type sortie={position: float array; largeur: float};;
type obstacle={coin1: float array; coin2: float array; coin3: float array; coin4: float array};;

(* On ne s'intéresse pas à la congestion générale, mais à la congestion dans une certain
rayon autour de la personne à laquelle on s'intéressera*)
(*création des constantes*)
let rayon_congestion= 5.;;
let pi= 4.*.atan(1.);;
let dimx=650;;

let dimy=650;;
(*fonctions sur les vecteurs*)
let norme vect= sqrt((vect.(0))**2.+.(vect.(1))**2.);;

let vecteur ref point= [|point.(0)-.ref.(0); point.(1)-.ref.(1)|];;

let renvoie_angle2 vect= atan2 vect.(1) vect.(0);;

(*renvoie le point d'intersection entre deux droites*)
let intersection droite1 droite2=
    let y1= droite1.(1).(1) -. droite1.(0).(1) and
    x1=droite1.(1).(0) -. droite1.(0).(0) and
    y2=droite2.(1).(1) -. droite2.(0).(1) and
    x2=droite2.(1).(0) -. droite2.(0).(0) in
    let a1=y1/.x1 and a2= y2/.x2 in
    let b1=droite1.(1).(1)-.a1*.droite1.(1).(0) and b2=droite2.(1).(1)-.a2*.droite2.(1).(0) in
    let x=(b2-.b1)/.(a1-.a2) in
    [|x; a1*.x+.b1|];;
	
(*effectue la rotation du vecteur par l'angle nommé angle*)
let rotation angle vecteur= [|vecteur.(0)*.(cos angle) -. vecteur.(1)*.(sin angle); 
    vecteur.(0)*.(sin angle) +. vecteur.(1)*.(cos angle)|];;
	
(*vérification de collisions entre deux personnes*)
let collision humain1 humain2= (humain1.rayon+.humain2.rayon )> norme (vecteur humain1.position_h humain2.position_h);;

let tri_carre point1 point2=
    if point1.(1)<point2.(1) then -1
    else if point1.(1)>point2.(1) then 1
    else
        if point1.(0)<point2.(0) then -1
        else 1;;

(*calcul d'angle non orienté entre deux vecteurs*)
let angle_entre_deux_vecteurs vect1 vect2= acos ((vect1.(0)*.vect2.(0) +. 
    vect1.(1)*.vect2.(1))/.((norme vect1)*. (norme vect2)));;

(* vérifie si un point est dans un rectangle en calculant la somme des angles formés
entre le point et les différents sommet du rectangle*)
let somme_angles origine coins=
    Array.sort tri_carre coins;
    let tmp=coins.(3) in
    coins.(3)<-coins.(2);
    coins.(2)<- tmp;
    let vect1= vecteur origine coins.(0) and
    vect2= vecteur origine coins.(1) and
    vect3= vecteur origine coins.(2) and
    vect4= vecteur origine coins.(3) in
    let somme_angle = (angle_entre_deux_vecteurs vect1 vect2) +. 
        (angle_entre_deux_vecteurs vect2 vect3)+. (angle_entre_deux_vecteurs vect3 vect4)+.
        (angle_entre_deux_vecteurs vect4 vect1) in
    (* on considère une certaine marge d'erreur dûe à l'utilisation de nombres flottants*)
    (somme_angle> 2.*.pi-. pi/.20. )&&(somme_angle< 2.*.pi+. pi/.20. );;

let verifie_collision tab_obstacles nb_obstacles nouveau_obstacle=
    let res=ref false in
    (*on vérifie si il y a collision avec les autres obstacles*)
    for i=0 to nb_obstacles-1 do
        let obstacle_considere=tab_obstacles.(i) in 
        let tab_coins=[|obstacle_considere.coin1; obstacle_considere.coin2;
            obstacle_considere.coin3;obstacle_considere.coin4|] in
        (* on vérifie pour chaque point du nouvel obstacle si l'un de ses coins est à 
        l'intérieur d'un autre obstacle*)
        for j=0 to 3 do
            res:=(!res)||(somme_angles nouveau_obstacle.(j) tab_coins)
        done
    done;
    !res;;
	
let verifie_collision_population population nb_humain nouvel_humain=
    let res= ref false in
    for i=0 to nb_humain -1 do
        if not population.(i).sorti then
            let tmp=(collision population.(i) nouvel_humain) in
                res:= !res|| tmp;
    done;
    !res;;
    
let cre_groupe_aleatoire nbpersonne tab_obstacles nb_obstacles rayon_moyen=
(*on crée le tableau contenant les humains*)
	let humain_de_base={position_h=[|0.;0.|]; vitesse_desire=0.;stress=0.; congestion=0.; 
        vitesse=0.; direction=[|0.;0.|]; rayon=0.; sorti=false; i_sortie=0; danger=false} in
	let population= Array.make nbpersonne humain_de_base in
	for i=0 to nbpersonne-1 do
	(* on cré une personne aléatoire*)
	   let personne_temporaire= ref {position_h=[|Random.float ((float_of_int dimx)-.2.*.8.);
            Random.float ((float_of_int dimy)-.2.*.8.)|]; vitesse_desire=3.*.65.1+. 
            Random.float (2.*.65.1);stress=Random.float 5.; congestion=0.; vitesse=0.; 
            direction=[|0.;0.|]; rayon=rayon_moyen +. Random.float 3.; sorti=false; i_sortie=0;
            danger=false} in
	   let coin_bas= ref [| !personne_temporaire.position_h.(0)-. !personne_temporaire.rayon; 
            !personne_temporaire.position_h.(1)-. !personne_temporaire.rayon|] in
	   let rayon= ref !personne_temporaire.rayon in
	   if i>0 then
		(*tant qu'il y a collision, on crée une nouvelle personne aléatoire*)
		while (verifie_collision tab_obstacles nb_obstacles [|!coin_bas; 
                [|!coin_bas.(0)+. !rayon*.2.; !coin_bas.(1)|]; [|!coin_bas.(0); 
                !coin_bas.(1)+. !rayon*.2.|]; [|!coin_bas.(0)+. !rayon*.2.;
                !coin_bas.(1)+. !rayon*.2.|]|]) ||
                (verifie_collision_population population i !personne_temporaire) do
		  personne_temporaire:={position_h=[|Random.float ((float_of_int dimx)-.2.*.8.);
                    Random.float ((float_of_int dimy)-.2.*.8.)|]; vitesse_desire=3.*.65.1+.
                    Random.float (2.*.65.1);stress=Random.float 5.; congestion=0.; vitesse=0.;
                    direction=[|0.;0.|]; rayon=rayon_moyen +. Random.float 3.; sorti=false; 
                    i_sortie=0;danger=false};
                    coin_bas:= [| !personne_temporaire.position_h.(0)-. !personne_temporaire.rayon; 
                    !personne_temporaire.position_h.(1)-. !personne_temporaire.rayon |];
                    rayon:= !personne_temporaire.rayon;
                done
            else
                begin
            (* si c'est la première personne qu'on ajoute, on a simplement besoin de vérifier la collision avec les obstacles*)
                while (verifie_collision tab_obstacles nb_obstacles [|!coin_bas; 
                [|!coin_bas.(0)+. !rayon*.2.; !coin_bas.(1)|]; [|!coin_bas.(0); !coin_bas.(1)+. 
                !rayon*.2.|]; [|!coin_bas.(0)+. !rayon*.2.; !coin_bas.(1)+. !rayon*.2.|]|]) do
							personne_temporaire:={position_h=[|Random.float ((float_of_int dimx)-.2.*.8.); 
                    Random.float ((float_of_int dimy)-.2.*.8.)|]; vitesse_desire=3.*.65.1+.
                    Random.float (2.*.65.1);stress=Random.float 5.; congestion=0.; vitesse=0.;
                    direction=[|0.;0.|]; rayon=rayon_moyen +. Random.float 3.; sorti=false; 
                    i_sortie=0;danger=false};
                   coin_bas:= [| !personne_temporaire.position_h.(0)-. !personne_temporaire.rayon;
                   !personne_temporaire.position_h.(1)-. !personne_temporaire.rayon |];
                  rayon:= !personne_temporaire.rayon;
                done;
                (* une fois que lescaractéristiques de la personne sont correctes, on l'ajoute à la 
                population*)
                population.(i)<- !personne_temporaire;
                end
    done;
    population;;
  
(*Pour calculer la ocngestion autour d'une personne, on calcule le nombre 
de personnes présentes dans un certain
rayon et on divise ce nombre par l'aire explorée*)
let calcule_congestion_personnes tableau_personnes nb_personnes ind_humain=
	let aire_occupee= ref 0. and personne= tableau_personnes.(ind_humain) in
	for i=0 to nb_personnes-1 do
		if not tableau_personnes.(i).sorti && sqrt((personne.position_h.(0)-.tableau_personnes.(i).position_h.(0))**2. +.(personne.position_h.(1)-.tableau_personnes.(i).position_h.(1))**2.)<rayon_congestion*. personne.rayon then
			aire_occupee:= !aire_occupee +. pi*.(tableau_personnes.(i).rayon**2.)
	done;
	personne.congestion<- !aire_occupee/.(pi*.(rayon_congestion**2.));;

let genere_obstacles_aleatoire_carre nb_obstacles=
    (* on calcule un certains coté maximal pour éviter que nous ne puissions pas placer tous les obstacles voulus*)
    let cote_max=(float_of_int dimx)/. (float_of_int nb_obstacles) in
    let tab_obstacles= Array.make nb_obstacles {coin1=[|0.;0.|];coin2=[|0.;0.|];coin3=[|0.;0.|];
        coin4=[|0.;0.|]} in
    for i=0 to nb_obstacles-1 do
        let cote_temporaire=  ref (cote_max/.2.+. (Random.float (cote_max/.2.))) in
        let point_origine=[|10.+. Random.float ((float_of_int dimx)-.20.-. !cote_temporaire);
            10.+. Random.float ((float_of_int dimy)-.20.-. !cote_temporaire)|] in
	let obstacle_temporaire= ref [| point_origine; [|point_origine.(0)+. !cote_temporaire; 
            point_origine.(1)|]; [|point_origine.(0); point_origine.(1)+. !cote_temporaire|];
            [|point_origine.(0)+. !cote_temporaire; point_origine.(1)+. !cote_temporaire|] |] in
	if i!=0 then begin
            while (verifie_collision tab_obstacles (i) !obstacle_temporaire) do
		  (* tant qu'il y a collision entre deux obstacles on modifie les caractéristiques de 
                    l'obstacle*)
                    cote_temporaire:=  (cote_max/.2.+. Random.float (cote_max/.2.));
                    point_origine.(0)<-10.+. Random.float ((float_of_int dimx)-.20.-. 
                                        !cote_temporaire);
                    point_origine.(1)<-10.+. Random.float ((float_of_int dimy)-.20.-. 
                                        !cote_temporaire);
                    obstacle_temporaire:= [| point_origine; [|point_origine.(0)+. !cote_temporaire;
                        point_origine.(1)|]; [|point_origine.(0); point_origine.(1)+. 
                        !cote_temporaire|];[|point_origine.(0)+. !cote_temporaire; point_origine.(1)+. 
                        !cote_temporaire|] |];
            done;
            (*une fois que l'obstacle est correct, on l'ajoute au tableau d'obstacle*)
            tab_obstacles.(i)<-{coin1= !obstacle_temporaire.(0); coin2= !obstacle_temporaire.(1);
                coin3= !obstacle_temporaire.(2);coin4= !obstacle_temporaire.(3)};
        end
    done;
    tab_obstacles;;

let genere_obstacle_cercle centre rayon=
    (* avec cette modélisation et les fonctions implémentées pour gérer les 
    obstacles, pour gérer un obstacle rond, il faut le créer en utilisant 
    des obstacles carrés occupant 1 pixel*)
    let nb= ref 0  and l=ref [] in
    for i= max (centre.(0)- rayon) 0 to min (dimx-1) (centre.(0)+ rayon) do
        for j= max 0 (centre.(1)-rayon) to min (dimy-1) (centre.(1)+rayon) do
            if (j-centre.(1))*(j-centre.(1))+(i-centre.(0))*(i-centre.(0)) <=rayon*rayon then
                begin
                    nb:= !nb +1;
                    let x=float_of_int i and y=float_of_int j in
                    let ob={coin1=[|x;y|]; coin2=[|x+.1.;y|]; coin3=[|x;y+.1.|];
                    coin4=[|x+.1.;y+.1.|]} in
                    l:= ob::(!l);
                end
        done
    done;
    !l;;

let sortie_la_plus_proche tab_sortie humain nbsortie=
    let min= ref 0 in
    for i=1 to nbsortie-1 do
        let sortie_regardee= tab_sortie.(i) in
        (*on compare pour chaque porte si elleest plus proche que le minimum trouvé précédemment*)
        if norme (vecteur humain.position_h sortie_regardee.position)<norme (vecteur humain.position_h tab_sortie.(!min).position) then
            min:=i
    done;
    !min;;

(* On récupère les coins extrêmes de la porte*)
let find_limite_porte porte=
    let pos= porte.position in
    let largeur_max=porte.largeur-.4. in
    if pos.(0)=0. || pos.(0)=(float_of_int dimx) then [|pos.(0); pos.(1)-. largeur_max|], [|pos.(0);    pos.(1)+. largeur_max|]
    else [|pos.(0)-.largeur_max; pos.(1)|], [|pos.(0)+.largeur_max; pos.(1)|];;
	
let renvoie_coins obstacle=
    (*renvoie un tableau contenant les coordonnées des coins d'un obstacle*)
    let p1=[| obstacle.coin1.(0);obstacle.coin1.(1)|] and
        p2=[| obstacle.coin2.(0); obstacle.coin2.(1)|] and
        p3=[| obstacle.coin3.(0);obstacle.coin3.(1)|] and
        p4=[| obstacle.coin4.(0); obstacle.coin4.(1)|] in
    [|p1;p2;p3;p4|];;

let tri_carre_croissant point1 point2=
    (* ce tri sert à organiser les coins d'un obstacle afin que 
    l'affichage se fasse correctement*)
    if point1.(0)<point2.(0) then
        if point1.(1)<point2.(1) then -1
        else if point1.(1)>point2.(1)then 1
        else -1
    else if point2.(0)<point1.(0) then
        if point1.(1)<point2.(1) then -1
        else if point1.(1)>point2.(1) then 1
        else 1
    else
        if point1.(1)<point2.(1) then -1
        else 1;;

(*ajustement aux obstacles*)
let max tab n =
    (*on récupère le maximum d'un tableau*)
    let max= ref tab.(0) in
    for i=0 to n-1 do
        if !max< tab.(i) then max:=tab.(i)
    done;
    !max;;

let collision_o obstacle humain dir angle=
    (*vérifie la collision avecun obstacle*)
    let coins= renvoie_coins obstacle in
    (* on tourne les vecteurs de façon à ce que la direction de l'humain vers la sortie la plus 
    proche ait un angle de 0*)
    let v1=rotation (-.angle) (vecteur humain.position_h coins.(0)) and
        v2=rotation (-.angle) (vecteur humain.position_h coins.(1)) and
        v3=rotation (-.angle) (vecteur humain.position_h coins.(2)) and
        v4=rotation (-.angle) (vecteur humain.position_h coins.(3)) in
    (* nous récupérons les angles que forme les différents vecteurs e l'humain aux différent 
    coins du carré avec l'axe x*)
    let angle1= renvoie_angle2 v1 and
        angle2= renvoie_angle2 v2 and
        angle3= renvoie_angle2 v3 and
        angle4= renvoie_angle2 v4 in
    (*on récupère les angles minimums et maximum*)
    let angle_max= max [|angle1;angle2;angle3;angle4|] 4 and
        angle_min= -. (max [|-.angle1;-.angle2;-.angle3;-.angle4|] 4) in
    (*si il y a collision, il y aura un angle négatif et un angle positif, et l'angle séparant 
    ces deux vecteurs devra être inférieur à pi*)
    if angle_max -. angle_min <=pi && angle_min<=0. && angle_max>=0. then true
    else false;;

let rec obstacle_collision tab_obstacle n humain dir i angle=
    (*on récupère tous le sobstacles avec lesquels il y aurait collision si l'humain prenait 
    la direction dir sans ajustement*)
    if i=n then []
    else
        if collision_o tab_obstacle.(i) humain dir angle then (tab_obstacle.(i))::
            (obstacle_collision tab_obstacle n humain dir (i+1) angle)
        else obstacle_collision tab_obstacle n humain dir (i+1) angle;;
        
let coins_a_considerer obstacle angle  humain=
    (* on récupère les coins ayant des angles par rapport au vecteur direction maximum en norme,
    ce seront les coins possible pour la nouvelle direction car nous avns que des obstacles 
    rectangulaires*)
    let tri a b= if a.(2)<b.(2) then -1 else 1 in
    let coins= renvoie_coins obstacle in
    (* on récupère les vecteurs reliant l'humain à chaque coin*)
    let v1=(vecteur humain.position_h coins.(0)) and v2=(vecteur humain.position_h coins.(1)) and
        v3=(vecteur humain.position_h coins.(2)) and v4=(vecteur humain.position_h coins.(3)) in
    (*on récupère les angles pour chaque vecteur*)
    let angle1= renvoie_angle2 (rotation (-.angle) v1) and 
        angle2= renvoie_angle2 (rotation (-.angle)  v2)  
        and angle3= renvoie_angle2 (rotation (-.angle) v3) 
        and angle4= renvoie_angle2 (rotation (-.angle) v4) in
    let tab= [|[|coins.(0).(0);coins.(0).(1); angle1|];[|coins.(1).(0);coins.(1).(1); angle2|];
        [|coins.(2).(0);coins.(2).(1); angle3|];[|coins.(3).(0);coins.(3).(1); angle4|]|] in
    (*on tri le tableau et on récupère les angles voulus*)
    Array.sort (tri) tab;
    [|[|tab.(0).(0); tab.(0).(1)|]; [|tab.(3).(0); tab.(3).(1)|]|];;
    
let angle_min coins humain dir=
    (* on compare les angles formés entre l'humain et les deux coins, on récupère le minimum, 
    celui qui sera le plus proche de la direction optimale*)
    let angle1=  ref (angle_entre_deux_vecteurs (vecteur humain.position_h coins.(0)) dir) and
        angle2= ref (angle_entre_deux_vecteurs (vecteur humain.position_h coins.(1)) dir) in
    if !angle1 >pi then angle1:= 2.*.pi -. !angle1;
    if !angle2 >pi then angle2:= 2.*.pi -. !angle2;
    if !angle1< !angle2 then coins.(0) 
    else coins.(1);;

let  rec coins_plus_proche min liste humain angle dir= match liste with
    (*la liste est composé des obstacles avec lesquels il y a collision, nous cherchons 
    alors l'obstacle le plus proche de l'humain avec lequel il y aura collision, c'est 
    par rapport à celui-ci que nous devrons adapter la trajectoire*)
    |[]-> min
    |obstacle::q when min.(0).(0)= -.1.-> 
        let coins= coins_a_considerer obstacle angle humain in
        (* si on a pas encre de minimum, on prend le 1er obstacle*)
        coins_plus_proche coins q humain angle dir
    |obstacle::q -> 
        let coins= coins_a_considerer obstacle angle humain in
        (* on récupère le point le plus proche de notre direction*)
        let angle_min_coins= angle_min min humain dir in
        (*on récupère le point d'intersection entre la droite passant par les deux 
        nouveaux coins de l'obstacle à considérer et entre la droite et le point 
        d'angle minimum avec la direction d'origine de l'actuel obstacle le plus 
        proche*)
        let point_coupe= intersection coins [|humain.position_h;  angle_min_coins|] in
        (* on compare la norme entre ce point d'intersection et l'humain et la norme 
        entre l'humain et le point d'angle inimum du minimum actuel en fonction de 
        la norme, nous pouvons déterminer quel obstacle est le plus proche*)
        if norme (vecteur humain.position_h angle_min_coins)< 
            norme (vecteur humain.position_h point_coupe) then 
                coins_plus_proche min q humain angle dir
        else coins_plus_proche coins q humain angle dir;;
        
let ajuste_obstacle tab_obstacle nb_obstacle humain direction=
    (* on vérifie que l'humain n'est pas déjà sur une sortie*)
    if direction.(0)<>0. && direction.(1)<>0. then
        let angle_humain=renvoie_angle2 direction in
        (*on récupère les obstacles avec lesquels il y a collision*)
        let liste_obs_avec_collision= obstacle_collision tab_obstacle nb_obstacle humain 
        direction 0 angle_humain in
        (*si il n'y a pas de collision, on va directement vers la sortie*)
        if liste_obs_avec_collision=[] then direction
        else
            (* sinon, notre nouvelle direction est celle allant de l'humain au coin d'angle 
            minimum avec la direction de l'obstacle le pus proche*)
            let coins_les_plus_proches= coins_plus_proche [|[|-.1.; 0.|];[|0.;0.|]|] 
                liste_obs_avec_collision humain angle_humain direction in
            vecteur humain.position_h (angle_min coins_les_plus_proches humain  direction)
    else [|0.;0.|];;

(* coefficients d'influence de la population*)
let coeff_influence_personne=0.005;;

(* nous récupérons les personnes situées dans un certain rayon de l'humain considéré*)
let rec influenceur population nb_personnes i centre rayon=
    if i= nb_personnes then []
    else
        let personne= population.(i) in
        if not personne.sorti && norme (vecteur centre personne.position_h)< 20.*.rayon then 
            personne::(influenceur population nb_personnes (i+1) centre rayon)
        else (influenceur population nb_personnes (i+1) centre rayon);;

let rec influence personne_influenceur centre rayon= match personne_influenceur with
    |[]->[|0.;0.|]
    |t::q -> if t.sorti=false then
                let centre_pers= t.position_h in
                (*on considère le vecteur reliant la personne considérée et celle qui 
                a une influence*)
                let vect= vecteur centre centre_pers in
                (* on récupère l'influence des autres personnes dans la zone*)
                let somme= influence q centre rayon in
                (* en fonction de la distance entre ces deux personne, cet personne attirera
                ou repoussera l'humain considéré*)
                if (norme vect)<7.*.rayon then 
                    if (norme vect)<>0. then [|somme.(0)-. 7.*.rayon*.vect.(0)/.(norme vect); 
                        somme.(1)-. 7.*.rayon*.vect.(1)/.(norme vect)|]
                    else [|somme.(0); somme.(1)|]
                else [|somme.(0)+.vect.(0); somme.(1)+.vect.(1)|]
                else influence q centre rayon;;

let ajuste_population population nb_personne personne direction stress congestion=
    (*on récupère les personnes influant l'humain*)
    let pers_influenceur= influenceur population nb_personne 0 personne.position_h personne.rayon in
    (*on récupère la force influençant l'humain*)
    let facteur_influent= influence pers_influenceur personne.position_h personne.rayon in
    (*on récupère les normes de directions de d'influence*)
    let norme_dir= norme direction and norme_f_influent= norme facteur_influent in
    (*on renvoie la nouvelle direction*)
    if norme_dir=0. && norme_f_influent=0. then [|0.;0.|]
    else if norme_dir=0. then [|
        stress*.congestion*.coeff_influence_personne*.facteur_influent.(0)/.norme_f_influent; 
        stress*.congestion*.coeff_influence_personne*.facteur_influent.(1)/.norme_f_influent|]
    else if norme_f_influent=0. then [|direction.(0)/.norme_dir ;direction.(1)/.norme_dir|]
    else 
        [|(direction.(0)/.norme_dir +.
        stress*.congestion*.coeff_influence_personne*.facteur_influent.(0)/.norme_f_influent);
        (direction.(1)/.norme_dir +. 
        stress*.congestion*.coeff_influence_personne*.facteur_influent.(1)/.norme_f_influent)|];;

let decale humain direction angle_decalage_sortie=
    let angle = renvoie_angle2 [| norme (direction); 4.*.humain.rayon|] in
    if angle_decalage_sortie<0. then rotation (-.angle) direction
    else rotation (angle) direction;;
    
let point_plus_proche_porte porte humain=
    (* la porte étant d'une certaine largeur, on calcule le point de la porte 
    tel que le trajet de la position de l'humain à ce point soit minimal*)
    let pos_p=porte.position and pos_h= humain.position_h in
    if pos_p.(0)=0.||pos_p.(0)=(float_of_int dimx) then 
        if pos_h.(1)<=pos_p.(1)+. porte.largeur/.2. && pos_h.(1)>=pos_p.(1)-. 
            porte.largeur/.2. then [|pos_p.(0); pos_h.(1)|]
        else if pos_h.(1)>pos_p.(1)+. porte.largeur/.2. then 
            [|pos_p.(0); pos_p.(1)+. porte.largeur/.2.|]
        else [|pos_p.(0); pos_p.(1)-. porte.largeur/.2.|]
    else
        if pos_h.(0)<=pos_p.(0)+. porte.largeur/.2. && 
            pos_h.(0)>=pos_p.(0)-. porte.largeur/.2. then [|pos_h.(0); pos_p.(1)|]
        else if pos_h.(0)>pos_p.(0)+. porte.largeur/.2. then 
            [|pos_p.(0)+. porte.largeur/.2.; pos_p.(1)|]
        else [|pos_p.(0)-. porte.largeur/.2.; pos_p.(1)|];;

let ajuste_direction tab_sortie nb_sortie tab_obstacle nb_obstacles population nb_personne =
    for i=0 to nb_personne -1 do
    let humain= population.(i) in
    if humain.sorti=false && humain.danger then begin
        calcule_congestion_personnes population nb_personne i;
        (*on récupère la sortie que vise notre être humain*)
        let s_plus_proche= tab_sortie.( humain.i_sortie) in
        (* on calcule le vecteur allant de la position de l'humain au point le plus proche 
        de cette porte*)
        let dir= vecteur humain.position_h (point_plus_proche_porte s_plus_proche humain) in
        (*On ajuste par rapport à la population*)
        let direction=ajuste_population population nb_personne humain dir humain.stress
            humain.congestion in
        (*On ajuste en fonction des obstacles*)
        let new_dir= (ajuste_obstacle tab_obstacle nb_obstacles humain direction) in
        let angle_dir= renvoie_angle2 direction in
        let angle_new_dir= renvoie_angle2 (rotation (-.angle_dir) new_dir) in
        (*On vérifie i la direction est changée en fonction des obstacles*)
        if new_dir.(0)<> direction.(0)||new_dir.(1)<> direction.(1) then 
            humain.direction<- decale humain new_dir angle_new_dir
        else humain.direction<- direction;
        end
    done;;

let assigne_sortie population nb_personne sorties nb_sortie=
    (*On calcule la sortie la plus proche pour chaque humain*)
    for i=0 to nb_personne-1 do
        let h=population.(i) in
        h.i_sortie<-( sortie_la_plus_proche sorties h nb_sortie)
    done;;
	
let renvoie_zone_porte porte=
    (*on renvoie la zone de la porte où si un humain s'y trouve, il est considéré comme évacué*)
    let pos=porte.position in
    if pos.(0)=0. then [| [|0.; pos.(1)-. porte.largeur|]; [|5.; pos.(1)-. porte.largeur|]; 
        [|5.; pos.(1)+. porte.largeur|];[|0.; pos.(1)+. porte.largeur|]|]
    else if pos.(0)=(float_of_int dimx) then[| [|(float_of_int dimx)-.5.; 
        pos.(1)-. porte.largeur|];[|(float_of_int dimx); pos.(1)-. porte.largeur|];
        [|(float_of_int dimx); pos.(1)+. porte.largeur|]; [|(float_of_int dimx)-.5.; 
        pos.(1)+. porte.largeur|]|]
    else if pos.(1)=0. then [| [|pos.(0)-. porte.largeur;0.|];[|pos.(0)+. porte.largeur;0.|]; 
        [| pos.(0)+. porte.largeur;5.|]; [| pos.(0)-. porte.largeur;5.|]|]
    else [| [| pos.(0)-. porte.largeur;(float_of_int dimy)-.5.|]; [| pos.(0)+. porte.largeur;
        (float_of_int dimy)-.5.|];[|pos.(0)+. porte.largeur;(float_of_int dimy)|];
        [|pos.(0)-. porte.largeur;(float_of_int dimy)|]|];;

let verifie_collision_population_deuxieme_partie population deb fin nouvel_humain=
    let res= ref false in
    for i=deb+1 to fin -1 do
        if not population.(i).sorti then
            let tmp=(collision population.(i) nouvel_humain) in
            res:= !res|| tmp;
    done;
    !res;;
	
let avance_ou_non new_pos rayon_h tab_pop nb_pop indice=
    (* on vérifie qu'il n'y a pas de collisions ni avec les obstacles, ni avec 
    le reste de la population*)
    let h_temp={position_h= new_pos; vitesse_desire=0.;stress=0.; congestion=0.; 
        vitesse=0.; direction=[|0.;0.|]; rayon=rayon_h; sorti=false; i_sortie=0; danger=true} in
    (verifie_collision_population tab_pop indice h_temp)||
        (verifie_collision_population_deuxieme_partie tab_pop indice nb_pop h_temp);;

let rayon_danger= 100.;;

(* vérifie si un humain est bien dans un certain rayon par rapport à une position référence*)
let dans_rayon humain location rayon= 
    let x= humain.position_h.(0) and y= humain.position_h.(1) in
    sqrt((x-. location.(0))**2. +. (y-.location.(1))**2.)<=rayon;;
    
let cre_danger tab_humain nb_humain=
    (* on cré au hasard un point que sera source de l'information de l'existence du danger*)
    let location_danger=[|Random.float ((float_of_int dimx)-.2.*.8.); 
        Random.float ((float_of_int dimy)-.2.*.8.)|] in
    for i=0 to nb_humain-1 do
        let humain= tab_humain.(i) in
        if humain.danger=false && dans_rayon humain location_danger rayon_danger then humain.danger<-true
    done;;
	
(* on définit le rayon de propagation du danger*)
let rayon_propagation= ref 50. ;;

let propage_danger tab_humain nb_humain indice=
    (*chaque personne qui sait qu'il y a un danger propage l'information aux autres personnes
    proche de lui par son attitude*)
    let pos_reference= tab_humain.(indice).position_h in
    for i= 0 to nb_humain -1 do
        if i<>indice then
            let h=tab_humain.(i) in
            if ((h.danger=false) && (dans_rayon h pos_reference !rayon_propagation)) then 
                h.danger<-true
    done;;

let calcule_congestion carre population nb_humains=
    let congestion=ref 0. in
    for i=0 to nb_humains -1 do
        let humain= population.(i) in
        if somme_angles humain.position_h carre then congestion:= !congestion +. humain.rayon
    done; !congestion;;
   
let choisi_direction_humain humain population nb_humains tab_sortie=
    (* on récupère la sortie assignée, le rayon, et la position de l'humain*)
    let sortie= tab_sortie.(humain.i_sortie) in
    let rayon=humain.rayon in
    (* en foncion de la congestion à droite et à gauche de la porte, on renvoie une direction 
    de mouvement qui dirige l'humain vers l'endroit le moins congestionné, qui en fonction de 
    la porte visé sera un mouvement vers le bas/haut/gauche/droite, ces mouvements serviront 
    à débloquer les humains ne pouvant bouger sans ces mouvements*)
    let pos=humain.position_h in
    if sortie.position.(1)=0.||pos.(1)=(float_of_int dimy) then
        (* on calcule les congestion dans les deux carrés*)
        let carre_gauche=[|[|pos.(0)-. 5.*.rayon; pos.(1)-.5.*.rayon|];[|pos.(0); 
            pos.(1)-.5.*.rayon|]; [|pos.(0); pos.(1)+.5.*.rayon|];[|pos.(0)-.5.*.rayon; 
            pos.(1)+.5.*.rayon|]|] in
        let carre_droit= [|[|pos.(0); pos.(1)-.5.*.rayon|];[|pos.(0)+.5.*.rayon; 
            pos.(1)-.5.*.rayon|]; [|pos.(0)+.5.*.rayon; pos.(1)+.5.*.rayon|];
            [|pos.(0); pos.(1)+.5.*.rayon|]|] in
        let congestion_droite= calcule_congestion carre_droit population nb_humains and
            congestion_gauche= calcule_congestion carre_gauche population nb_humains in
        if congestion_gauche>congestion_droite then [|1.;0.|]
        else [|-1.;0.|]
    else (* on calcule les congestion dans les deux carrés*)
        let carre_haut=[|[|pos.(0)-. 5.*.rayon; pos.(1)|];[|pos.(0)+.5.*.rayon; pos.(1)|];
            [|pos.(0)+.5.*.rayon; pos.(1)+.5.*.rayon|];[|pos.(0)-.5.*.rayon; pos.(1)+.5.*.rayon|]|] in
        let carre_bas= [|[|pos.(0)-.5.*.rayon; pos.(1)-.5.*.rayon|];[|pos.(0)+.5.*.rayon; 
            pos.(1)-.5.*.rayon|]; [|pos.(0)+.5.*.rayon; pos.(1)|];[|pos.(0)-.5.*.rayon; pos.(1)|]|] in
        let congestion_haut= calcule_congestion carre_haut population nb_humains and
            congestion_bas= calcule_congestion carre_bas population nb_humains in
        if congestion_haut>congestion_bas then [|0.;-1.|]
        else [|0.;1.|];;
        
let avance dt coeff tab_obstacles nb_obstacles tab_sortie nb_sortie population nb_pop=
    (* on met à jour les directions pour chaque humain*)
    ajuste_direction tab_sortie nb_sortie tab_obstacles nb_obstacles population nb_pop;
    for i=0 to nb_pop-1 do
        let humain= population.(i) in
        let v= humain.vitesse_desire in
        (* on parcoure les humains qui ne sont pas sortis et qui sont alerté du danger*)
        if humain.sorti=false && humain.danger then begin
            (* on propage l'information de danger*)
            propage_danger population nb_pop i;
            let n= norme humain.direction in
            if n<>0. then begin
                (* on récupère la nouvelle position et on vérifie si l'humain peut avancer*)
                let new_pos=[|humain.position_h.(0) +. 
                    (humain.direction.(0)/.n)*.v*.dt;humain.position_h.(1) +. 
                    (humain.direction.(1)/.n)*.v*.dt|] in
                if not (avance_ou_non new_pos humain.rayon population nb_pop i) then 
                    humain.position_h<-new_pos
                else begin
                    (*si l'humain ne peut pas avancer, on lui fait effectuer des mouvements 
                    infinitésimaux lattéraux*)
                    humain.direction<- (choisi_direction_humain humain population nb_pop tab_sortie);
                    let position_latterale=[|humain.position_h.(0) +. 
                        (humain.direction.(0))*.v*.dt/.5.; humain.position_h.(1) +. 
                        (humain.direction.(1))*.v*.dt/.5.|] in
                    (* si cette position lattérale est impossible à occuper, l'humain ne bouge pas*)
                    if not (avance_ou_non position_latterale humain.rayon population nb_pop i) then 
                        humain.position_h<-position_latterale;
                    end; (* on récupère le but de notre humain*)
                let porte_lp_proche=tab_sortie.(humain.i_sortie) in
                let zone_porte= renvoie_zone_porte porte_lp_proche in
                (*on vérifie si notre humain est arrivé à la porte*)
                if somme_angles humain.position_h zone_porte then humain.sorti<-true;
                end;
            end
    done;
    (* on augmente le rayon de propagation e l'information danger, plus de personnes sont au courant
    plus l'information se propage vite*)
    rayon_propagation:= !rayon_propagation*.(1.+. coeff*.dt);;

(*calul des flux*)
let dessiner_image img =
    draw_image (make_image img) 0 0;;

(*on additionne 2 matrices*)
let ajoute_matrices destination ajout=
    for i=0 to dimx-1 do
        for j=0 to dimy-1 do
            destination.(i).(j)<-destination.(i).(j)+. ajout.(i).(j)
        done
    done;;

(*fonctions min et max*)
let min a b= if a>b then b else a;;
let max a b= if a<b then b else a;;

let calcul_congestion_mat population nb_pop=
    let matrice_congestion= Array.make_matrix dimx dimy 0. in
    for i=0 to nb_pop-1 do
        (* on va ajouter dans chaque indice (i,j) les aires des personnes étant sur cette case*)
        let position= population.(i).position_h in
        let rayon= population.(i).rayon in
        if not population.(i).sorti then
            (* pour chaque persnne, voici les cases potentielles à modifier*) 
            let x_min= max 0 (int_of_float (position.(0)-.2.*.rayon)) and
                x_max= min (dimx-1) (int_of_float (position.(0)+.2.*.rayon)) and
                y_min=max 0 (int_of_float (position.(1)-.2.*.rayon)) and 
                y_max=min (dimy-1) (int_of_float (position.(1)+.2.*.rayon)) in
            let distance_max= sqrt((float_of_int (y_max-y_min))**2.+.
                (float_of_int (x_max-x_min))**2.) in
            let aire= pi*.rayon**2. in
            if distance_max>0. then 
            for x=x_min to x_max do
                for y=y_min to y_max do
                    (*on vérifie que la case est bien occupée par la personne*)
                    if sqrt(((float_of_int y)-.position.(1))**2.+.((float_of_int x)-. 
                        position.(0))**2.)<2.*.rayon then 
                            matrice_congestion.(x).(y)<-matrice_congestion.(x).(y)+. aire
                done
            done
    done; matrice_congestion;;

let max_min_matrice matrice dimx dimy=
    let max=ref matrice.(0).(0) and min=ref matrice.(0).(0) in
    for i=0 to dimx do
        for j=0 to dimy do
            if matrice.(i).(j)> !max then max:=matrice.(i).(j)
            else if matrice.(i).(j)< !min && matrice.(i).(j)>0. then min:=matrice.(i).(j)
        done
    done; !max, !min;;

let couleur valeur=
    (* on transforme une valeur en couleur RBG de facon à obtenir une corrélation telle que 
    haute-> valeur-> faible <-> rouge-> violet-> bleu-> vert-> blanc*)
    if valeur-256> 0 then
        if valeur -2*256>=0 then (valeur -2*256,0,3*256-valeur-1)
        else (0,2*256-valeur -1, valeur -256)
    else (255-valeur,255,255-valeur);;

let converti_matrice_couleur matrice=
    (* on converti les congestions enregistrées en couleurs*)
    let matrice_couleur= Array.make_matrix dimx dimy 0 in
    let max, min= max_min_matrice matrice (dimx-1) (dimy-1) in
    for i=0 to dimx-1 do
        for j=0 to dimy-1 do
            let valeur= int_of_float ((3.*.256. -.1.)*.
                (sin((atan2 (8.*.(matrice.(i).(j)-.min)) (max-.min))))) in
            let r, g, b= couleur valeur in
            if valeur>0 then matrice_couleur.(i).(j)<- rgb r g b
            else matrice_couleur.(i).(j)<- rgb 255 255 255
        done
    done; matrice_couleur;;
    
let rend_uniforme matrice =
    (*on uniformise la matrice afin que les délimitations de congestions soient plus dégradées*)
    let new_mat= Array.make_matrix dimx dimy 0. in
    for i=0 to dimx-1 do
        for j=0 to dimy-1 do
            (*on considère pour uniformiser les valeur dans un carré 21x21 
            tel que la case soit au centre*)
            let x_min= max 0 (i-10) and x_max =min (dimx-1) (i+10) and
                y_min= max 0 (j-10) and y_max =min (dimy-1) (j+10) in
            let moy= ref 0.  and comptage= ref 0. in 
            for x=x_min to x_max do
                for y=y_min to y_max do
                    moy:= !moy +. matrice.(x).(y);
                    comptage:= !comptage +.1.;
                done
            done;
            new_mat.(i).(j)<- !moy /. !comptage;
        done
    done;new_mat;;

(* pour tourner une matrice de 90° vrs la droite*)
let rotation_droite matrice =
    let n_x=Array.length matrice and n_y=Array.length matrice.(0) in
    let new_one= Array.make_matrix n_y n_x  matrice.(0).(0) in
    for i=0 to n_x-1 do
        for j=0 to n_y-1 do
            new_one.(n_y-1-j).(i)<- matrice.(i).(j)
        done;
    done;
    new_one;;
    
let tri_affichage (a,b) (c,d)= 
    (* tri de points afin d'avoir un affichage correct*)
    if a<c then if b<=d then -1 else 1
    else if a<c then if b<d then -1 else 1
    else if b<d then -1 else 1;;

let affiche_obstacles liste_obstacles nb_obstacles=
    set_color (rgb 0 0 0 ); 
    for i=0 to nb_obstacles-1 do
        let obstacle= liste_obstacles.(i) in
        (*on récupère les coins de façon à pouvoir les afficher*)
        let p1=(int_of_float obstacle.coin1.(0),int_of_float obstacle.coin1.(1)) and
            p2=(int_of_float obstacle.coin2.(0),int_of_float obstacle.coin2.(1)) and
            p3=(int_of_float obstacle.coin3.(0),int_of_float obstacle.coin3.(1)) and
            p4=(int_of_float obstacle.coin4.(0),int_of_float obstacle.coin4.(1)) in
        let tab=[|p1;p2;p3;p4|] in
        (* on tri ces mêmes coins afin que l'affichage soit correct*)
        Array.sort tri_affichage tab;
        let tamp= tab.(2) in
        tab.(2)<- tab.(3);
        tab.(3)<- tamp;
        fill_poly tab;
    done;;

let affiche_pop population nb_humains couleur=
    (*pour chaque humain, on va afficher un cercle correcpondant au rayon assigné et 
    la couleur de l'humain changera en fonction du statut de sa connaissance du danger*)
    for i=0 to nb_humains-1 do
        let humain=population.(i) in
        if humain.danger=false then set_color green
        else set_color red;
        if humain.sorti=false then 
            fill_circle (int_of_float humain.position_h.(0)) (int_of_float humain.position_h.(1)) 
            (int_of_float humain.rayon);
    done;;
    
let affiche_sortie tab_sorties nb_sorties=
    (* affiche chaque sortie, en fonction de la position de la sortie les coins sont 
    différents*)
    set_color green;
    for i=0 to nb_sorties-1 do
    let sortie=tab_sorties.(i) in
    let pos=sortie.position in
    let tab=[|[|(0,0);(0,0);(0,0);(0,0)|]|] in
    if pos.(0)=0. then tab.(0)<-[| (0,int_of_float (pos.(1)+.sortie.largeur));
        (0,int_of_float (pos.(1)-.sortie.largeur));(5,int_of_float (pos.(1)+.sortie.largeur));
        (5,int_of_float (pos.(1)-.sortie.largeur)) |] 
    else if pos.(0)=(float_of_int dimx) then 
        tab.(0)<- [| (dimx,int_of_float (pos.(1)+.sortie.largeur));(dimx,
        int_of_float (pos.(1)-.sortie.largeur));(dimx-5,int_of_float (pos.(1)+.sortie.largeur));
        (dimx-5,int_of_float (pos.(1)-.sortie.largeur)) |]
    else if pos.(1)=0. then tab.(0)<- [| (int_of_float (pos.(0)+.sortie.largeur),0);
        (int_of_float (pos.(0)-.sortie.largeur),0);(int_of_float (pos.(0)+.sortie.largeur),5);
        (int_of_float (pos.(0)-.sortie.largeur),5) |] 
    else tab.(0)<-[| (int_of_float (pos.(0)+.sortie.largeur),dimy);
        (int_of_float (pos.(0)-.sortie.largeur),dimy);
        (int_of_float (pos.(0)+.sortie.largeur),dimy-5);
        (int_of_float (pos.(0)-.sortie.largeur),dimy-5) |];
    Array.sort tri_affichage tab.(0);
    let tamp= tab.(0).(2) in
    tab.(0).(2)<- tab.(0).(3);
    tab.(0).(3)<- tamp;
    fill_poly tab.(0);
    done;;
    
let affiche_direction population nb_pop =
    (* fonction permettant d'afficher la direction choisie par chaque personne*)
    set_color blue;
    let segments= Array.make nb_pop (0,0,0,0) in
    for i=0 to nb_pop-1 do
        if  not population.(i).sorti && population.(i).danger then
            let dir= population.(i).direction and pos=population.(i).position_h in
            let n= norme dir in
            segments.(i)<-(int_of_float (20.*.dir.(0)/.n +. pos.(0)), 
            int_of_float (20.*.dir.(1)/.n +. pos.(1)),
            int_of_float ( pos.(0)),int_of_float ( pos.(1)))
    done;
    draw_segments segments;;
    
(*simulation*)
let humain_aleatoire()=
    {position_h=[|Random.float ((float_of_int dimx)-.2.*.8.); 
    Random.float ((float_of_int dimy)-.2.*.8.)|]; vitesse_desire=3.*.65.1+. 
    Random.float (3.*.65.1); stress=Random.float 5.; congestion=0.; vitesse=0.; 
    direction=[|0.;0.|]; rayon=9. +. Random.float 5.; sorti=false; i_sortie=0;danger=false};;

let salle_de_classe_61()=
    let largeur_max_h= 23. in
    let obstacles= Array.make 38 {coin1=[|0.;0.|]; coin2=[|0.;0.|]; 
        coin3=[|0.;0.|]; coin4=[|0.;0.|]} in
    let sortie=[|{position=[|0.;0.5*.largeur_max_h|]; largeur= 2.*.largeur_max_h};
        {position=[|0.;650.-.2.5*.largeur_max_h|]; largeur= 2.*.largeur_max_h}|] in
    let matrice_congestion=Array.make_matrix dimx dimy 0. and n_ob=38 in
    (*tableau*)
    obstacles.(0)<-{coin1=[|66.;647.|]; coin2=[|585.;647.|]; coin3=[|585.;650.|]; coin4=[|66.;650.|]};
    (* étagères*)
    obstacles.(1)<-{coin1=[|2.*.largeur_max_h;0.|]; coin2=[|5.*.largeur_max_h;0.|]; 
        coin3=[|5.*.largeur_max_h;largeur_max_h*.1.5|]; coin4=[|largeur_max_h*.2.;
        largeur_max_h*.1.5|]};
    obstacles.(2)<-{coin1=[|650.-.5.*.largeur_max_h;0.|]; coin2=[|650.-.2.*.largeur_max_h;0.|];
        coin3=[|650.-.2.*.largeur_max_h;largeur_max_h*.1.5|]; coin4=[|650.-.largeur_max_h*.5.;
        largeur_max_h*.1.5|]};
    (*bureau professeur*)
    obstacles.(3)<-{coin1=[|650.-.5.*.largeur_max_h;650.-.3.5*.largeur_max_h|]; 
        coin2=[|650.-.2.*.largeur_max_h;650.-.3.5*.largeur_max_h|]; coin3=[|650.-.2.*.largeur_max_h;
        650.-.1.5*.largeur_max_h|]; coin4=[|650.-.5.*.largeur_max_h;650.-.1.5*.largeur_max_h|]};
    (* ajout des bureaux pour les élèves*)
    let o=[|2.25*.largeur_max_h;3.5*.largeur_max_h|] in(*coin d'origine*)
    let l=6.*.largeur_max_h and h=3.75*.largeur_max_h in
    for i=0 to 5 do
        for j=0 to 3 do
            let i2=float_of_int i and j2=float_of_int j in
            obstacles.(4+4*i+j)<-{coin1=[|o.(0)+.j2*.l;o.(1)+.i2*.h|]; coin2=[|o.(0)+.j2*.l +.
                3.5*.largeur_max_h;o.(1)+.i2*.h|]; coin3=[|o.(0)+.j2*.l+.3.5*.largeur_max_h;
                o.(1)+.i2*.h+.2.*.largeur_max_h|]; coin4=[|o.(0)+.j2*.l;o.(1)+.i2*.h+.
                2.*.largeur_max_h|]}
        done
    done;
    (*on a 24 tables de deux + 1 professeur, on doit donc placer 49 personnes dans la salle*)
    let population= Array.make 61 {position_h=[|0.;0.|]; vitesse_desire=0.;stress=0.; congestion=0.; 
        vitesse=0.; direction=[|0.;0.|]; rayon=0.; sorti=false; i_sortie=0; danger=false} in
    (* on place le professeur*)
    population.(0)<-{position_h=[|650.-.3.5*.largeur_max_h;650.-.0.75*.largeur_max_h|]; 
        vitesse_desire=3.*.65.1+. Random.float (3.*.65.1);stress=Random.float 5.; congestion=0.; 
        vitesse=0.; direction=[|0.;0.|]; rayon=13.; sorti=false; i_sortie=0; danger=false};
    let o_h=[|3.75*.largeur_max_h;2.75*.largeur_max_h|] in
    for i=0 to 5 do
        for j=0 to 3 do
            let i2=float_of_int i and j2=float_of_int j in
            let h1= humain_aleatoire() and h2=humain_aleatoire() in
            h1.position_h<-[|o_h.(0)+.j2*.l; o_h.(1)+.i2*.h|];
            h2.position_h<-[|o_h.(0)+.j2*.l+.1.5*.largeur_max_h; o_h.(1)+.i2*.h|];
            population.(1+8*i+ 2*j)<-h1;
            population.(2+8*i+ 2*j)<-h2;
        done
    done;
    (obstacles, sortie, n_ob, population, matrice_congestion);;

let dt=0.005;;

let affiche_matrice_congestion matrice obstacles n=
    let new_m=rend_uniforme matrice in
    let matrice_couleur=converti_matrice_couleur new_m in
    let matrice_tourne=rotation_droite matrice_couleur in
    dessiner_image matrice_tourne;
    affiche_obstacles obstacles n;;
    
(*échelle: 651 pixels =10m largeur moyenne d'épaule à épaule : 36cm = 23 pixels*)
let tab_obstacles, tab_sortie, n_ob, population, 
    matrice_congestion=salle_de_classe_61();;
let temps_total= ref 0.;;
let coeff_agmentation_r_propagation=80.;;
rayon_propagation:=50.;;
try 
    assigne_sortie population 61 tab_sortie 2;
    cre_danger population 61;
    open_graph "651x651+0";
    auto_synchronize false;
    while true do
        auto_synchronize false;
        clear_graph();
        affiche_obstacles tab_obstacles n_ob;
        affiche_sortie tab_sortie 2;
        affiche_pop population 61 red;
        affiche_direction population 61;
        temps_total:= !temps_total +. dt;
        avance dt coeff_agmentation_r_propagation tab_obstacles n_ob tab_sortie 2 population 61;
        let matrice=calcul_congestion_mat population 61 in
        ajoute_matrices matrice_congestion matrice;
        synchronize ();
    done;
with
    _ -> close_graph ();;