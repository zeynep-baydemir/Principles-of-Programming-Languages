% zeynep baydemir
% 2019400096
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :-
% With manhattan distance we can find distance between two locations.
manhattan_distance(A, B, Distance) :- nth0(0,A,A1), nth0(1,A,A2), nth0(0,B,B1), nth0(1,B,B2),Distance is abs(A1-B1) + abs(A2-B2).

% 10 points
% minimum_of_list(+List, -Minimum) :- .
minimum_of_list(List, M) :- min_list(List,M).

% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .
% I find all objects with given type then find their distances and put them with their keys in dictionary. Finding minimum of values of dictionary.

append([],[],[]).
append([], X, X).                                   
append(X,[],X).
append( [X | Y], Z, [X | W]) :- append( Y, Z, W). 

rec_min_dist(Bag, _Values, _ObjDict, _Dict,_N,_X,_Y,_DictR):- length(Bag,L), L==0,!.

rec_min_dist(Bag, _Values,ObjDict, Dict,N,X,Y,DictR):- length(Bag,L), M is N+1,L==M,!,nth0(N, Bag, Ind1),find_dist(Ind1,ObjDict,X,Y,D1), put_dict(Ind1,Dict,D1,DictR).
rec_min_dist(Bag, Values, ObjDict, Dict,N,X,Y,DictR):- length(Bag,L), M is N+1,L>M,nth0(N, Bag, Ind1),find_dist(Ind1,ObjDict,X,Y,D1),rec_min_dist(Bag, Values,ObjDict,Dict,M,X,Y,Res),put_dict(Ind1,Res,D1,DictR).

find_dist(Ind, ObjDict,X,Y,D):-get_dict(Ind, ObjDict,Obj), X1 is Obj.x, Y1 is Obj.y,manhattan_distance([X,Y],[X1,Y1],D) .

find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :- nth0(0,State,A),nth0(1,State,O), 
								  dict_pairs(O,_Tag,Pairs),pairs_keys_values(Pairs,_Keys,Values),findall(X,O.X.type=ObjectType,Bag), 
								  (Bag == [] -> (fail) ;
								  (get_dict(x,A,X),get_dict(y,A,Y),
								  rec_min_dist(Bag,Values,O,[],0,X,Y,DictR),
								  dict_pairs(DictR,_Tag2,Disind),pairs_keys_values(Disind,_Indexes,Dists),
								  minimum_of_list(Dists,Distance),
								  get_dict(ObjKey,DictR,Distance),Object = O.ObjKey)).

% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .

% I find vertical distance and horizontal distance. Depending on its sign I call recursive function to write necessary actions. 

append_rec(List, 1, Action):- append([],Action,List).
append_rec(List, Number, Action) :-  M is Number-1,append_rec(NewList, M, Action),append(NewList,Action,List). 
subst(X,X1,Subs) :- Subs is X-X1.
writingx_acts(X,X1,_List):- subst(X,X1,O), O == 0,!.
writingx_acts(X,X1,List):- subst(X,X1,O), O > 0,!,append_rec(List,O,[go_right]).
writingx_acts(X,X1,List):- subst(X,X1,O),P is -1*O,append_rec(List,P,[go_left]).
writingy_acts(Y,Y1,_List):- subst(Y,Y1,O), O == 0,!.
writingy_acts(Y,Y1,List):- subst(Y,Y1,O), O > 0,!,append_rec(List,O,[go_down]).
writingy_acts(Y,Y1,List):- subst(Y,Y1,O),P is -1*O,append_rec(List,P,[go_up]).
navigate_to(State, X, Y, ActionList, DepthLimit):- nth0(0,State,A), get_dict(x,A,X1), get_dict(y,A,Y1),
						   manhattan_distance([X1,Y1],[X,Y],Len), Len =< DepthLimit,!,
						   writingx_acts(X,X1,Listx),writingy_acts(Y,Y1,Listy),
						   append(Listx,Listy,ActionList).


% 10 points
% chop_nearest_tree(+State, -ActionList) :- .

wrt_click_acts(0,_List).
wrt_click_acts(P,List):- P > 0,!,append_rec(List,P,[left_click_c]).

% I find nearest tree, navigate to it, then write necessary actions.

chop_nearest_tree(State, ActionList) :- find_nearest_type(State, tree, _Key, Obj, Dist),
					Xo is Obj.x, Yo is Obj.y, navigate_to(State, Xo, Yo, ActList, Dist),
					Hp is Obj.hp, Hp1 is Hp+1, wrt_click_acts(Hp1,ChList), append(ActList,ChList,ActionList).

% 10 points
% mine_nearest_stone(+State, -ActionList) :- .			     
	
% I find nearest stone, navigate to it, then write necessary actions.
mine_nearest_stone(State, ActionList) :- find_nearest_type(State, stone, _Key, Obj, Dist),
					 Xo is Obj.x, Yo is Obj.y, navigate_to(State, Xo, Yo, ActList, Dist),
					 Hp is Obj.hp, Hp1 is Hp+1, wrt_click_acts(Hp1,ChList), append(ActList,ChList,ActionList).


% 10 points
% gather_nearest_food(+State, -ActionList) :- .

%I find nearest food, navigate to it, then write necessary actions.				
gather_nearest_food(State, ActionList) :- find_nearest_type(State, food, _Key, Obj, Dist),
					  Xo is Obj.x, Yo is Obj.y, navigate_to(State, Xo, Yo, ActList, Dist),
					  Hp is Obj.hp, Hp1 is Hp+1, wrt_click_acts(Hp1,ChList), append(ActList,ChList,ActionList).	




% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- nth0(0,State,A).

% With log_cont, stick_cont, cobble_cont I check whether I have enough item.
% with is_member, I check whether agent has that type in inventory and if he has item, then find number of it.
% with collect_requirements I check number of items and implement necessary actions.

log_cont(Need, Have, Out) :- Have >= Need -> (Out is 1); (Out is 0).
		    
stick_cont(Need, Have, HaveLog, O) :- Have >= Need -> (O is 1); 
						      (LogNeed is (Need-Have)*2-HaveLog , log_cont(LogNeed, HaveLog,O)).

cobble_cont(Need, Have, Out) :- Have >= Need -> (Out is 1); (Out is 0).

is_member(Dict, Type, Number) :- dict_pairs(Dict,_Tag, Pairs), pairs_keys_values(Pairs, Keys, _Values),
				 member(Type,Keys) -> (get_dict(Type, Dict, Number));
						      (Number is 0).					  
								

collect_requirements(State, stick, ActionList) :- nth0(0,State,A), dict_pairs(A.inventory, _Tag, Pairs), pairs_keys_values(Pairs, Keys, _Values),member(log,Keys),!,
						  get_dict(log,A.inventory, Num),
						  (Num >= 2) -> (ActionList = []); 
								(chop_nearest_tree(State,ActionList)).	

				  
collect_requirements(State, stick, ActionList) :- nth0(0,State,A),
						  chop_nearest_tree(State,ActionList).

collect_requirements(State, stone_pickaxe, ActionList) :- nth0(0,State,A), 
							  is_member(A.inventory, log, LogN), log_cont(3, LogN, Log),
							  is_member(A.inventory, stick, StickN), stick_cont(2, StickN, LogN,Stick), 
							  is_member(A.inventory, cobblestone,CobN), cobble_cont(3, CobN, Cobb),
							  X is Log + Stick + Cobb,
							  X < 3,!,chop_nearest_tree(State,Acts1),execute_actions(State, Acts1,FS1),
								  chop_nearest_tree(FS1,Acts2),execute_actions(FS1, Acts2,FS2),
								   mine_nearest_stone(FS2,Acts3),
								   append(Acts1,Acts2,A1),append(A1,Acts3,A2),append(A2,[craft_stick],ActionList).




collect_requirements(State, stone_axe, ActionList) :- nth0(0,State,A), 
							  is_member(A.inventory, log, LogN), log_cont(3, LogN, Log),
							  is_member(A.inventory, stick, StickN), stick_cont(2, StickN, LogN,Stick), 
							  is_member(A.inventory, cobblestone,CobN), cobble_cont(3, CobN, Cobb),
							  X is Log + Stick + Cobb,
							  X < 3,!,chop_nearest_tree(State,Acts1),execute_actions(State, Acts1,FS1),
								     chop_nearest_tree(FS1,Acts2),execute_actions(FS1, Acts2,FS2),
								     mine_nearest_stone(FS2,Acts3),
								     append(Acts1,Acts2,A1),append(A1,Acts3,A2),append(A2,[craft_stick],ActionList).
collect_requirements(State, stone_axe, ActionList) :- nth0(0,State,A),
							  is_member(A.inventory, log, LogN), log_cont(3, LogN, _Log),
							  is_member(A.inventory, stick, StickN), stick_cont(2, StickN, LogN, _Stick),
							  is_member(A.inventory, cobblestone,CobN), cobble_cont(3, CobN, _Cobb),							    
							  ActionList = [].
						  
%  trace,state(A, O, T), State=[A, O, T],collect_requirements(State, stone_pickaxe, Acts).

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .

% unavailable predicate checks whether that location is empty
% square_cont checks whether surrounding tiles empty
unavailable(State, X, Y) :- nth0(1, State, O), get_dict(_, O, Obj), 
			    Xo is Obj.x, Yo is Obj.y,
			    X = Xo, Y = Yo.



square_cont(State, X, Y, MaxX, MaxY,Xr,Yr) :-   Xa is X+1, Xe is X-1, Ya is Y+1, Ye is Y-1,
						(unavailable(State, X, Y) -> (
						(Xa) =< MaxX -> (
							square_cont(State, Xa, Y, MaxX, MaxY,Xr,Yr)
			 	        	        ); ( 
							(Ya) =< MaxY -> (
						    		square_cont(State, 2,Ya, MaxX, MaxY,Xr,Yr)
					                        );(
								false
							)		
	 					)
		 	                      ); (     
						(unavailable(State, Xe, Ye);
						 unavailable(State, X, Ye); 
						 unavailable(State, Xa, Ye);
						 unavailable(State, Xe, Y);
						 unavailable(State, Xa, Y);
						 unavailable(State, Xe, Ya);
						 unavailable(State, X, Ya);
						 unavailable(State, Xa, Ya)) -> (
						   (Xa) =< MaxX -> (
							square_cont(State, Xa, Y, MaxX, MaxY,Xr,Yr)
				        		); ( 
							(Ya) =< MaxY -> (
						    	     square_cont(State, 2,Ya, MaxX, MaxY,Xr,Yr)
					                     );(
							     false
							     )		
	 					       )
						  );(
						  Xr is X-1, Yr is Y-1 ))).


find_castle_location(State, XMin, YMin, XMax, YMax) :- width(W), height(H),
						       square_cont(State, 2, 2, W-2, H-2, XMin, YMin), 
						       XMax is XMin+2, YMax is YMin+2.  
							     


% 15 points
% make_castle(+State, -ActionList) :- .

% mine_nearest_cobble mine nearest stones, if there isnt enough stones, mine cobblestones.
% call_mine_cobb mine cobbles recursively
% make_castle checks inventory of agent and find number of required cobblestones, then mine them. Finds castle location and place castle.
mine_nearest_cobble(State, ActionList, Number) :- mine_nearest_stone(State, ActionList),Number = 3,!.
					      

mine_nearest_cobble(State, ActionList, Number) :- find_nearest_type(State, cobblestone, _Key, Obj, Dist),
					          Xo is Obj.x, Yo is Obj.y, navigate_to(State, Xo, Yo, ActList, Dist),
					          Hp is Obj.hp, Hp1 is Hp+1, wrt_click_acts(Hp1,ChList), append(ActList,ChList,ActionList),Number = 1 .

cobb_need(Need, Have, Out) :- Out is Need - Have.

call_mine_cobb(Number,_State,ActList, List) :- Number =<0,!,List = ActList.
call_mine_cobb(Number,State,ActList, List) :- mine_nearest_cobble(State, Acts1,N), execute_actions(State, Acts1, FS1), Num is Number-N, append(ActList, Acts1, NewList), call_mine_cobb(Num,FS1,NewList,List) .


make_castle(State, ActionList) :- nth0(0,State,A), 
				  dict_pairs(A.inventory, _Tag, Pairs),
				  is_member(A.inventory, cobblestone,CobN), cobb_need(9, CobN, Cobb),
				  call_mine_cobb(Cobb, State, ActList, CobbActs), execute_actions(State, CobbActs, CobbState),
				  find_castle_location(CobbState, XMin, YMin, _XMax, _YMax), Xn is XMin+1, Yn is YMin+1,
 				  nth0(0,CobbState, Agent),
				  manhattan_distance([Xn,Yn],[Agent.x,Agent.y],Dist), 
				  navigate_to(CobbState, Xn, Yn, NavActs, Dist),
				  append([CobbActs, NavActs,[place_c,place_e,place_n,place_w,place_s,place_ne,place_nw,place_sw,place_se]], ActionList).







	

















