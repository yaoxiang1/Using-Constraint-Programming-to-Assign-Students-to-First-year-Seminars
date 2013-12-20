:- lib(ic_hybrid_sets).
:- lib(branch_and_bound).
:- lib(listut).

run(Res):-
 		build_list(Returned_list), 
		build_unique_list(Returned_list, List), 
		build_gender(List, Gender_list), 
		build_intern(List, Intern_list),
		foo(Student_size,Seminar_size,Student_seminar_list,S2),
		
		create_empty(Seminar_size,EmptyRanks),
		add_all(Returned_list,EmptyRanks,Ranks,1,0),
		%print('\n'),
		%print(Ranks),
		%print('\n'),		

		student_traverse(Student_seminar_list,Res,S_size,M,F,I,D,Ranks,Rank_output,Gender_list,Intern_list),
		all_disjoint(M),
		all_disjoint(F),
		all_disjoint(I),
		all_disjoint(D),
		print("\n"),
		print("Numb_stu"),
		print("\n"),
		print(Student_size),
		working_sum(S_size,Student_size),
		cal_gender(M,F,D,I,Dev_gender),
		cal_rank(Rank_output,Dev_rank),
		
		%ic:(Dev#>0),
		ic:(Dev #= Dev_gender + 69 * Dev_rank) ,
		%find(Student_seminar_list, Student_size,S_size, Res,Dev,M,F,I,D,Gender_list,Intern_list).
		bb_min(find(Student_seminar_list, Student_size,S_size, Res,Dev,M,F,I,D,Gender_list,Intern_list,Rank_output),Dev,O).
		
test(X,Y,Z) :- ic:(Y::[1..10]) ,ic:(Z::[1..10]), ic:(X#= Y + Z).

		
cal_rank([],0).
cal_rank([[N1,N2,N3,N4,N5,N6]|Rest],Dev_rank):-
		#(N1,S1),
		#(N2,S2),
		#(N3,S3),
		#(N4,S4),
		#(N5,S5),
		#(N6,S6),
		ic:(S1 #>=0),
		ic:(S1 #=<16),
		cal_rank(Rest,Dev_rank1),
		ic:(Dev_rank #= Dev_rank1 + -43 * S1 + -25 * S2 +  -15 * S3 + -9 * S4 + -5 * S5 + -3 * S6).
				


build_list(Returned_list) :- set_flag(print_depth,500000), open(test, read, S), get_lines(S,X), get_strings(X,Returned_list).

build_unique_list([First|Rest],[First|List]) :- build_unique_list_minus_one([First|Rest],List).


build_unique_list_minus_one([], []).
build_unique_list_minus_one([ First | [] ], []).
build_unique_list_minus_one([ First | [ Second | Rest ] ], List) :- 
							nth1(1,First,Student_index1,_), 
							nth1(1,Second,Student_index2,_), 
							Student_index1 < Student_index2, 
							append([Second],List1,List),
							build_unique_list_minus_one([ Second | Rest ], List1). 
build_unique_list_minus_one([ First | [ Second | Rest ] ], List) :- 
							nth1(1,First,Student_index1,_), 
							nth1(1,Second,Student_index2,_), 
							Student_index1 = Student_index2, 
							build_unique_list_minus_one([ Second | Rest ], List). 
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
build_gender([],[]).
build_gender([First|Rest],[1|Gender_list1]) :- nth1(3,First,Gender,_), Gender=="M",build_gender(Rest,Gender_list1).
build_gender([First|Rest],[-1|Gender_list1]) :- nth1(3,First,Gender,_), Gender=="F",build_gender(Rest,Gender_list1).

build_intern([],[]).
build_intern([First|Rest],[1|Intern_list1]) :- nth1(4,First,Nation,_), Nation=="US",build_intern(Rest,Intern_list1).
build_intern([First|Rest],[-1|Intern_list1]) :- nth1(4,First,Nation,_), Nation\=="US",build_intern(Rest,Intern_list1).
				
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% foo(Student_size,Seminar_size,Student_seminar_list,S2),find(Student_seminar_list, Student_size,S_size, Student_Sem,Dev),
% build_list(Returned_list), build_unique_list(Returned_list, List), build_gender(List, Gender_list), build_intern(List, Intern_list).
% build_list(Returned_list), build_unique_list(Returned_list, List), build_gender(List, Gender_list), count_dif_gender_one_sem(Gender_list,[29,49,126,133,180,203,259,263,282,403,438,453,469,478,516,521],Gender_dif_count).
% build_list(Returned_list), build_unique_list(Returned_list, List), build_intern(List, Intern_list), count_dif_intern_one_sem(Intern_list,[29,49,126,133,180,203,259,263,282,403,438,453,469,478,516,521],Intern_dif_count).

%test(D):- S `:: [1,2,3,4,5]..[1,2,3,4,5],count_dif_gender_one_sem([1,1,1,-1,-1],S,D).


% Count the difference of gender in one seminar
count_dif_gender_one_sem(Gender_list,[],0).
count_dif_gender_one_sem(Gender_list,[First_student_id|Rest],Numb_gender_dif) :- 
					ic:element(First_student_id,Gender_list,Gender_binary),
					count_dif_gender_one_sem(Gender_list,Rest,Numb_gender_dif1),Numb_gender_dif #= Numb_gender_dif1 + Gender_binary.

% Count the difference of international in one seminar
count_dif_intern_one_sem(Intern_list,[],0).
count_dif_intern_one_sem(Intern_list,[First_student_id|Rest],Numb_intern_dif) :- 
					ic:element(First_student_id,Intern_list,Intern_binary),
					count_dif_intern_one_sem(Intern_list,Rest,Numb_intern_dif1),Numb_intern_dif #= Numb_intern_dif1 + Intern_binary.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
							
% open, read and close
foo(Student_size,Seminar_size,Student_seminar_list,Returned_list) :- 
			 	 set_flag(print_depth,2000), 
				 open(test, read, S),
                 get_lines(S,X),
				 get_strings(X,Returned_list), 
				 close(S),
				 cal_student_numb(Returned_list, Student_size),
				 cal_seminar_numb(Returned_list, 0, Seminar_size),
				 create_upper_bound(Returned_list,1,Seminar_size,Student_seminar_list),
				 build_domains(Returned_list, Domain,1,[]),
				 print(Domain).				 
				 
				 
				 get_lines(S, Lines) :-
				        ( read_string(S, end_of_line, _, Line) ->
				            Lines = [Line|Ls],
				            get_lines(S, Ls)
				        ;
				            Lines = []
				        ).

				 get_strings([], []).
				 get_strings([X|Lines_in], [Y|Lines_out]) :- 
				 				split_string(X, "\t", "", Y2),
								number_string_process(Y2,Y),
								get_strings(Lines_in,Lines_out).
								
								
			     number_string_process([X1|[X2|Y]],[Y1|[Y2|Y]]) :- 		
				 	   			number_string(Y1,X1),
								number_string(Y2,X2).

						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_domains([], [Domain], _, Domain).
build_domains([First|Rest], Domain,Student_numb,Student_domain):-
	nth1(1,First,Student_index1,_), 
	Student_index1 = Student_numb,
	nth1(2,First,Student_seminar,_), 
	append(Student_domain,[Student_seminar],Student_domain2),
	build_domains(Rest, Domain,Student_numb,Student_domain2).
build_domains([First|Rest], [Student_domain|Domain2],Student_numb,Student_domain):-
	nth1(1,First,Student_index1,_), 
	Student_index1 > Student_numb,
	nth1(2,First,Student_seminar,_), 
	Student_numb_next is Student_numb +1,
	build_domains([First|Rest], Domain2,Student_numb_next,[]).





create_upper_bound(L, Current_seminar ,Seminar_size, R_list) :- 
				Current_seminar > Seminar_size, R_list = [].
create_upper_bound(L, Current_seminar ,Seminar_size, [Upperbound|R_list]) :- 
				Current_seminar =< Seminar_size, 
				cal_upper(L, Current_seminar, Upperbound),
				Current_seminar1 is Current_seminar+1,
				create_upper_bound(L, Current_seminar1 ,Seminar_size, R_list).

cal_upper([], Current_seminar, []).
cal_upper([[Student_numb|[Seminar_numb|_]]|Rest], Current_seminar, [Student_numb|Rest1]) :- 
			Current_seminar = Seminar_numb,
			cal_upper(Rest,Current_seminar, Rest1). 
cal_upper([[Student_numb|[Seminar_numb|_]]|Rest], Current_seminar, Rest1) :- 
			Current_seminar \== Seminar_numb,
			cal_upper(Rest, Current_seminar, Rest1). 


create_rank_set(Domain, Current_seminar ,Seminar_size, R_list) :- 
				Current_seminar > Seminar_size, R_list = [].
create_rank_set(Domain, Current_seminar ,Seminar_size, [Upperbound|R_list]) :- 
				Current_seminar =< Seminar_size, 
				cal_rank(Domain, Current_seminar, Upperbound),
				Current_seminar1 is Current_seminar+1,
				create_rank_set(Domain, Current_seminar1 ,Seminar_size, R_list).

cal_rank([], Current_seminar, []).
cal_rank([[Student_numb|[Seminar_numb|_]]|Rest], Current_seminar, [Student_numb|Rest1]) :- 
			Current_seminar = Seminar_numb,
			cal_upper(Rest,Current_seminar, Rest1). 
cal_rank([[Student_numb|[Seminar_numb|_]]|Rest], Current_seminar, Rest1) :- 
			Current_seminar \== Seminar_numb,
			cal_upper(Rest, Current_seminar, Rest1). 

create_empty(Seminar_size,Ranks):- 
			length(Ranks,Seminar_size),
			create_one(Ranks).			

create_one([]).
create_one([[[],[],[],[],[],[]]|Rest]):-
		 	create_one(Rest).
	
	
add_all([],Ranks,Ranks,_,_).
add_all([[Student,Seminar|_]|Students_rest],IRank,ORanks,Rank,Student):-
			Rank1 is Rank + 1,
			add_all(Students_rest,IRank,ORanks2,Rank1,Student),
			add_one(Student,Seminar,Rank,ORanks2,ORanks).
add_all([[Student,Seminar|_]|Students_rest],IRank,ORanks,Rank,Student1):-
			Student \== Student1,add_one(Student,Seminar,1,IRank,ORanks2),
			add_all(Students_rest,ORanks2,ORanks,2,Student).


		
	
	
add_one(Student,1,Rank,[Rank_f|Rank_r],[OutRank_f|Rank_r]):- 
					add_one_helper(Student,Rank,Rank_f,OutRank_f).
add_one(Student,Seminar,Rank,[Rank_f|Rank_r],[Rank_f|OutRank_f]):- 
					Seminar>1,
					Seminar1 is Seminar - 1,
					add_one(Student,Seminar1,Rank,Rank_r,OutRank_f).
						

add_one_helper(Student,1, [Rank_f|Rank_r],[[Student | Rank_f]|Rank_r]).
add_one_helper(Student,Rank, [Rank_f|Rank_r],[Rank_f|OutRank_f]):- 
					Rank > 1, Rank1 is Rank - 1,add_one_helper(Student,Rank1,Rank_r,OutRank_f).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% method used to calculate the students' number by access the last item using uth1 
% and then access the first item to get the student number
% nth1 is used to access the first element in a list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cal_student_numb(L, Student_size) :- 
									 length(L,Size),
									 nth1(Size,L,Element,_),
									 nth1(1,Element,Student_size,_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			
cal_seminar_numb([], Y, Y).				 
cal_seminar_numb([Y|List], Seminar_max, Seminar_size) :- 
		nth1(2,Y,Size,_), Size < Seminar_max,cal_seminar_numb(List, Seminar_max, Seminar_size).
cal_seminar_numb([Y|List], Seminar_max, Seminar_size) :- 
		nth1(2,Y,Size,_), Size >= Seminar_max,cal_seminar_numb(List, Size, Seminar_size).
				 

student_in_seminar_list(Student) :-
			Student = [[1,2,5,6,8,9,10],[1,3,4,5,7,8],[2,3,4,6,7,9,10]].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the set for rank, gender, domester, 




student_traverse([],[],[],[],[],[],[],[],[],_,_).
% skip seminar that no one request it
student_traverse([[]|Y],Res,S_size,M_rest,F_rest,I_rest,D_rest,  [_|Ranks_list],  Ranks_list_out,  Gender_list, Intern_list) :-
			student_traverse(Y,Res,S_size, M_rest,F_rest,I_rest,D_rest, Ranks_list,  Ranks_list_out,  Gender_list, Intern_list).
student_traverse([X|Y],[X1|Res],[XS1|S_size],[M|M_rest],[F|F_rest],[I|I_rest],[D|D_rest],[First_r|Rest_r],  [First_r_out|Rest_r_out],  Gender_list, Intern_list) :-
			X1 :: []..X,
			#(X1, XS1),			
			%integers(XS1),
			%XS1#>=0,
			%XS1#=<17,
			%%%CHANGE
			filter(X,1,Gender_list,MD),
			filter(X,-1,Gender_list,FD),
			filter(X,1,Intern_list,DD),
			filter(X,-1,Intern_list,ID),
			M :: []..MD,
			F :: []..FD,
			D :: []..DD,
			I :: []..ID,
			#(M, XS_M),
			#(I, XS_I),
			ic:(XS_M::[3..10]),
			ic:(XS_I::[0..3]),
			all_union([M,F],X1),
			all_union([D,I],X1),
			%XS1::[15..16],
			ic:(XS1#>8),			
			ic:(XS1#<17),
			First_r = [N1,N2,N3,N4,N5,N6],		
			R1 :: []..N1,
			R2 :: []..N2, 
			R3 :: []..N3, 
			R4 :: []..N4, 
			R5 :: []..N5, 
			R6 :: []..N6, 
			all_union([R1,R2,R3,R4,R5,R6],X1),
			First_r_out = [R1,R2,R3,R4,R5,R6],
			student_traverse(Y,Res,S_size, M_rest,F_rest,I_rest,D_rest,Rest_r,Rest_r_out,Gender_list, Intern_list).


	
get_rank([],_, []).
get_rank([First|Rest],Index, [Set|List_set2]):-
			nth1(Index,First,Set),
			get_rank(Rest,Index,List_set2).
			
			


filter([],_,_, []).
filter([FirstS|RestS],Value,Type_list, Rest):-
			ic:element(FirstS,Type_list,V), V \= Value, filter(RestS,Value,Type_list, Rest).
filter([FirstS|RestS],Value,Type_list, [FirstS|Rest]):-
			ic:element(FirstS,Type_list,V), V = Value, filter(RestS,Value,Type_list, Rest).
 
find(Student, Numb_student, S_size, Res, Dev,M,F,I,D, Gender_list, Intern_list, Ranks) :- 
			%append(I,D,All),
			%flatten(Ranks,Rf),
			%append(M,All,All2),
			%append(All2,Rf,R3),
			
			
			%get_rank(Ranks,1,Search_set1),
			%get_rank(Ranks,2,Search_set2),	
			%get_rank(Ranks,3,Search_set3),	
			%get_rank(Ranks,4,Search_set4),	
			%get_rank(Ranks,5,Search_set5),	
			%get_rank(Ranks,6,Search_set6),	
			
			append(M,F,T),
			%append(Search_set1,T,T2),
			labeling_smallest_glb(T),
			
			
			%labeling_smallest_glb(Search_set1),
			%labeling_smallest_glb(Search_set2),			
			%labeling_smallest_glb(F),		
					
			%labeling_smallest_glb(Search_set3),		
			%labeling_smallest_glb(Search_set4),
			%labeling_smallest_glb(Search_set5),
			%labeling_smallest_glb(Search_set6),
			
			
			
			%cal_male(Res,Gender,M_size,Dev),
			print('\nDev: '),
			print(Dev),
			print('\n'),
			print('-> Male: '),
			cal_list_count(M),
			print('\n'),
			print('-> Intern: '),
			cal_list_count(I),
			print('\n'),
			print('-> Size: '),
			cal_list_count(Res),
			print('\n'),
			print('-> First Choice: '),
			%print(Ranks),
			cal_rank_size(Ranks,Total_student_1,1),
			cal_rank_size(Ranks,Total_student_2,2),
			cal_rank_size(Ranks,Total_student_3,3),
			cal_rank_size(Ranks,Total_student_4,4),
			cal_rank_size(Ranks,Total_student_5,5),
			cal_rank_size(Ranks,Total_student_6,6),		
			print(Total_student_1+" "+Total_student_2+" "+Total_student_3+" "+Total_student_4+" "+Total_student_5+" "+Total_student_6),
			print('\n'),
			open(foo,write,S2),
			write(S2,M),
			write(S2,"\n"),
			write(S2,I),
			close(S2).


cal_rank_size([],0,_).
cal_rank_size([First|Rest],F1_total,Index):- nth1(Index,First,N1), cal_rank_size(Rest,F1_total1,Index), length(N1, Total_size),F1_total is Total_size + F1_total1.



cal_size_type([],[]).
cal_size_type([First|Rest],[Size|Size_list]):- #(First, Size),cal_size_type(Rest,Size_list).

		
cal_list_count([]).
cal_list_count([First|Rest]):-
			length(First,N), 
			print(N), 
			print(' '),
			cal_list_count(Rest).
			
count_list_M([],0).
count_list_M([1|Rest],Total):- count_list_M(Rest,Total1),Total is Total1+1.
count_list_M([-1|Rest],Total):- count_list_M(Rest,Total).

count_list_I([],0).
count_list_I([-1|Rest],Total):- count_list_I(Rest,Total1),Total is Total1+1.
count_list_I([1|Rest],Total):- count_list_I(Rest,Total).


cal_gender([],[],[],[],0).
cal_gender([M1|MRest],[F1|FRest],[D1|DRest],[I1|IRest],Dev) :-
			#(M1,MS),
			#(F1,FS),
			ic:(DifG #= MS-FS),	
			#(D1,DS),
			#(I1,IS),
			ic:(DifC #= DS-IS),				
			cal_gender(MRest,FRest,DRest,IRest,Dev1),
			ic:(Dev #= Dev1 + 23*sqr(DifG)+ 8*sqr(DifC)).
			%set_range(M1,LB,UB),
			%print(LB),
			%print('\n'),
			%print(UB),
			%print('\n').
			

working_sum([],0).
working_sum([First|Rest],Sum) :-
working_sum(Rest,Sum2),ic:(Sum #= Sum2+First).		

bubble_sort_process(List1,List1,X_size):- length(List1,X), X==X_size.
bubble_sort_process(List1,List2,X_size):-
X_size < length(List1),bubble_sort(List1,List3),X is X_size + 1,bubble_sort_process(List3,List2,X).



bubble_sort([],[]).
bubble_sort([X],[X]).
bubble_sort([X,Y|XL],[X|ZL]):-
length(X) =< length(Y),bubble_sort([Y|XL],ZL).	
bubble_sort([X,Y|XL],[Y|ZL]):-
length(X) > length(Y),bubble_sort([X|XL],ZL).		


search(Seminar_size,Dev):-
 foo(Student_size,Seminar_size,Student_seminar_list,S2),find(Student_seminar_list, Student_size,S_size,Res),cal_dev_size(Seminar_size,Dev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% now not cal size for each seminar, cal numb of (male - female)^2, sum for all seminar 
% loop through student, build up G=[1,-1,-1,1,] I:[-1,1,0,-1,0]
% male +1, female -1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cal_dev_gender([],Gender_list,0).			 
cal_dev_gender([First_sem|Rest_sem],Gender_list,Dev):- 
			%lub(First_sem, FS),
			count_dif_gender_one_sem(Gender_list,First_sem,Gender_dif),cal(Gender_dif,Gender_dif_one),cal_dev_gender(Rest_sem,Gender_list,Dev2),Dev #= Gender_dif_one + Dev2.


cal_male([],_,_,0).			 
cal_male([First_sem|Rest_sem],Gender_list,[First_male|Rest_male],Dev):- 
			%lub(First_sem, FS),
			count_male(Gender_list,First_sem,Count_male),
			First_male #>= Count_male, 
			#(First_sem,S),
			ic:(Gender_dif #= Count_male-(S-Count_male)) ,
			cal(Gender_dif,Gender_dif_one),
			%print(Gender_dif_one),
			%print(' '),
			cal_male(Rest_sem,Gender_list,Rest_male,Dev2),ic:(Dev #= Gender_dif_one + Dev2).


count_male(Gender_list,[],0).
count_male(Gender_list,[First_per|Rest_per],Count_male):-
		ic:element(First_per,Gender_list,1), count_male(Gender_list,Rest_per,Count_male1),Count_male #= Count_male1+1 .
count_male(Gender_list,[First_per|Rest_per],Count_male):-
		ic:element(First_per,Gender_list,-1), count_male(Gender_list,Rest_per,Count_male).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cal_cons_gender([],Gender_list).			 
cal_cons_gender([First_sem|Rest_sem],Gender_list):- 
			lub(First_sem, FS),
			count_dif_gender_one_sem(Gender_list,FS,Gender_dif), print(Gender_dif),print(' '), cal_cons_gender(Rest_sem,Gender_list).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cal_dev_size([],0).			 
cal_dev_size([First|Rest],Dev):-
New_First #= 16-First, cal(New_First,Dev1), cal_dev_size(Rest,Dev2),Dev #= Dev1+Dev2.

	
