:- lib(branch_and_bound).
:- lib(ic). 
:- lib(ic_global).
:- lib(listut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Search method used to call read the data into certain data structure and 
% Use branch bound search to search for optimal solution from find()
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search(Students):-
				 read_data(_,_,Students,Domain),
				 bb_min(find(Students, Domain,Dev),Dev,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use optimize to calculate deviation for student ranking and call labeling for the searching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 
find(Students,Domains, Dev):-
				 optimize(Students,Domains,1, Dev),
				 labeling(Students).

			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimize function used to calculate student deviation for its ranking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 						
optimize([],_,_,0).
optimize([SFirst|SRest],Domains,Student_index, Dev):-
				 nth1(Student_index,Domains,Dom),
				 element(Choice,Dom,SFirst),
				 Student_index_next is Student_index + 1,
				 optimize(SRest,Domains,Student_index_next, Dev2),
				 Dev #= sqr(Choice) + Dev2.
				 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Open, read and close used to load the data into data structure and assign students' choices 
% to different groups
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 						
read_data(Student_size,Seminar_size,Students,Domain) :- 
			 	 set_flag(print_depth,2000), 
				 open(test, read, S),
                 get_lines(S,X),
				 get_strings(X,Returned_list), 
				 close(S),
				 cal_student_numb(Returned_list, Student_size),
				 cal_seminar_numb(Returned_list, 0, Seminar_size),
				 length(Students,Student_size),
				 build_domains(Returned_list, Domain,1,[]),
				 constrain(Students,Domain),
				 alldifferent(Students,16).		

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper method used to read data to lines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 									 
				 get_lines(S, Lines) :-
				      ( read_string(S, end_of_line, _, Line) ->
				            Lines = [Line|Ls],
				            get_lines(S, Ls)
				      ;
				            Lines = []
				      ).
					  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper method used to seperate data to string
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 									 
				 get_strings([], []).
				 get_strings([X|Lines_in], [Y|Lines_out]) :- 
				 	  split_string(X, "\t", "", Y2),
					  number_string_process(Y2,Y),
					  get_strings(Lines_in,Lines_out).
		   	     number_string_process([X1|[X2|Y]],[Y1|[Y2|Y]]) :- 		
			 	   	  number_string(Y1,X1),
					  number_string(Y2,X2).
										 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% helper method used to set the contraint of student data to domain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				 									 
constrain([],[]).
constrain([SFirst|SRest],[Dfirst|Drest]):-
				 SFirst :: Dfirst, constrain(SRest,Drest). 


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
% calculate the size of seminars from the student data structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
cal_seminar_numb([], Y, Y).				 
cal_seminar_numb([Y|List], Seminar_max, Seminar_size) :- 
				 nth1(2,Y,Size,_), Size < Seminar_max,cal_seminar_numb(List, Seminar_max, Seminar_size).
cal_seminar_numb([Y|List], Seminar_max, Seminar_size) :- 
				 nth1(2,Y,Size,_), Size >= Seminar_max,cal_seminar_numb(List, Size, Seminar_size).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% custom searching strategy used to search the possible solutions
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
				 nth1(2,First,_,_), 
				 Student_numb_next is Student_numb +1,
				 build_domains([First|Rest], Domain2,Student_numb_next,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this function is used in processing the input to get it into the right form from out student data structure 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_unique_list_minus_one([], []).
build_unique_list_minus_one([ _ | [] ], []).
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
				 append([First],_,List),
				 build_unique_list_minus_one([ Second | Rest ], List). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% build the gender list used for future balance between two genders
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
build_gender([],[]).
build_gender([First|Rest],[1|Gender_list1]) :- 
				 nth1(3,First,Gender,_), 
				 Gender=="M",
				 build_gender(Rest,Gender_list1).
build_gender([First|Rest],[-1|Gender_list1]) :- 
				 nth1(3,First,Gender,_), 
				 Gender=="F",
				 build_gender(Rest,Gender_list1).
	
	
	
	
	
