			
			 --- Converts Hayes,John to John Hayes ---
			 --- Works with no spaces after comma ---	
select concat(concat(substr(Agent_Name, instr(Agent_Name, ',') + 1, length(Agent_Name)), 
				     ' '), 
			  substr(Agent_Name, 1, instr( Agent_Name, ',')-1))
			  
			  
			 --- Converts 'Hayes, John' to 'John Hayes' ---
			 --- Works with spaces after comma ----
select trim(concat(concat(substr(EOM_REPTV_FULL_NM, instr(EOM_REPTV_FULL_NM, ',') + 1, length(EOM_REPTV_FULL_NM)), 
                     ' '), 
              substr(EOM_REPTV_FULL_NM, 1, instr( EOM_REPTV_FULL_NM, ',')-1)))
			  
			  
			  
			  
			  
			  ----- ALTERNATE WAY -----
			  
			 --- Converts Hayes,John to John Hayes ---
			 --- Works with no spaces after comma ---	
select substr(Agent_Name, instr(Agent_Name, ',') + 1, length(Agent_Name)) 
	    || ' '
		|| substr(Agent_Name, 1, instr( Agent_Name, ',')-1)
			  
			  
			  
			 --- Converts 'Hayes, John' to 'John Hayes' ---
			 --- Works with spaces after comma ----
select trim(substr(EOM_REPTV_FULL_NM, instr(EOM_REPTV_FULL_NM, ',') + 1, length(EOM_REPTV_FULL_NM)) 
			|| ' '
			|| substr(EOM_REPTV_FULL_NM, 1, instr( EOM_REPTV_FULL_NM, ',')-1))
			  		  
			  
			  