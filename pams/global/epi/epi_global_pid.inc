C epi_global_pid.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    epi_global_pid.idl
C    Table: epi_global_pid
C         description: final PID assignment for global tracks  
#ifndef EPI_GLOBAL_PID_INC
#define EPI_GLOBAL_PID_INC
#endif
	STRUCTURE /EPI_GLOBAL_PID_ST/
	INTEGER*4 flag !  reports pid info that passes filters 0 
	INTEGER*4 id !  unique primary key 0 
	INTEGER*4 method !  reports epi module control settings 0 
	INTEGER*4 most_prob_id !  Geant PID code # for most probable mass 0 
	INTEGER*4 nhypo !  # rows in hypo_pid for this track 0 
	   REAL*4 quality !  sharpness of final pid probability dist. 0 
	END STRUCTURE
C
      CHARACTER*(*) EPI_GLOBAL_PID_SPEC
      PARAMETER    (EPI_GLOBAL_PID_SPEC=
     + ' struct epi_global_pid {'           //
     + ' long flag;'                        //
     + ' long id;'                          //
     + ' long method;'                      //
     + ' long most_prob_id;'                //
     + ' long nhypo;'                       //
     + ' float quality;'                    //
     + ' };')
C
