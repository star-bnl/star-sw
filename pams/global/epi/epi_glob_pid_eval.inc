C epi_glob_pid_eval.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    epi_glob_pid_eval.idl
C    Table: epi_glob_pid_eval
C         description: Evaluation data for each row in table   
#ifndef EPI_GLOB_PID_EVAL_INC
#define EPI_GLOB_PID_EVAL_INC
#endif
	STRUCTURE /EPI_GLOB_PID_EVAL_ST/
	INTEGER*4 id !  primary key = global_pid.id              0 
	INTEGER*4 mc_check !  =1(0) if glb. trk. is(not) good MC track 0 
	INTEGER*4 mc_pid !  if mc_check=1 is the Geant pid code #    0 
	INTEGER*4 pid !  Reconstr.PID #, copied from global_pid   0 
	INTEGER*4 svt_seg !  indicates if global track has svt segmnt 0 
	INTEGER*4 tof_seg !  indicates if global track has TOF hit    0 
	INTEGER*4 tpc_seg !  indicates if global track has tpc segmnt 0 
	   REAL*4 eta !  pseudorapidity for global track          0 
	   REAL*4 invpt !  1/pt copied from globtrk.invpt           0 
	END STRUCTURE
C
      CHARACTER*(*) EPI_GLOB_PID_EVAL_SPEC
      PARAMETER    (EPI_GLOB_PID_EVAL_SPEC=
     + ' struct epi_glob_pid_eval {'        //
     + ' long id;'                          //
     + ' long mc_check;'                    //
     + ' long mc_pid;'                      //
     + ' long pid;'                         //
     + ' long svt_seg;'                     //
     + ' long tof_seg;'                     //
     + ' long tpc_seg;'                     //
     + ' float eta;'                        //
     + ' float invpt;'                      //
     + ' };')
C
