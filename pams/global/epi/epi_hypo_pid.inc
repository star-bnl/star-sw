C epi_hypo_pid.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    epi_hypo_pid.idl
C    Table: epi_hypo_pid
C         description: Contains all mass hypotheses for each   
#ifndef EPI_HYPO_PID_INC
#define EPI_HYPO_PID_INC
#endif
	STRUCTURE /EPI_HYPO_PID_ST/
	INTEGER*4 det !  detector:1-5 for svt,tpc,tof,emc,xtpc 0 
	INTEGER*4 gid !  Geant pid code # for this hypothesis 0 
	INTEGER*4 id !  identifier for rows corres. to globtrk 0 
	INTEGER*4 id_global_pid !  foreign key to global pid table 0 
	INTEGER*4 method !  report options in detec. specific pid 0 
	   REAL*4 nsigma !  # std.dev. from mean;(+)higher;(-)lower 0 
	   REAL*4 prob !  Probability for this mass hypothesis 0 
	   REAL*4 weight !  calculated weight for this detector &trk 0 
	END STRUCTURE
C
      CHARACTER*(*) EPI_HYPO_PID_SPEC
      PARAMETER    (EPI_HYPO_PID_SPEC=
     + ' struct epi_hypo_pid {'             //
     + ' long det;'                         //
     + ' long gid;'                         //
     + ' long id;'                          //
     + ' long id_global_pid;'               //
     + ' long method;'                      //
     + ' float nsigma;'                     //
     + ' float prob;'                       //
     + ' float weight;'                     //
     + ' };')
C
