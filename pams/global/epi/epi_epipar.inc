C epi_epipar.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    epi_epipar.idl
C    Table: epi_epipar
C         description: Control parameters for global PID module
#ifndef EPI_EPIPAR_INC
#define EPI_EPIPAR_INC
#endif
	STRUCTURE /EPI_EPIPAR_ST/
	INTEGER*4 ctrl_conflict !  =0 ignores conflicts,=1 resolves conflic     0 
	INTEGER*4 ctrl_sharp !  sharpness filter option                      0 
	INTEGER*4 ctrl_wt !  selects relative weights for detectors       0 
	   REAL*4 nsig_max !  #sigma cut-off                               0 
	   REAL*4 qfact_min !  min. fraction of tpc trk - mc to be good     0 
	   REAL*4 sharp_min !  minimum sharpness cut-off                    0 
	   REAL*4 w_emc !  Relative detector weight for the EMC         0 
	   REAL*4 w_svt !  Relative detector weight for the SVT         0 
	   REAL*4 w_tof !  Relative detector weight for the TOF         0 
	   REAL*4 w_tpc !  Relative detector weight for the TPC         0 
	   REAL*4 w_xtpc !  Relative detector weight for the XTPC        0 
	   REAL*4 weight_min !  min. weight cut-off used in conflict res     0 
	END STRUCTURE
C
      CHARACTER*(*) EPI_EPIPAR_SPEC
      PARAMETER    (EPI_EPIPAR_SPEC=
     + ' struct epi_epipar {'               //
     + ' long ctrl_conflict;'               //
     + ' long ctrl_sharp;'                  //
     + ' long ctrl_wt;'                     //
     + ' float nsig_max;'                   //
     + ' float qfact_min;'                  //
     + ' float sharp_min;'                  //
     + ' float w_emc;'                      //
     + ' float w_svt;'                      //
     + ' float w_tof;'                      //
     + ' float w_tpc;'                      //
     + ' float w_xtpc;'                     //
     + ' float weight_min;'                 //
     + ' };')
C
