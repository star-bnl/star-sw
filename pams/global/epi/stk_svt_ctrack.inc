C stk_svt_ctrack.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C  This file was made by the idl compiler "stic". Do not edit.
C  Instead, edit the source idl file and then rerun the compiler.
C  For help, type contact Craig Tull or Herb Ward. 
C  COMMENTS FROM IDL FILE:
C  stk_svt_ctrack.idl
C      (onetasth.ace)
C  
C    Table: stk_svt_ctrack
C    SVT candidate tracks                    
C    essentially an intermediate table...    
C  
#ifndef STK_SVT_CTRACK_INC
#define STK_SVT_CTRACK_INC
#endif
	STRUCTURE /STK_SVT_CTRACK_ST/
	INTEGER*4 id !  candidate identification number 0 
	INTEGER*4 id_mctrack !  key to mkine table 0 
	INTEGER*4 nspt !  number of active spt in candidate 0 
	INTEGER*4 pid !  particle type (geant) 0 
	INTEGER*4 sec !  sec=0 unknown =1 primary =2 secondary 0 
	INTEGER*4 spt(8) !  space point indices in svt_spt table 0 
	END STRUCTURE
C
      CHARACTER*(*) STK_SVT_CTRACK_SPEC
      PARAMETER    (STK_SVT_CTRACK_SPEC=
     + ' struct stk_svt_ctrack {'           //
     + ' long id;'                          //
     + ' long id_mctrack;'                  //
     + ' long nspt;'                        //
     + ' long pid;'                         //
     + ' long sec;'                         //
     + ' long spt[8];'                      //
     + ' };')
C
