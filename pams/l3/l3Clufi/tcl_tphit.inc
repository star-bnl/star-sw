C tcl_tphit.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    tcl_tphit_st.h
C  Table of hits (space points) in tpc
#ifndef TCL_TPHIT_INC
#define TCL_TPHIT_INC
#endif
	STRUCTURE /TCL_TPHIT_ST/
	INTEGER*4 cluster !  id of a cluster used to reconstruct pnt. 0 
	INTEGER*4 flag !  evaluation of the hit quality 0 
	INTEGER*4 id !  a unique point id 0 
	INTEGER*4 id_globtrk !  id of reconstructed/sim global track 0 
	INTEGER*4 nseq !  number of sequences contributing to hit 0 
	INTEGER*4 row !  TPC row number 0 
	INTEGER*4 track !  id of a track to which the pnt was assgn 0 
	   REAL*4 alpha !  reconstructed crossing angle in xy 0 
	   REAL*4 dalpha !  error on the crossing angle 0 
	   REAL*4 dlambda !  error on the dip angle (degree) 0 
	   REAL*4 dq !  error on the charge 0 
	   REAL*4 dx !  error on the x coordinate 0 
	   REAL*4 dy !  error on the y coordinate 0 
	   REAL*4 dz !  error on the z coordinate 0 
	   REAL*4 lambda !  dip angle (degree) 0 
	   REAL*4 phi !  orientation of the hit w.r.t padplane 0 
	   REAL*4 prf !  value of the pad response (cm) 0 
	   REAL*4 q !  total charge assigned to this point 0 
	   REAL*4 x !  reconstructed x coordinate 0 
	   REAL*4 y !  reconstructed y coordinate 0 
	   REAL*4 z !  reconstructed z coordinate 0 
	   REAL*4 zrf !  value of the drift response (cm) 0 
	END STRUCTURE
C
      CHARACTER*(*) TCL_TPHIT_SPEC
      PARAMETER    (TCL_TPHIT_SPEC=
     + ' struct tcl_tphit {'                //
     + ' long cluster;'                     //
     + ' long flag;'                        //
     + ' long id;'                          //
     + ' long id_globtrk;'                  //
     + ' long nseq;'                        //
     + ' long row;'                         //
     + ' long track;'                       //
     + ' float alpha;'                      //
     + ' float dalpha;'                     //
     + ' float dlambda;'                    //
     + ' float dq;'                         //
     + ' float dx;'                         //
     + ' float dy;'                         //
     + ' float dz;'                         //
     + ' float lambda;'                     //
     + ' float phi;'                        //
     + ' float prf;'                        //
     + ' float q;'                          //
     + ' float x;'                          //
     + ' float y;'                          //
     + ' float z;'                          //
     + ' float zrf;'                        //
     + ' };')
C
