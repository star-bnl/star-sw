C stk_svt_track.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C  This file was made by the idl compiler "stic". Do not edit.
C  Instead, edit the source idl file and then rerun the compiler.
C  For help, type contact Craig Tull or Herb Ward. 
C  COMMENTS FROM IDL FILE:
C  stk_svt_track.idl
C      (onetasth.ace)
C  
C    Table: stk_svt_track
C    Tracks reconstructed in the SVT         
C                                            
C  
#ifndef STK_SVT_TRACK_INC
#define STK_SVT_TRACK_INC
#endif
	STRUCTURE /STK_SVT_TRACK_ST/
	INTEGER*4 flag !  status flag 0 
	INTEGER*4 id !  track unique id number 0 
	INTEGER*4 id_globtrk !  key to global track table 0 
	INTEGER*4 id_match !  key to match table 0 
	   REAL*4 chisq(2) !  chi squares of circle/line fit 0 
	   REAL*4 cov(15) !  covariant matrix 0 
	   REAL*4 cres(10) !  residue in circle fit 0 
	   REAL*4 dedx(2) !  dE/dx information with error 0 
	   REAL*4 impact !  impact parameter 0 
	   REAL*4 invpt !  (charge)/momentum at reference point 0 
	   REAL*4 lres(10) !  residue from linear fit (phi-z) 0 
	   REAL*4 phi0 !  aximuthal angle of first point 0 
	   REAL*4 pid !  Particle ID in SVT alone 0 
	   REAL*4 psi !  azimuthal angle of momentum at r,phi,z 0 
	   REAL*4 r0 !  reference point in cyl.coord. of 1 point 0 
	   REAL*4 tanl !  tg of dip angle at r,phi,z 0 
	   REAL*4 z0 !  z coordinate of first point 0 
	END STRUCTURE
C
      CHARACTER*(*) STK_SVT_TRACK_SPEC
      PARAMETER    (STK_SVT_TRACK_SPEC=
     + ' struct stk_svt_track {'            //
     + ' long flag;'                        //
     + ' long id;'                          //
     + ' long id_globtrk;'                  //
     + ' long id_match;'                    //
     + ' float chisq[2];'                   //
     + ' float cov[15];'                    //
     + ' float cres[10];'                   //
     + ' float dedx[2];'                    //
     + ' float impact;'                     //
     + ' float invpt;'                      //
     + ' float lres[10];'                   //
     + ' float phi0;'                       //
     + ' float pid;'                        //
     + ' float psi;'                        //
     + ' float r0;'                         //
     + ' float tanl;'                       //
     + ' float z0;'                         //
     + ' };')
C
