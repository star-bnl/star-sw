C tpt_track.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    tpt_track.idl
C    Table: tpt_track
C    
C         description: Reconstructed TPC tracks                
#ifndef TPT_TRACK_INC
#define TPT_TRACK_INC
#endif
	STRUCTURE /TPT_TRACK_ST/
	INTEGER*4 flag !  status flag 0 
	INTEGER*4 hitid !  ID of the hit where parameters are given 0 
	INTEGER*4 id !  track id 0 
	INTEGER*4 id_globtrk !  Pointer to the globtrk table row. 0 
	INTEGER*4 ndedx !  Number of points used in dE/dx calc 0 
	INTEGER*4 nfit !  Number of points included in the fit 0 
	INTEGER*4 nrec !  Number of points assigned to that track 0 
	INTEGER*4 q !  charge 0 
	   REAL*4 chisq(2) !  chi squared of the momentum fit 0 
	   REAL*4 cov(15) !  covariance matrix(psi,invp,tanl,phi,z) 0 
	   REAL*4 dedx(2) !  dE/dx information 0 
	   REAL*4 invp !  1/pt (transverse momentum) at (r,phi,z) 0 
	   REAL*4 phi0 !  azimuthal angle of the first point 0 
	   REAL*4 psi !  azimuthal angle of the momentum at (r,.. 0 
	   REAL*4 r0 !  r (in cyl. coord.) for the first point 0 
	   REAL*4 tanl !  tg of the dip angle at (r,phi,z) 0 
	   REAL*4 z0 !  z coordinate of the first point 0 
	END STRUCTURE
C
      CHARACTER*(*) TPT_TRACK_SPEC
      PARAMETER    (TPT_TRACK_SPEC=
     + ' struct tpt_track {'                //
     + ' long flag;'                        //
     + ' long hitid;'                       //
     + ' long id;'                          //
     + ' long id_globtrk;'                  //
     + ' long ndedx;'                       //
     + ' long nfit;'                        //
     + ' long nrec;'                        //
     + ' long q;'                           //
     + ' float chisq[2];'                   //
     + ' float cov[15];'                    //
     + ' float dedx[2];'                    //
     + ' float invp;'                       //
     + ' float phi0;'                       //
     + ' float psi;'                        //
     + ' float r0;'                         //
     + ' float tanl;'                       //
     + ' float z0;'                         //
     + ' };')
C
