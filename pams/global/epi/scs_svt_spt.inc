C scs_svt_spt.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C  scs_svt_spt.idl 
C   Table: scs_svt_spt
C   Parametric representation of recon     
C   structed SVT space points.              
#ifndef SCS_SVT_SPT_INC
#define SCS_SVT_SPT_INC
#endif
	STRUCTURE /SCS_SVT_SPT_ST/
	INTEGER*4 flag !  Status of point. (Need to define bits) 0 
	INTEGER*4 id !  Identifier of reconstructed space point 0 
	INTEGER*4 id_cluster !  link to cluster point came from 0 
	INTEGER*4 id_globtrk !  key to global track table 0 
	INTEGER*4 id_match !  key to match table 0 
	INTEGER*4 id_mctrack !  id of montecarlo track 0 
	INTEGER*4 id_track !  Link to reconstructed track X is on. 0 
	INTEGER*4 id_wafer !  Identifier in svt_geom table of wafer 0 
	   REAL*4 cov(3) !  Errors on X (diagonal for now) 0 
	   REAL*4 de(2) !  Fitted or estimated dE in Si (+error) 0 
	   REAL*4 res(3) !  residu of fit 0 
	   REAL*4 x(3) !  Coordinates in STAR reference system 0 
	END STRUCTURE
C
      CHARACTER*(*) SCS_SVT_SPT_SPEC
      PARAMETER    (SCS_SVT_SPT_SPEC=
     + ' struct scs_svt_spt {'              //
     + ' long flag;'                        //
     + ' long id;'                          //
     + ' long id_cluster;'                  //
     + ' long id_globtrk;'                  //
     + ' long id_match;'                    //
     + ' long id_mctrack;'                  //
     + ' long id_track;'                    //
     + ' long id_wafer;'                    //
     + ' float cov[3];'                     //
     + ' float de[2];'                      //
     + ' float res[3];'                     //
     + ' float x[3];'                       //
     + ' };')
C
