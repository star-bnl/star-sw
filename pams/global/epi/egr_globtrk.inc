C egr_globtrk.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C    egr_globtrk.idl
C    Table: egr_globtrk
#ifndef EGR_GLOBTRK_INC
#define EGR_GLOBTRK_INC
#endif
	STRUCTURE /EGR_GLOBTRK_ST/
	INTEGER*4 icharge !  charge of track 0 
	INTEGER*4 id !  global track primary key 0 
	INTEGER*4 id_emc !  f key to EMC table for track 0 
	INTEGER*4 id_global_pid !  foreign key to global_pid table 0 
	INTEGER*4 id_hypo_pid !  foreign key to hypo_pid table 0 
	INTEGER*4 id_part !  foreign key to particle table 0 
	INTEGER*4 id_pver !  f key to primary vertex for att. tracks 0 
	INTEGER*4 id_tof !  foreign key to ctf_cor table 0 
	INTEGER*4 last_row !  outermost TPC row included in track 0 
	INTEGER*4 ndegf !  no. deg. of freedom for track fit. 0 
	INTEGER*4 sflag !  Status of track reconstruction 0 
	   REAL*4 chisq !  global fit chi squared 0 
	   REAL*4 cov(15) !  Covariance for global track fit 0 
	   REAL*4 invpt !  1/pt at start of global track 0 
	   REAL*4 length !  Track length - first to last point 0 
	   REAL*4 phi0 !  phi angle at start of global track 0 
	   REAL*4 psi !  angle of p vector at start of gl. track 0 
	   REAL*4 r0 !  r coord at start of global track 0 
	   REAL*4 tanl !  tan(dip) at start of global track 0 
	   REAL*4 xlast(3) !  coordinates of outermost track point 0 
	   REAL*4 z0 !  z coord. at start of global track 0 
	END STRUCTURE
C
      CHARACTER*(*) EGR_GLOBTRK_SPEC
      PARAMETER    (EGR_GLOBTRK_SPEC=
     + ' struct egr_globtrk {'              //
     + ' long icharge;'                     //
     + ' long id;'                          //
     + ' long id_emc;'                      //
     + ' long id_global_pid;'               //
     + ' long id_hypo_pid;'                 //
     + ' long id_part;'                     //
     + ' long id_pver;'                     //
     + ' long id_tof;'                      //
     + ' long last_row;'                    //
     + ' long ndegf;'                       //
     + ' long sflag;'                       //
     + ' float chisq;'                      //
     + ' float cov[15];'                    //
     + ' float invpt;'                      //
     + ' float length;'                     //
     + ' float phi0;'                       //
     + ' float psi;'                        //
     + ' float r0;'                         //
     + ' float tanl;'                       //
     + ' float xlast[3];'                   //
     + ' float z0;'                         //
     + ' };')
C
