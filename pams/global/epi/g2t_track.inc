C g2t_track.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
#ifndef G2T_TRACK_INC
#define G2T_TRACK_INC
#endif
	STRUCTURE /G2T_TRACK_ST/
	INTEGER*4 eg_label !  generator track label (0 if GEANT track) 0 
	INTEGER*4 eg_pid !  event generator particle id 0 
	INTEGER*4 ge_pid !  GEANT particle id 0 
	INTEGER*4 hit_ctb_p !  Id of first ctb hit on track linked list 0 
	INTEGER*4 hit_eem_p !  Id of first eem hit on track linked list 0 
	INTEGER*4 hit_emc_p !  Id of first emc hit on track linked list 0 
	INTEGER*4 hit_esm_p !  Id of first esm hit on track linked list 0 
	INTEGER*4 hit_ftp_p !  Id of first ftp hit on track linked list 0 
	INTEGER*4 hit_mwc_p !  Id of first mwc hit on track linked list 0 
	INTEGER*4 hit_pgc_p !  Id of first pgc hit on track linked list 0 
	INTEGER*4 hit_psc_p !  Id of first psc hit on track linked list 0 
	INTEGER*4 hit_smd_p !  pointer to first SHM linked list hit 0 
	INTEGER*4 hit_svt_p !  Id of first svt hit on track linked list 0 
	INTEGER*4 hit_tof_p !  Id of first tof hit on track linked list 0 
	INTEGER*4 hit_tpc_p !  Id of first tpc hit on track linked list 0 
	INTEGER*4 hit_vpd_p !  Id of first vpd hit on track linked list 0 
	INTEGER*4 id !  primary key 0 
	INTEGER*4 is_shower !  1 if shower track, 0 if not 0 
	INTEGER*4 itrmd_vertex_p !  First intermediate vertex 0 
	INTEGER*4 n_ctb_hit !  Nhits in ctb 0 
	INTEGER*4 n_eem_hit !  Nhits in eem (endcap em cal) 0 
	INTEGER*4 n_emc_hit !  Nhits in emc 0 
	INTEGER*4 n_esm_hit !  Nhits in esm (endcap shower max) 0 
	INTEGER*4 n_ftp_hit !  Nhits in forward tpc 0 
	INTEGER*4 n_mwc_hit !  Nhits in mwc 0 
	INTEGER*4 n_pgc_hit !  Nhits in pgc (PMD) 0 
	INTEGER*4 n_psc_hit !  Nhits in psc (PMD) 0 
	INTEGER*4 n_smd_hit !  number of hits in shower max 0 
	INTEGER*4 n_svt_hit !  Nhits in svt 0 
	INTEGER*4 n_tof_hit !  Nhits in tof 0 
	INTEGER*4 n_tpc_hit !  Nhits in tpc 0 
	INTEGER*4 n_vpd_hit !  Nhits in vpd 0 
	INTEGER*4 next_parent_p !  Id of next parent track 0 
	INTEGER*4 next_vtx_trk_p !  Next daughter track of start vertex 0 
	INTEGER*4 start_vertex_p !  Id of start vertex of track 0 
	INTEGER*4 stop_vertex_p !  Id of stop vertex of this track 0 
	   REAL*4 charge !  Charge 0 
	   REAL*4 e !  Energy 0 
	   REAL*4 eta !  Pseudorapidity 0 
	   REAL*4 p(3) !  Momentum 0 
	   REAL*4 pt !  Transverse momentum 0 
	   REAL*4 ptot !  Total momentum 0 
	   REAL*4 rapidity !  Rapidity 0 
	END STRUCTURE
C
      CHARACTER*(*) G2T_TRACK_SPEC
      PARAMETER    (G2T_TRACK_SPEC=
     + ' struct g2t_track {'                //
     + ' long eg_label;'                    //
     + ' long eg_pid;'                      //
     + ' long ge_pid;'                      //
     + ' long hit_ctb_p;'                   //
     + ' long hit_eem_p;'                   //
     + ' long hit_emc_p;'                   //
     + ' long hit_esm_p;'                   //
     + ' long hit_ftp_p;'                   //
     + ' long hit_mwc_p;'                   //
     + ' long hit_pgc_p;'                   //
     + ' long hit_psc_p;'                   //
     + ' long hit_smd_p;'                   //
     + ' long hit_svt_p;'                   //
     + ' long hit_tof_p;'                   //
     + ' long hit_tpc_p;'                   //
     + ' long hit_vpd_p;'                   //
     + ' long id;'                          //
     + ' long is_shower;'                   //
     + ' long itrmd_vertex_p;'              //
     + ' long n_ctb_hit;'                   //
     + ' long n_eem_hit;'                   //
     + ' long n_emc_hit;'                   //
     + ' long n_esm_hit;'                   //
     + ' long n_ftp_hit;'                   //
     + ' long n_mwc_hit;'                   //
     + ' long n_pgc_hit;'                   //
     + ' long n_psc_hit;'                   //
     + ' long n_smd_hit;'                   //
     + ' long n_svt_hit;'                   //
     + ' long n_tof_hit;'                   //
     + ' long n_tpc_hit;'                   //
     + ' long n_vpd_hit;'                   //
     + ' long next_parent_p;'               //
     + ' long next_vtx_trk_p;'              //
     + ' long start_vertex_p;'              //
     + ' long stop_vertex_p;'               //
     + ' float charge;'                     //
     + ' float e;'                          //
     + ' float eta;'                        //
     + ' float p[3];'                       //
     + ' float pt;'                         //
     + ' float ptot;'                       //
     + ' float rapidity;'                   //
     + ' };')
C
