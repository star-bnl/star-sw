/* g2t_track.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef G2T_TRACK_H
#define G2T_TRACK_H
#define G2T_TRACK_SPEC \
"struct g2t_track { \
	long eg_label; \
	long eg_pid; \
	long ge_pid; \
	long hit_ctb_p; \
	long hit_eem_p; \
	long hit_emc_p; \
	long hit_esm_p; \
	long hit_ftp_p; \
	long hit_mwc_p; \
	long hit_pgc_p; \
	long hit_psc_p; \
	long hit_smd_p; \
	long hit_svt_p; \
	long hit_tof_p; \
	long hit_tpc_p; \
	long hit_vpd_p; \
	long id; \
	long is_shower; \
	long itrmd_vertex_p; \
	long n_ctb_hit; \
	long n_eem_hit; \
	long n_emc_hit; \
	long n_esm_hit; \
	long n_ftp_hit; \
	long n_mwc_hit; \
	long n_pgc_hit; \
	long n_psc_hit; \
	long n_smd_hit; \
	long n_svt_hit; \
	long n_tof_hit; \
	long n_tpc_hit; \
	long n_vpd_hit; \
	long next_parent_p; \
	long next_vtx_trk_p; \
	long start_vertex_p; \
	long stop_vertex_p; \
	float charge; \
	float e; \
	float eta; \
	float p[3]; \
	float pt; \
	float ptot; \
	float rapidity; \
};"
typedef struct g2t_track_st {
	long eg_label; /* generator track label (0 if GEANT track) */
	long eg_pid; /* event generator particle id */
	long ge_pid; /* GEANT particle id */
	long hit_ctb_p; /* Id of first ctb hit on track linked list */
	long hit_eem_p; /* Id of first eem hit on track linked list */
	long hit_emc_p; /* Id of first emc hit on track linked list */
	long hit_esm_p; /* Id of first esm hit on track linked list */
	long hit_ftp_p; /* Id of first ftp hit on track linked list */
	long hit_mwc_p; /* Id of first mwc hit on track linked list */
	long hit_pgc_p; /* Id of first pgc hit on track linked list */
	long hit_psc_p; /* Id of first psc hit on track linked list */
	long hit_smd_p; /* pointer to first SHM linked list hit */
	long hit_svt_p; /* Id of first svt hit on track linked list */
	long hit_tof_p; /* Id of first tof hit on track linked list */
	long hit_tpc_p; /* Id of first tpc hit on track linked list */
	long hit_vpd_p; /* Id of first vpd hit on track linked list */
	long id; /* primary key */
	long is_shower; /* 1 if shower track, 0 if not */
	long itrmd_vertex_p; /* First intermediate vertex */
	long n_ctb_hit; /* Nhits in ctb */
	long n_eem_hit; /* Nhits in eem (endcap em cal) */
	long n_emc_hit; /* Nhits in emc */
	long n_esm_hit; /* Nhits in esm (endcap shower max) */
	long n_ftp_hit; /* Nhits in forward tpc */
	long n_mwc_hit; /* Nhits in mwc */
	long n_pgc_hit; /* Nhits in pgc (PMD) */
	long n_psc_hit; /* Nhits in psc (PMD) */
	long n_smd_hit; /* number of hits in shower max */
	long n_svt_hit; /* Nhits in svt */
	long n_tof_hit; /* Nhits in tof */
	long n_tpc_hit; /* Nhits in tpc */
	long n_vpd_hit; /* Nhits in vpd */
	long next_parent_p; /* Id of next parent track */
	long next_vtx_trk_p; /* Next daughter track of start vertex */
	long start_vertex_p; /* Id of start vertex of track */
	long stop_vertex_p; /* Id of stop vertex of this track */
	float charge; /* Charge */
	float e; /* Energy */
	float eta; /* Pseudorapidity */
	float p[3]; /* Momentum */
	float pt; /* Transverse momentum */
	float ptot; /* Total momentum */
	float rapidity; /* Rapidity */
} G2T_TRACK_ST;
#endif /* G2T_TRACK_H */
