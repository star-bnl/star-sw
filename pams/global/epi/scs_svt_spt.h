/* scs_svt_spt.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 scs_svt_spt.idl 
  Table: scs_svt_spt
  Parametric representation of recon     
  structed SVT space points.              

 */
#ifndef SCS_SVT_SPT_H
#define SCS_SVT_SPT_H
#define SCS_SVT_SPT_SPEC \
"struct scs_svt_spt { \
	long flag; \
	long id; \
	long id_cluster; \
	long id_globtrk; \
	long id_match; \
	long id_mctrack; \
	long id_track; \
	long id_wafer; \
	float cov[3]; \
	float de[2]; \
	float res[3]; \
	float x[3]; \
};"
typedef struct scs_svt_spt_st {
	long flag; /* Status of point. (Need to define bits) */
	long id; /* Identifier of reconstructed space point */
	long id_cluster; /* link to cluster point came from */
	long id_globtrk; /* key to global track table */
	long id_match; /* key to match table */
	long id_mctrack; /* id of montecarlo track */
	long id_track; /* Link to reconstructed track X is on. */
	long id_wafer; /* Identifier in svt_geom table of wafer */
	float cov[3]; /* Errors on X (diagonal for now) */
	float de[2]; /* Fitted or estimated dE in Si (+error) */
	float res[3]; /* residu of fit */
	float x[3]; /* Coordinates in STAR reference system */
} SCS_SVT_SPT_ST;
#endif /* SCS_SVT_SPT_H */
