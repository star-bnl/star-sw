/* tpt_track.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
   tpt_track.idl

   Table: tpt_track
   
        description: Reconstructed TPC tracks                


 */
#ifndef TPT_TRACK_H
#define TPT_TRACK_H
#define TPT_TRACK_SPEC \
"struct tpt_track { \
	long flag; \
	long hitid; \
	long id; \
	long id_globtrk; \
	long ndedx; \
	long nfit; \
	long nrec; \
	long q; \
	float chisq[2]; \
	float cov[15]; \
	float dedx[2]; \
	float invp; \
	float phi0; \
	float psi; \
	float r0; \
	float tanl; \
	float z0; \
};"
typedef struct tpt_track_st {
	long flag; /* status flag */
	long hitid; /* ID of the hit where parameters are given */
	long id; /* track id */
	long id_globtrk; /* Pointer to the globtrk table row. */
	long ndedx; /* Number of points used in dE/dx calc */
	long nfit; /* Number of points included in the fit */
	long nrec; /* Number of points assigned to that track */
	long q; /* charge */
	float chisq[2]; /* chi squared of the momentum fit */
	float cov[15]; /* covariance matrix(psi,invp,tanl,phi,z) */
	float dedx[2]; /* dE/dx information */
	float invp; /* 1/pt (transverse momentum) at (r,phi,z) */
	float phi0; /* azimuthal angle of the first point */
	float psi; /* azimuthal angle of the momentum at (r,.. */
	float r0; /* r (in cyl. coord.) for the first point */
	float tanl; /* tg of the dip angle at (r,phi,z) */
	float z0; /* z coordinate of the first point */
} TPT_TRACK_ST;
#endif /* TPT_TRACK_H */
