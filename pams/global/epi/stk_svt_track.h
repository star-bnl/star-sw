/* stk_svt_track.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 This file was made by the idl compiler "stic". Do not edit.
 Instead, edit the source idl file and then rerun the compiler.
 For help, type contact Craig Tull or Herb Ward. 
 COMMENTS FROM IDL FILE:
 stk_svt_track.idl
     (onetasth.ace)
 

   Table: stk_svt_track
   Tracks reconstructed in the SVT         
                                           

 
 */
#ifndef STK_SVT_TRACK_H
#define STK_SVT_TRACK_H
#define STK_SVT_TRACK_SPEC \
"struct stk_svt_track { \
	long flag; \
	long id; \
	long id_globtrk; \
	long id_match; \
	float chisq[2]; \
	float cov[15]; \
	float cres[10]; \
	float dedx[2]; \
	float impact; \
	float invpt; \
	float lres[10]; \
	float phi0; \
	float pid; \
	float psi; \
	float r0; \
	float tanl; \
	float z0; \
};"
typedef struct stk_svt_track_st {
	long flag; /* status flag */
	long id; /* track unique id number */
	long id_globtrk; /* key to global track table */
	long id_match; /* key to match table */
	float chisq[2]; /* chi squares of circle/line fit */
	float cov[15]; /* covariant matrix */
	float cres[10]; /* residue in circle fit */
	float dedx[2]; /* dE/dx information with error */
	float impact; /* impact parameter */
	float invpt; /* (charge)/momentum at reference point */
	float lres[10]; /* residue from linear fit (phi-z) */
	float phi0; /* aximuthal angle of first point */
	float pid; /* Particle ID in SVT alone */
	float psi; /* azimuthal angle of momentum at r,phi,z */
	float r0; /* reference point in cyl.coord. of 1 point */
	float tanl; /* tg of dip angle at r,phi,z */
	float z0; /* z coordinate of first point */
} STK_SVT_TRACK_ST;
#endif /* STK_SVT_TRACK_H */
