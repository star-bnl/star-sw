/* tcl_tphit.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
   tcl_tphit_st.h
 Table of hits (space points) in tpc

 */
#ifndef TCL_TPHIT_H
#define TCL_TPHIT_H
#define TCL_TPHIT_SPEC \
"struct tcl_tphit { \
	long cluster; \
	long flag; \
	long id; \
	long id_globtrk; \
	long nseq; \
	long row; \
	long track; \
	float alpha; \
	float dalpha; \
	float dlambda; \
	float dq; \
	float dx; \
	float dy; \
	float dz; \
	float lambda; \
	float phi; \
	float prf; \
	float q; \
	float x; \
	float y; \
	float z; \
	float zrf; \
};"
typedef struct tcl_tphit_st {
	long cluster; /* id of a cluster used to reconstruct pnt. */
	long flag; /* evaluation of the hit quality */
	long id; /* a unique point id */
	long id_globtrk; /* id of reconstructed/sim global track */
	long nseq; /* number of sequences contributing to hit */
	long row; /* TPC row number */
	long track; /* id of a track to which the pnt was assgn */
	float alpha; /* reconstructed crossing angle in xy */
	float dalpha; /* error on the crossing angle */
	float dlambda; /* error on the dip angle (degree) */
	float dq; /* error on the charge */
	float dx; /* error on the x coordinate */
	float dy; /* error on the y coordinate */
	float dz; /* error on the z coordinate */
	float lambda; /* dip angle (degree) */
	float phi; /* orientation of the hit w.r.t padplane */
	float prf; /* value of the pad response (cm) */
	float q; /* total charge assigned to this point */
	float x; /* reconstructed x coordinate */
	float y; /* reconstructed y coordinate */
	float z; /* reconstructed z coordinate */
	float zrf; /* value of the drift response (cm) */
} TCL_TPHIT_ST;
#endif /* TCL_TPHIT_H */
