/* tte_tpeval.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 tte_tpeval.idl
     (onetasth.ace)
 

   Table: tte_tpeval
   Evaluation of the tracking package      
                                           

 */
#ifndef TTE_TPEVAL_H
#define TTE_TPEVAL_H
#define TTE_TPEVAL_SPEC \
"struct tte_tpeval { \
	long inhit; \
	long mtrk; \
	long nfit; \
	long ngood; \
	long nrec; \
	long qg; \
	long qr; \
	long rtrk; \
	long vid; \
	float abpullav[2]; \
	float chisq[2]; \
	float dpt; \
	float dpx; \
	float dpy; \
	float dpz; \
	float pid; \
	float prob[2]; \
	float ptg; \
	float ptr; \
	float ptvg; \
	float ptvr; \
	float pullav[2]; \
	float pullmx[2]; \
	float pxg; \
	float pxr; \
	float pxvg; \
	float pxvr; \
	float pyg; \
	float pyr; \
	float pyvg; \
	float pyvr; \
	float pzg; \
	float pzr; \
	float pzvg; \
	float pzvr; \
	float qfact; \
	float rdca; \
	float zdca; \
};"
typedef struct tte_tpeval_st {
	long inhit; /* hit id of innermost hit on rec. track */
	long mtrk; /* mc track # for he dominant contribution */
	long nfit; /* Number of points included in fit */
	long ngood; /* Number of points from the dominant track */
	long nrec; /* Number of reconstructed points */
	long qg; /* Generated charge */
	long qr; /* Reconstructed charge */
	long rtrk; /* Reconstructed track # */
	long vid; /* Vertex id */
	float abpullav[2]; /* Average of the absolute values of pulls */
	float chisq[2]; /* Chi**2 for the helix fit */
	float dpt; /* error on reconstructed pt */
	float dpx; /* error on reconstructed px */
	float dpy; /* error on reconstructed py */
	float dpz; /* error on reconstructed pz */
	float pid; /* Particle id */
	float prob[2]; /* Probability for a given chisq */
	float ptg; /* Transverse momentum at the first pnt(mc) */
	float ptr; /* Reconstructed pt at the first point */
	float ptvg; /* Transverse momentum at the vertex (mc). */
	float ptvr; /* Transverse momentum rec'd at the mc vtx */
	float pullav[2]; /* Average pull per track */
	float pullmx[2]; /* Maximum pull */
	float pxg; /* Generated px at the same point as pxr */
	float pxr; /* reconstructed px */
	float pxvg; /* Generated px at the vertex */
	float pxvr; /* Reconstructed px at the mc vertex */
	float pyg; /* Generated py at the same point as pyr */
	float pyr; /* Reconstructed py */
	float pyvg; /* Generated py at the vertex */
	float pyvr; /* Reconstructed py at the mc vertex */
	float pzg; /* Generated pz at the same point as pzr */
	float pzr; /* Reconstructed pz */
	float pzvg; /* Generated pz at the vertex */
	float pzvr; /* Reconstructed pz at the mc vertex */
	float qfact; /* % of points from the dominant mc track */
	float rdca; /* Dist. of closest approach vtx (radial) */
	float zdca; /* Dist. of closest approach vtx (drift) */
} TTE_TPEVAL_ST;
#endif /* TTE_TPEVAL_H */
