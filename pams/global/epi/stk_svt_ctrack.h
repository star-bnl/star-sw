/* stk_svt_ctrack.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
 This file was made by the idl compiler "stic". Do not edit.
 Instead, edit the source idl file and then rerun the compiler.
 For help, type contact Craig Tull or Herb Ward. 
 COMMENTS FROM IDL FILE:
 stk_svt_ctrack.idl
     (onetasth.ace)
 

   Table: stk_svt_ctrack
   SVT candidate tracks                    
   essentially an intermediate table...    

 
 */
#ifndef STK_SVT_CTRACK_H
#define STK_SVT_CTRACK_H
#define STK_SVT_CTRACK_SPEC \
"struct stk_svt_ctrack { \
	long id; \
	long id_mctrack; \
	long nspt; \
	long pid; \
	long sec; \
	long spt[8]; \
};"
typedef struct stk_svt_ctrack_st {
	long id; /* candidate identification number */
	long id_mctrack; /* key to mkine table */
	long nspt; /* number of active spt in candidate */
	long pid; /* particle type (geant) */
	long sec; /* sec=0 unknown =1 primary =2 secondary */
	long spt[8]; /* space point indices in svt_spt table */
} STK_SVT_CTRACK_ST;
#endif /* STK_SVT_CTRACK_H */
