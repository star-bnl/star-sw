/* epi_glob_pid_eval.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
   epi_glob_pid_eval.idl
   Table: epi_glob_pid_eval
        description: Evaluation data for each row in table   

 */
#ifndef EPI_GLOB_PID_EVAL_H
#define EPI_GLOB_PID_EVAL_H
#define EPI_GLOB_PID_EVAL_SPEC \
"struct epi_glob_pid_eval { \
	long id; \
	long mc_check; \
	long mc_pid; \
	long pid; \
	long svt_seg; \
	long tof_seg; \
	long tpc_seg; \
	float eta; \
	float invpt; \
};"
typedef struct epi_glob_pid_eval_st {
	long id; /* primary key = global_pid.id              */
	long mc_check; /* =1(0) if glb. trk. is(not) good MC track */
	long mc_pid; /* if mc_check=1 is the Geant pid code #    */
	long pid; /* Reconstr.PID #, copied from global_pid   */
	long svt_seg; /* indicates if global track has svt segmnt */
	long tof_seg; /* indicates if global track has TOF hit    */
	long tpc_seg; /* indicates if global track has tpc segmnt */
	float eta; /* pseudorapidity for global track          */
	float invpt; /* 1/pt copied from globtrk.invpt           */
} EPI_GLOB_PID_EVAL_ST;
#endif /* EPI_GLOB_PID_EVAL_H */
