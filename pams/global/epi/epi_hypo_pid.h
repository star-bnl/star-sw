/* epi_hypo_pid.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
   epi_hypo_pid.idl
   Table: epi_hypo_pid
        description: Contains all mass hypotheses for each   

 */
#ifndef EPI_HYPO_PID_H
#define EPI_HYPO_PID_H
#define EPI_HYPO_PID_SPEC \
"struct epi_hypo_pid { \
	long det; \
	long gid; \
	long id; \
	long id_global_pid; \
	long method; \
	float nsigma; \
	float prob; \
	float weight; \
};"
typedef struct epi_hypo_pid_st {
	long det; /* detector:1-5 for svt,tpc,tof,emc,xtpc */
	long gid; /* Geant pid code # for this hypothesis */
	long id; /* identifier for rows corres. to globtrk */
	long id_global_pid; /* foreign key to global pid table */
	long method; /* report options in detec. specific pid */
	float nsigma; /* # std.dev. from mean;(+)higher;(-)lower */
	float prob; /* Probability for this mass hypothesis */
	float weight; /* calculated weight for this detector &trk */
} EPI_HYPO_PID_ST;
#endif /* EPI_HYPO_PID_H */
