/* epi_global_pid.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
/* COMMENTS FROM IDL FILE:
   epi_global_pid.idl
   Table: epi_global_pid
        description: final PID assignment for global tracks  

 */
#ifndef EPI_GLOBAL_PID_H
#define EPI_GLOBAL_PID_H
#define EPI_GLOBAL_PID_SPEC \
"struct epi_global_pid { \
	long flag; \
	long id; \
	long method; \
	long most_prob_id; \
	long nhypo; \
	float quality; \
};"
typedef struct epi_global_pid_st {
	long flag; /* reports pid info that passes filters */
	long id; /* unique primary key */
	long method; /* reports epi module control settings */
	long most_prob_id; /* Geant PID code # for most probable mass */
	long nhypo; /* # rows in hypo_pid for this track */
	float quality; /* sharpness of final pid probability dist. */
} EPI_GLOBAL_PID_ST;
#endif /* EPI_GLOBAL_PID_H */
