/* eg_track.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef EG_TRACK_H
#define EG_TRACK_H
#define EG_TRACK_SPEC \
"struct eg_track { \
	long ge_pid; \
	float p[3]; \
	long itrack; \
	long ivertex; \
	long iz; \
};"
typedef struct eg_track_st {
	long ge_pid; /*idtype        GEANT id                     */
	float p[3]; /*pptl[3]       momentum                     */
	long itrack; /*it            Track Number  (not in gstar) */
	long ivertex; /*iv            Vertex Number (not in gstar) */
	long iz; /* struct eg_track_t  *p_prev_not_fs_track;     ptr to prev non-fs track     */
} EG_TRACK_ST;
#endif /* EG_TRACK_H */
