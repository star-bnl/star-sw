/* eg_event.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef EG_EVENT_H
#define EG_EVENT_H
#define EG_EVENT_SPEC \
"struct eg_event { \
	long n_event; \
	float b_impact; \
	float phi_impact; \
	long n_track; \
	long n_vertex; \
};"
typedef struct eg_event_st {
	long n_event; /*nrevt     eg event number                       */
	float b_impact; /*bimevt    actual impact parameter               */
	float phi_impact; /* long   n_part_neut_west;            number of participant neutrons        */
	long n_track; /*nptls     # tracks                              */
	long n_vertex; /* struct eg_vertex_t   *p_first_fs_vertex;      ptr to ll of fin. state vert*/
} EG_EVENT_ST;
#endif /* EG_EVENT_H */
