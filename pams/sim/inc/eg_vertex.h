/* eg_vertex.h */
/* This file was made by the idl compiler "stic". Do not edit.
** Instead, edit the source idl file and then re-run the compiler.
** For help, type contact Craig Tull or Herb Ward. */
#ifndef EG_VERTEX_H
#define EG_VERTEX_H
#define EG_VERTEX_SPEC \
"struct eg_vertex { \
	float x[3]; \
	float t; \
	long ivstor; \
	long iz; \
	long npstor; \
};"
typedef struct eg_vertex_st {
	float x[3]; /*xstrx,y,z  space point                             */
	float t; /*xstrt      time coordinate                         */
	long ivstor; /*ivstor     ?                        (not in gstar) */
	long iz; /*iz         ?                        (not in gstar) */
	long npstor; /* struct eg_vertex_t *p_prev_fs_vertex;      ptr to prev final state vertex */
} EG_VERTEX_ST;
#endif /* EG_VERTEX_H */
