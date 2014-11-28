#ifndef _DELAUNAY_H
#define _DELAUNAY_H

/* 
 * This header specifies the interface for the use of Delaunay graphs. 
 */
 
#include <inttypes.h>

typedef uintptr_t edge_ref;
#define MASK ~3

typedef struct {
    edge_ref next[4];
    void *data[4];
    unsigned mark;
} edge_struct;

edge_ref delaunay_build(int sites);
edge_ref *delaunay_edges(int n);
void destroy_edge(edge_ref e);

#define ROT(e) (((e)&MASK)+(((e)+1)&3))
#define SYM(e) (((e)&MASK)+(((e)+2)&3))
#define TOR(e) (((e)&MASK)+(((e)+3)&3))

#define ONEXT(e) ((edge_struct *)((e)&MASK))->next[(e)&3]
#define ROTRNEXT(e) ((edge_struct *)((e)&MASK))->next[((e)+1)&3]
#define SYMDNEXT(e) ((edge_struct *)((e)&MASK))->next[((e)+2)&3]
#define TORLNEXT(e) ((edge_struct *)((e)&MASK))->next[((e)+3)&3]

#define RNEXT(e) (TOR(ROTRNEXT(e)))
#define DNEXT(e) (SYM(SYMDNEXT(e)))
#define LNEXT(e) (ROT(TORLNEXT(e)))

#define OPREV(e) (ROT(ROTRNEXT(e)))
#define DPREV(e) (TOR(TORLNEXT(e)))
#define RPREV(e) (SYMDNEXT(e))
#define LPREV(e) (SYM(ONEXT(e)))

#define ODATA(e) ((edge_struct *)((e)&MASK))->data[(e)&3]
#define RDATA(e) ((edge_struct *)((e)&MASK))->data[((e)+1)&3]
#define DDATA(e) ((edge_struct *)((e)&MASK))->data[((e)+2)&3]
#define LDATA(e) ((edge_struct *)((e)&MASK))->data[((e)+3)&3]

#define MARK(e)  ((edge_struct *)((e)&MASK))->mark

#endif
