/***********************************************************************
 *
 * v0MiniDstFiller.cc
 * $Id: v0MiniDstFiller.cc,v 1.1 1999/07/13 12:42:25 jones Exp $
 *
 * Description:
 * Fills V0 mini dst object as a ROOT Ordered Collection
 *
 * Author:
 * Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 * History:
 * $Log: v0MiniDstFiller.cc,v $
 * Revision 1.1  1999/07/13 12:42:25  jones
 * *** empty log message ***
 *
 ***********************************************************************/
#include "StEvent/StEvent.h"
#include "StEvent/StV0Vertex.h"
#include "StEvent/StV0Vertex.h"
#include "StV0MiniDst.hh"
#include <TOrdCollection.h>

void v0MiniDstFiller(StEvent& event, TOrdCollection* v0MiniDstCollection) {

  // Obtain the vertex collection from StEvent
  StVertexCollection* vertices = event.vertexCollection();
  StVertexIterator iter;
  StVertex *vertex = 0;
  StVertex *primaryVertex = 0;
  StV0Vertex *v0Vertex = 0;
  StV0MiniDst *v0m = 0;
  int n_v0 = 0;

  // First loop over vertices searches for primary vertex
  for( iter=vertices->begin(); iter!=vertices->end(); iter++) {
    vertex = *iter;
    if( vertex->type() == primary ) {
      primaryVertex = vertex;
      break;
    }
  }

  // Second loop over vertices builds linked list of v0 candidates
  for( iter=vertices->begin(); iter!=vertices->end(); iter++) {
    vertex = *iter;
    if( vertex->type() == V0 ) {
      v0Vertex = dynamic_cast<StV0Vertex*>(vertex);
      v0m = new StV0MiniDst(v0Vertex,primaryVertex);
      if( n_v0 == 0 ) 
	v0MiniDstCollection->AddFirst(v0m);
      else
	v0MiniDstCollection->AddAt(v0m,n_v0);
      n_v0++;
    }
  }

  printf("v0MiniDstFiller: found %d v0 candidates\n",n_v0);
}
