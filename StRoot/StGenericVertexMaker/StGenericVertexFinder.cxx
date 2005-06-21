/***************************************************************************
 * $Id: StGenericVertexFinder.cxx,v 1.7 2005/06/21 02:16:36 balewski Exp $
 *
 * Author: Lee Barnby, April 2003
 *
 ***************************************************************************
 * Description: Base class for vertex finders
 *
 ***************************************************************************/
#include "StGenericVertexFinder.h"
#include "StMessMgr.h"
#include "StMaker.h"


StGenericVertexFinder::StGenericVertexFinder() : 
  mVertexConstrain(false), mMode(0){
}


/*!
  Adds the vertex to StEvent (currently as a primary)
  Here we invent our own flag and other data to put in
  In real life we have to get it from somewhere (as done for position)
*/
//======================================================
//======================================================
void 
StGenericVertexFinder::FillStEvent(StEvent* event) const{

  uint i;
  for(i=0;i<mVertexList.size(); i++) {
    //allocates new memory for each vertex
    StPrimaryVertex* primV = new StPrimaryVertex(mVertexList[i]); 
    event->addPrimaryVertex(primV);
    gMessMgr->Info() << "StGenericVertexFinder::FillStEvent: Added "<<i+1<<" primary vertex" << endm;
  }
}


void
StGenericVertexFinder::mClear(){
  printf(" StGenericVertexFinder::mClear()dddddddddddddddddddddddddddddd\n"); 
  mVertexList.clear();
}



void StGenericVertexFinder::NoVertexConstraint() 
{
  mVertexConstrain = false; 
  gMessMgr->Info() << "StGenericVertexFinder::No Vertex Constraint" << endm;
}


// $Log: StGenericVertexFinder.cxx,v $
// Revision 1.7  2005/06/21 02:16:36  balewski
// multiple prim vertices are stored in StEvent
//
// Revision 1.6  2004/12/13 20:39:58  fisyak
// Add initaition of StGenericVertexFinder variables, replace mDumMaker by StMaker::GetChain() method
//
// Revision 1.5  2004/07/30 22:59:00  calderon
// Setting the primary vertex flag to 1 for the moment, as per
// dst_vertex.idl.  This was causing the FTPC code to reject the
// primary vertex used as their seed.
//
// Revision 1.4  2004/07/24 02:57:40  balewski
// clean up of ppLMV, CTB-util separated
//
// Revision 1.3  2004/07/23 00:57:43  jeromel
// Base class method implementation
//
// Revision 1.2  2004/04/06 02:43:43  lbarnby
// Fixed identification of bad seeds (no z~0 problem now). Better flagging. Message manager used.
//
// Revision 1.1  2003/05/09 22:22:46  lbarnby
// Initial revision: a base class for STAR (StEvent-based) vertex finders
//
