/***************************************************************************
 * $Id: StGenericVertexFinder.cxx,v 1.17 2016/02/29 22:58:22 jwebb Exp $
 *
 * Author: Lee Barnby, April 2003
 *
 ***************************************************************************
 * Description: Base class for vertex finders
 *
 ***************************************************************************/
#include "StGenericVertexFinder.h"
#include "StEventTypes.h"
#include "StPrimaryVertex.h"
#include "StMessMgr.h"
#include "StMaker.h"
#include "StEventTypes.h"

//______________________________________________________________________________
StGenericVertexFinder::StGenericVertexFinder() : 
  mVertexConstrain(false), mMode(0), mDebugLevel(0)
{
  
  mIsMC	  =0;            	// flag minor differences between Data & M-C
  mUseBtof=0;           	// default use btof = false
  mUseCtb =0;            	// default use ctb = false
  mVertexOrderMethod = orderByNumberOfDaughters;
}
//______________________________________________________________________________
StGenericVertexFinder::~StGenericVertexFinder()
{
}

/*!
  Adds the vertex to StEvent (currently as a primary)
  Here we invent our own flag and other data to put in
  In real life we have to get it from somewhere (as done for position)
*/
//______________________________________________________________________________
void 
StGenericVertexFinder::FillStEvent(StEvent* event){

  for(UInt_t i=0;i<mVertexList.size(); i++) {
    //allocates new memory for each vertex
    StPrimaryVertex* primV = new StPrimaryVertex(mVertexList[i]); 
    event->addPrimaryVertex(primV,mVertexOrderMethod);
    LOG_INFO << "StGenericVertexFinder::FillStEvent: Added " <<i+1 
		     <<" primary vertex (" << mVertexOrderMethod << ")" << endm;
  }

  // Use StEvent's ordering
  // (might be undesirable for some debugging)
  // Also could be wrong if StEvent already has vertices for some reason
  mVertexList.clear();
  for(UInt_t i=0;i<event->numberOfPrimaryVertices(); i++)
    mVertexList.push_back(*(event->primaryVertex(i)));

}
//______________________________________________________________________________
void StGenericVertexFinder::addVertex(StPrimaryVertex* vtx)
{
  mVertexList.push_back(*vtx);
}
//______________________________________________________________________________
void StGenericVertexFinder::UsePCT(bool usePCT)
{
  LOG_WARN << "StGenericVertexFinder::UsePCT() not implemented for this vertex finder." << endm;
  LOG_WARN << "StGenericVertexFinder::Expect Post-crossing tracks to be used by default in old finders." << endm;
}
//_____________________________________________________________________________
int StGenericVertexFinder::size() const
{
  return mVertexList.size();
}
//______________________________________________________________________________
StPrimaryVertex* StGenericVertexFinder::getVertex(int idx) const
{
   return (idx<(int)mVertexList.size())? (StPrimaryVertex*)(&(mVertexList[idx])) : 0;
}
//______________________________________________________________________________
void
StGenericVertexFinder::Clear()
{
  mVertexList.clear();
}



//______________________________________________________________________________
void StGenericVertexFinder::NoVertexConstraint() 
{
  mVertexConstrain = false; 
  LOG_INFO << "StGenericVertexFinder::No Vertex Constraint" << endm;
}


// $Log: StGenericVertexFinder.cxx,v $
// Revision 1.17  2016/02/29 22:58:22  jwebb
// Moved include of StEventTypes from header of generic class to implementation files of generic and concrete classes.
//
// Revision 1.16  2013/08/16 20:49:38  perev
// PPV with only StEvent dependency
//
// Revision 1.15  2010/09/10 21:06:45  rjreed
// Added function UseBOTF and bool mUseBtof to switch the use of the TOF on and off in vertex finding.  Default value is off (false).
//
// Revision 1.14  2009/11/11 03:52:14  genevb
// Re-order the vertices upon filling StEvent
//
// Revision 1.13  2008/10/23 20:37:31  genevb
// Add switches for turning on/off use of Post-Crossing Tracks [default:off]
//
// Revision 1.12  2006/05/04 20:01:30  jeromel
// Switched to logger
//
// Revision 1.11  2006/04/26 15:37:03  jeromel
// mVertexOrderMethod (To be tested)
//
// Revision 1.10  2006/04/08 00:18:09  mvl
// Added member for debuglevel
//
// Revision 1.9  2005/07/19 21:45:07  perev
// MultiVertex
//
// Revision 1.8  2005/07/14 15:39:22  balewski
// nothing, to force recompilation of this code by Autobuild
//
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
