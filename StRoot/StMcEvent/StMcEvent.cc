/***************************************************************************
 *
 * $Id: StMcEvent.cc,v 2.1 1999/11/19 19:06:31 calderon Exp $
 * $Log: StMcEvent.cc,v $
 * Revision 2.1  1999/11/19 19:06:31  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:15  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:50  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.2  1999/07/28 20:27:32  calderon
 * Version with SL99f libraries
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcEvent.hh"
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcSvtHit.hh"
#include "tables/St_g2t_event_Table.h"

#include <string>
#include <utility>

TString StMcEvent::mCvsTag = "$Id: StMcEvent.cc,v 2.1 1999/11/19 19:06:31 calderon Exp $";
static const char rcsid[] = "$Id: StMcEvent.cc,v 2.1 1999/11/19 19:06:31 calderon Exp $";

void StMcEvent::initToZero()
{
    
    mPrimaryVertex = 0;       
    mTpcHits = 0;             
    mSvtHits = 0;             
    mFtpcHits = 0;            
    
    // Create the collections
    
    mTpcHits = new StMcTpcHitCollection();
    mSvtHits = new StMcSvtHitCollection();
    mFtpcHits = new StMcFtpcHitCollection();
    
}

StMcEvent::StMcEvent()
{   cout << "Inside StMcEvent Constructor" << endl;
    initToZero();
}

StMcEvent::StMcEvent(g2t_event_st* evTable) {
    
    mEventGeneratorEventLabel = evTable->eg_label;
    mEventNumber = evTable->n_event;
    mRunNumber   = evTable->n_run;
    mType  = evTable->event_type;
    mZWest = evTable->n_part_prot_west;
    mNWest = evTable->n_part_neut_west;
    mZEast = evTable->n_part_prot_east;
    mNEast = evTable->n_part_neut_east;
    mImpactParameter   = evTable->b_impact;
    mPhiReactionPlane  = evTable->phi_impact;
    mTriggerTimeOffset = evTable->time_offset;

    initToZero();

}



StMcEvent::StMcEvent(const StMcEvent&) { /* noop */} // private

const StMcEvent&
StMcEvent::operator=(const StMcEvent&) { return *this;} // private

StMcEvent::~StMcEvent()
{
    cout << "Inside StMcEvent Destructor" << endl; 
    if (mTpcHits) delete mTpcHits;
    mTpcHits=0;
    cout << "Deleted Tpc Hits" << endl;
    if (mSvtHits) delete mSvtHits;
    mSvtHits=0;
    cout << "Deleted Svt Hits" << endl;
    if (mFtpcHits) delete mFtpcHits;
    mFtpcHits=0;
    cout << "Deleted FTpc Hits" << endl;
    
    for(StPtrVecMcTrackIterator it=mTracks.begin();
	it != mTracks.end(); it++)
	delete *it;    
    cout << "Deleted Tracks" << endl;
    
    for(StPtrVecMcVertexIterator iv=mVertices.begin();
	iv != mVertices.end(); iv++)
	delete *iv;
    cout << "Deleted Vertices" << endl;
}

int StMcEvent::operator==(const StMcEvent& e) const
{
  return (e.mEventNumber == mEventNumber &&
	  e.mRunNumber   == mRunNumber &&
	  e.mType        == mType
	  );  
}

int StMcEvent::operator!=(const StMcEvent& e) const
{
    return !(e == *this);   // invoke operator==()
}

ostream&  operator<<(ostream& os, const StMcEvent& e)
{
    os << "Label : " << e.eventGeneratorEventLabel() << endl; 
    os << "Run   : " << e.runNumber() << endl;
    os << "Id    : " << e.eventNumber() << endl;
    os << "Type  : " << e.type() << endl;
    os << "Impact Parameter : " << e.impactParameter() << endl;
    os << "Phi Reaction Pl. : " << e.phiReactionPlane() << endl;
    os << "Trig. Time Offset: " << e.triggerTimeOffset() << endl;
    return os;
}


void StMcEvent::setEventGeneratorEventLabel(unsigned long val) { mEventGeneratorEventLabel = val; }

void StMcEvent::setEventNumber(unsigned long  val) { mEventNumber = val;  }

void StMcEvent::setRunNumber(unsigned long val) { mRunNumber = val; }                

void StMcEvent::setType(unsigned long val) { mType = val; }              

void StMcEvent::setZWest(unsigned long val) { mZWest = val; }              

void StMcEvent::setNWest(unsigned long val) { mNWest = val; }     

void StMcEvent::setZEast(unsigned long val) { mZEast = val; }              

void StMcEvent::setNEast(unsigned long val) { mNEast = val; }     

void StMcEvent::setImpactParameter(float val) { mImpactParameter = val; }               

void StMcEvent::setPhiReactionPlane(float val) { mPhiReactionPlane = val; }                            

void StMcEvent::setTriggerTimeOffset(float val) { mTriggerTimeOffset = val; }

void StMcEvent::setPrimaryVertex(StMcVertex* val) {  mPrimaryVertex = val; }

void StMcEvent::setTpcHitCollection(StMcTpcHitCollection* val)
{   
    if (mTpcHits && mTpcHits!= val) delete mTpcHits;
    mTpcHits = val;
}               

void StMcEvent::setSvtHitCollection(StMcSvtHitCollection* val)
{
    if (mSvtHits && mSvtHits!= val) delete mSvtHits;
    mSvtHits = val;
}               

void StMcEvent::setFtpcHitCollection(StMcFtpcHitCollection* val)
{
    if (mFtpcHits && mFtpcHits!= val) delete mFtpcHits;
    mFtpcHits = val;
}              
