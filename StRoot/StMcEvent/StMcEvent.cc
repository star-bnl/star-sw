/***************************************************************************
 *
 * $Id: StMcEvent.cc,v 2.8 2000/06/06 02:58:40 calderon Exp $
 * $Log: StMcEvent.cc,v $
 * Revision 2.8  2000/06/06 02:58:40  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.7  2000/05/11 14:27:23  calderon
 * use clear() in destructors to reduce size of containers
 *
 * Revision 2.6  2000/04/17 23:01:14  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.5  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:51  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
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
#include "StMcTpcHitCollection.hh"
#include "StMcFtpcHitCollection.hh"
#include "StMcRichHitCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcEmcHitCollection.hh"
#include "StMcContainers.hh" 
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcRichHit.hh"
#include "StMcSvtHit.hh"
#include "StMcCalorimeterHit.hh"
#include "tables/St_g2t_event_Table.h"



TString StMcEvent::mCvsTag = "$Id: StMcEvent.cc,v 2.8 2000/06/06 02:58:40 calderon Exp $";
static const char rcsid[] = "$Id: StMcEvent.cc,v 2.8 2000/06/06 02:58:40 calderon Exp $";

void StMcEvent::initToZero()
{
    
    mPrimaryVertex = 0;       
    mTpcHits = 0;             
    mSvtHits = 0;             
    mFtpcHits = 0;            
    mRichHits = 0;            
    
    // Create the collections
    
    mTpcHits  = new StMcTpcHitCollection();
    mSvtHits  = new StMcSvtHitCollection();
    mFtpcHits = new StMcFtpcHitCollection();
    mRichHits = new StMcRichHitCollection();

    mBemcHits  = new StMcEmcHitCollection();
    mBprsHits  = new StMcEmcHitCollection();
    mBsmdeHits = new StMcEmcHitCollection();
    mBsmdpHits = new StMcEmcHitCollection();
    
}

StMcEvent::StMcEvent()
    :mEventGeneratorEventLabel(0),
     mEventNumber(0),
     mRunNumber(0),
     mType(0),
     mZWest(0),
     mNWest(0),
     mZEast(0),
     mNEast(0),
     mPrimaryTracks(0),
     mImpactParameter(0),
     mPhiReactionPlane(0),
     mTriggerTimeOffset(0)
{
    initToZero();
}

StMcEvent::StMcEvent(g2t_event_st* evTable)
    :mEventGeneratorEventLabel(evTable->eg_label),
     mEventNumber(evTable->n_event),
     mRunNumber(evTable->n_run),
     mType (evTable->event_type),
     mZWest(evTable->n_part_prot_west),
     mNWest(evTable->n_part_neut_west),
     mZEast(evTable->n_part_prot_east),
     mNEast(evTable->n_part_neut_east),
     mPrimaryTracks(evTable->n_track_prim),
     mImpactParameter(evTable->b_impact),
     mPhiReactionPlane(evTable->phi_impact),
     mTriggerTimeOffset(evTable->time_offset)
     
{
    initToZero();
}


StMcEvent::StMcEvent(const StMcEvent&) { /* noop */} // private

const StMcEvent&
StMcEvent::operator=(const StMcEvent&) { return *this;} // private

StMcEvent::~StMcEvent()
{
    if (mTpcHits) delete mTpcHits;
    mTpcHits=0;

    if (mSvtHits) delete mSvtHits;
    mSvtHits=0;

    if (mFtpcHits) delete mFtpcHits;
    mFtpcHits=0;

    if (mRichHits) delete mRichHits;
    mRichHits=0;

    if (mBemcHits) delete mBemcHits;
    mBemcHits=0;

    if (mBprsHits) delete mBprsHits;
    mBprsHits=0;

    if (mBsmdeHits) delete mBsmdeHits;
    mBsmdeHits=0;

    if (mBsmdpHits) delete mBsmdpHits;
    mBsmdpHits=0;

    for(StMcTrackIterator it=mTracks.begin();
	it != mTracks.end(); it++)
	delete *it;    
    mTracks.clear();
    
    for(StMcVertexIterator iv=mVertices.begin();
	iv != mVertices.end(); iv++)
	delete *iv;
    mVertices.clear();
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
    os << "Participant Protons  East: " << e.zEast() << endl;
    os << "Participant Protons  West: " << e.zWest() << endl;
    os << "Participant Neutrons East: " << e.nEast() << endl;
    os << "Participant Neutrons West: " << e.nWest() << endl;
    os << "Number of Primary Tracks : " << e.numberOfPrimaryTracks() << endl;
    os << "Impact Parameter : " << e.impactParameter()   << endl;
    os << "Phi Reaction Pl. : " << e.phiReactionPlane()  << endl;
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

void StMcEvent::setNumberOfPrimaryTracks(unsigned long val){ mPrimaryTracks = val; } 

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

void StMcEvent::setRichHitCollection(StMcRichHitCollection* val)
{
    if (mRichHits && mRichHits!= val) delete mRichHits;
    mRichHits = val;
}              

void StMcEvent::setBemcHitCollection(StMcEmcHitCollection* val)
{
    if (mBemcHits && mBemcHits!= val) delete mBemcHits;
    mBemcHits = val;
}              

void StMcEvent::setBprsHitCollection(StMcEmcHitCollection* val)
{
    if (mBprsHits && mBprsHits!= val) delete mBprsHits;
    mBprsHits = val;
}              

void StMcEvent::setBsmdeHitCollection(StMcEmcHitCollection* val)
{
    if (mBsmdeHits && mBsmdeHits!= val) delete mBsmdeHits;
    mBsmdeHits = val;
}              

void StMcEvent::setBsmdpHitCollection(StMcEmcHitCollection* val)
{
    if (mBsmdpHits && mBsmdpHits!= val) delete mBsmdpHits;
    mBsmdpHits = val;
}              
