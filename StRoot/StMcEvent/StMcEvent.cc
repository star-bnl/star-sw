/***************************************************************************
 *
 * $Id: StMcEvent.cc,v 2.14 2003/12/04 05:56:47 calderon Exp $
 * $Log: StMcEvent.cc,v $
 * Revision 2.14  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.13  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.12  2003/05/15 18:28:47  calderon
 * Added data members from modified g2t_event table:
 * Event Generator Final State Tracks, N Binary Collisions,
 * N Wounded Nucleons East and West, N Jets.
 *
 * Revision 2.11  2003/04/17 18:01:24  calderon
 * print the subprocess id when dumping event info in operator<<
 *
 * Revision 2.10  2003/03/18 22:37:39  calderon
 * Added member mSubProcessId which is used for Pythia events.
 * Only is set from constructor from g2t_event table.
 *
 * Revision 2.9  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
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
#include "StMcCtbHitCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcEmcHitCollection.hh"
#include "StMcTofHitCollection.hh"
#include "StMcPixelHitCollection.hh"
#include "StMcContainers.hh" 
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcRichHit.hh"
#include "StMcSvtHit.hh"
#include "StMcCalorimeterHit.hh"
#include "StMcTofHit.hh"
#include "StMcPixelHit.hh"
#include "tables/St_g2t_event_Table.h"



TString StMcEvent::mCvsTag = "$Id: StMcEvent.cc,v 2.14 2003/12/04 05:56:47 calderon Exp $";
static const char rcsid[] = "$Id: StMcEvent.cc,v 2.14 2003/12/04 05:56:47 calderon Exp $";

void StMcEvent::initToZero()
{
    
    mPrimaryVertex = 0;       
    mTpcHits = 0;             
    mSvtHits = 0;             
    mFtpcHits = 0;            
    mRichHits = 0;            
    mCtbHits = 0;
    mTofHits = 0;
    mEemcHits = 0;
    mPixelHits = 0;

    // Create the collections
    
    mTpcHits  = new StMcTpcHitCollection();
    mSvtHits  = new StMcSvtHitCollection();
    mFtpcHits = new StMcFtpcHitCollection();
    mRichHits = new StMcRichHitCollection();
    mCtbHits = new StMcCtbHitCollection();

    mBemcHits  = new StMcEmcHitCollection();
    mBprsHits  = new StMcEmcHitCollection();
    mBsmdeHits = new StMcEmcHitCollection();
    mBsmdpHits = new StMcEmcHitCollection();
    mTofHits = new StMcTofHitCollection();
    mEemcHits = new StMcEmcHitCollection();
    mPixelHits = new StMcPixelHitCollection();


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
     mSubProcessId(0),
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
     mEvGenFSTracks(evTable->n_track_eg_fs),
     mPrimaryTracks(evTable->n_track_prim),
     mSubProcessId(evTable->subprocess_id),
     mImpactParameter(evTable->b_impact),
     mPhiReactionPlane(evTable->phi_impact),
     mTriggerTimeOffset(evTable->time_offset),
     mNBinary(evTable->n_binary),
     mNWoundedEast(evTable->n_wounded_east),
     mNWoundedWest(evTable->n_wounded_west),
     mNJets(evTable->njets)
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

    if (mCtbHits) delete mCtbHits;
    mCtbHits=0;
    
    if (mBemcHits) delete mBemcHits;
    mBemcHits=0;

    if (mBprsHits) delete mBprsHits;
    mBprsHits=0;

    if (mBsmdeHits) delete mBsmdeHits;
    mBsmdeHits=0;

    if (mBsmdpHits) delete mBsmdpHits;
    mBsmdpHits=0;

    if (mTofHits) delete mTofHits;
    mTofHits=0;

    if (mEemcHits) delete mEemcHits;
    mEemcHits=0;

    if (mPixelHits) delete mPixelHits;
    mPixelHits=0;

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
    os << "# Ev. Gen. Fin. St. Track: " << e.eventGeneratorFinalStateTracks() << endl;
    os << "Number of Primary Tracks : " << e.numberOfPrimaryTracks() << endl;
    os << "Subprocess Id    : " << e.subProcessId() << endl;
    os << "Impact Parameter : " << e.impactParameter()   << endl;
    os << "Phi Reaction Pl. : " << e.phiReactionPlane()  << endl;
    os << "Trig. Time Offset: " << e.triggerTimeOffset() << endl;
    os << "N Binary Coll.   : " << e.nBinary() << endl;
    os << "N Wounded East   : " << e.nWoundedEast() << endl;
    os << "N Wounded West   : " << e.nWoundedWest() << endl;
    os << "N Jets           : " << e.nJets() << endl;
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

void StMcEvent::setEventGeneratorFinalStateTracks(unsigned long val){ mEvGenFSTracks = val; } 

void StMcEvent::setNumberOfPrimaryTracks(unsigned long val){ mPrimaryTracks = val; } 

void StMcEvent::setImpactParameter(float val) { mImpactParameter = val; }               

void StMcEvent::setPhiReactionPlane(float val) { mPhiReactionPlane = val; } 

void StMcEvent::setTriggerTimeOffset(float val) { mTriggerTimeOffset = val; }

void StMcEvent::setNBinary(unsigned long val) { mNBinary = val; }

void StMcEvent::setNWoundedEast(unsigned long val) { mNWoundedEast = val; }

void StMcEvent::setNWoundedWest(unsigned long val) { mNWoundedWest = val; }

void StMcEvent::setNJets(unsigned long val) { mNJets = val; }

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

void StMcEvent::setTofHitCollection(StMcTofHitCollection* val)
{
    if (mTofHits && mTofHits!= val) delete mTofHits;
    mTofHits = val;
}

void StMcEvent::setEemcHitCollection(StMcEmcHitCollection* val)
{
    if (mEemcHits && mEemcHits!= val) delete mEemcHits;
    mEemcHits = val;
}

void StMcEvent::setPixelHitCollection(StMcPixelHitCollection* val)
{
    if (mPixelHits && mPixelHits!= val) delete mPixelHits;
    mPixelHits = val;
}   
