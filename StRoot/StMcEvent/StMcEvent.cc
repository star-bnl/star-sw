/***************************************************************************
 *
 * $Id: StMcEvent.cc,v 1.3 1999/09/23 21:25:50 calderon Exp $
 * $Log: StMcEvent.cc,v $
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
#include "StMcTrackCollection.hh"
#include "StMcFtpcHitCollection.hh"
#include "StMcVertexCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcTpcHitCollection.hh"

#include <string>
#include <utility>

// static const char rcsid[] = "$Id: StMcEvent.cc,v 1.3 1999/09/23 21:25:50 calderon Exp $";

StMcEvent::StMcEvent()
{   cout << "Inside StMcEvent Constructor" << endl;
    init();
}

StMcEvent::StMcEvent(g2t_event_st& evTable) {
    init();
    mEventGeneratorEventLabel = evTable.eg_label;
    mEventNumber = evTable.n_event; //Check if really need n_event[0] and n_event[1]
    mRunNumber = evTable.n_run;
    mZWest = evTable.n_part_prot_west;
    mNWest = evTable.n_part_neut_west;
    mZEast = evTable.n_part_prot_east;
    mNEast = evTable.n_part_neut_east;
    mImpactParameter = evTable.b_impact;
    mPhiReactionPlane = evTable.phi_impact;
    mTriggerTimeOffset = evTable.time_offset;

    StThreeVectorF v;
#ifdef ST_NO_TEMPLATE_DEF_ARGS
    cout << "Ignore this line; required to make Sun's lousy compiler work " << v << endl;
    string dummyString;
    dummyString = "Ditto!";
    cout << dummyString << endl;
#endif
}



StMcEvent::StMcEvent(const StMcEvent&) { /* noop */} // private

const StMcEvent&
StMcEvent::operator=(const StMcEvent&) { return *this;} // private

StMcEvent::~StMcEvent()
{
    cout << "Inside StMcEvent Destructor" << endl; 
    // Delete hits first
    if (mTpcHits) for(StMcTpcHitIterator iht=mTpcHits->begin(); iht != mTpcHits->end(); iht++) delete *iht;
    delete mTpcHits; mTpcHits=0;
    cout << "Deleted Tpc Hits" << endl;
    if (mSvtHits) for(StMcSvtHitIterator ihs=mSvtHits->begin(); ihs != mSvtHits->end(); ihs++) delete *ihs;
    delete mSvtHits; mSvtHits=0;
    cout << "Deleted Svt Hits" << endl;
    if (mFtpcHits) for(StMcFtpcHitIterator ihf=mFtpcHits->begin(); ihf != mFtpcHits->end(); ihf++) delete *ihf;
    delete mFtpcHits; mFtpcHits=0;
    cout << "Deleted FTpc Hits" << endl;
    // Then delete tracks
    if (mTracks) for(StMcTrackIterator it=mTracks->begin(); it != mTracks->end(); it++) delete *it;
    delete mTracks; mTracks=0;
    cout << "Deleted Tracks" << endl;
    // Finally, delete vertices
    if (mVertices) for(StMcVertexIterator iv=mVertices->begin(); iv != mVertices->end(); iv++) delete *iv;
    delete mVertices; mVertices=0;
    cout << "Deleted Vertices" << endl;
}

void StMcEvent::init()
{
    
    mPrimaryVertex = 0;       
    mVertices = 0;            
    mTracks = 0;  
    mTpcHits = 0;             
    mSvtHits = 0;             
    mFtpcHits = 0;            
    
    // Create the collections
    
    mTracks = new StMcTrackCollection();
    mVertices = new StMcVertexCollection();
    mTpcHits = new StMcTpcHitCollection();
    mSvtHits = new StMcSvtHitCollection();
    mFtpcHits = new StMcFtpcHitCollection();
    
}

int StMcEvent::operator==(const StMcEvent& e) const
{
  return e.mEventNumber == mEventNumber;  // check if we need to do it like e.mEventNumber.first ...
}

int StMcEvent::operator!=(const StMcEvent& e) const
{
    return !(e == *this);   // invoke operator==()
}

ostream&  operator<<(ostream& os, const StMcEvent& e)
{
    os << "Label: " << e.eventGeneratorEventLabel() << endl; 
    os << "Id: " << e.eventNumber() << endl;
    os << "Run: " << e.runNumber() << endl;
    
    return os;
}


void StMcEvent::setEventGeneratorEventLabel(unsigned long val) { mEventGeneratorEventLabel = val; }


void StMcEvent::setEventNumber(unsigned long  val)
{
  mEventNumber = val;  //check if we need to do it like mEventNumber.first ...  and const pair<long,long>& val
}


void StMcEvent::setRunNumber(unsigned long val) { mRunNumber = val; }                

void StMcEvent::setZWest(unsigned long val) { mZWest = val; }              

void StMcEvent::setNWest(unsigned long val) { mNWest = val; }     

void StMcEvent::setZEast(unsigned long val) { mZEast = val; }              

void StMcEvent::setNEast(unsigned long val) { mNEast = val; }     

void StMcEvent::setImpactParameter(float val) { mImpactParameter = val; }               

void StMcEvent::setPhiReactionPlane(float val) { mPhiReactionPlane = val; }                            

void StMcEvent::setTriggerTimeOffset(float val) { mTriggerTimeOffset = val; }

void StMcEvent::setPrimaryVertex(StMcVertex* val) {
  mPrimaryVertex = val;
  
}

void StMcEvent::setVertexCollection(StMcVertexCollection* val) { mVertices = val; }               

void StMcEvent::setTrackCollection(StMcTrackCollection* val) { mTracks = val; }                

void StMcEvent::setTpcHitCollection(StMcTpcHitCollection* val) { mTpcHits = val; }               

void StMcEvent::setSvtHitCollection(StMcSvtHitCollection* val) { mSvtHits = val; }               

void StMcEvent::setFtpcHitCollection(StMcFtpcHitCollection* val) { mFtpcHits = val; }              


