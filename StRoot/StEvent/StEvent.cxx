/***************************************************************************
 *
 * $Id: StEvent.cxx,v 1.7 1999/05/04 20:59:23 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 14/01/1999 T. Wenaus  Add table-based constructor
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEvent.cxx,v $
 * Revision 1.7  1999/05/04 20:59:23  fisyak
 * move CVS Tag to StRun
 *
 * Revision 1.7  1999/05/04 20:59:23  fisyak
 * move CVS Tag to StRun
 *
 * Revision 1.6  1999/05/03 01:36:18  fisyak
 * Add Print
 *
 * Revision 1.5  1999/04/30 13:16:27  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:31  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.13  1999/03/23 21:58:03  ullrich
 * StGlobalTrack header file added.
 *
 * Revision 1.12  1999/03/09 22:31:18  wenaus
 * new Sun hack needed even though C++ 4.2 is supposed to be same as before
 *
 * Revision 1.11  1999/03/04 18:11:41  ullrich
 * Mods to cope with CC5
 *
 * Revision 1.10  1999/02/23 21:20:22  ullrich
 * Modified EMC hit collections.
 *
 * Revision 1.9  1999/02/22 20:48:52  wenaus
 * more delete cleanup
 *
 * Revision 1.8  1999/02/22 19:53:51  wenaus
 * cleaner deleting
 *
 * Revision 1.7  1999/02/10 23:26:53  wenaus
 * fix setPrimaryVertex in StEvent to set the vertex type to primary
 *
 * Revision 1.6  1999/02/10 21:50:27  wenaus
 * Plug memory leaks
 *
 * Revision 1.5  1999/02/10 18:58:23  wenaus
 * Print 'bad compiler' line only for bad compiler
 *
 * Revision 1.4  1999/01/30 23:03:10  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.3  1999/01/21 20:51:41  wenaus
 * Fix for Sun compiler peculiarity
 *
 * Revision 1.2  1999/01/15 22:53:39  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.11  2000/05/15 18:35:38  ullrich
#include <iostream.h>
 * All data member related to collections and containers are now
#include "StGlobalTrack.h"
#include "StThreeVectorF.hh"
#include "TString.h"
#include "TBrowser.h"
using namespace std;
static const Char_t rcsid[] = "$Id: StEvent.cxx,v 1.7 1999/05/04 20:59:23 fisyak Exp $";
 extern "C" {int isprint(int);}
StEvent::StEvent():St_DataSet("Event")
static const Char_t rcsid[] = "$Id: StEvent.cxx,v 1.7 1999/05/04 20:59:23 fisyak Exp $";
 * Changes due to the addition of the EMC to StEvent
StEvent::StEvent():St_DataSet("StEvent")
 * add rich pixel info/containers
    init();
St_DataSet("Event")
static const char rcsid[] = "$Id: StEvent.cxx,v 1.7 1999/05/04 20:59:23 fisyak Exp $";
StEvent::StEvent(StRun* run, dst_event_header_st& hdr, dst_event_summary_st& sum):
St_DataSet("StEvent")
#include "StEmcCollection.h"
    init(run);
    mType = hdr.event_type;
    mId.first = hdr.n_event[0];
    mId.second = hdr.n_event[1];
    mRunNumber = hdr.n_run;
    setTime(hdr.time);
    mTriggerMask = hdr.trig_mask;
    mBunchCrossingNumber = hdr.bunch_cross;
    StThreeVectorD v;
#ifdef ST_NO_TEMPLATE_DEF_ARGS
    cout << "Ignore this line; required to make Sun's lousy compiler work " << v << endl;
    TString dummyString;
    dummyString = "Ditto!";
    cout << dummyString << endl;
#endif
    mRunId = evtHdr.exp_run_id;
    mId    = evtHdr.n_event;
StEvent::StEvent(const StEvent&) { /* noop */} // private

const StEvent&
StEvent::operator=(const StEvent&) { return *this;} // private

    mTime  = evtHdr.time;
    mTriggerMask = evtHdr.trig_mask;
    SafeDelete(mTracks);
    SafeDelete(mVertices);
    // SafeDelete(mPrimaryVertex);   No, is deleted below in vertex collection
    SafeDelete(mSummary);
    //    SafeDelete(mPrimaryVertex);
    SafeDelete(mTriggerDetectors);
    SafeDelete(mL0Trigger);
    SafeDelete(mEmcTowerHits); // collection contains hits by value, this kills them all
    SafeDelete(mSvtHits);
    SafeDelete(mFtpcHits);
    SafeDelete(mEmcTowerHits); 
    SafeDelete(mTracks);
    SafeDelete(mVertices);
    SafeDelete(mL0Trigger);
}

    mRun = run;
{
    mType = "Unknown";
    //    mRun = run;
    mRunNumber = 0;           
    mTriggerMask = 0;         
    mBunchCrossingNumber = 0; 
    mTime.Set();
    mLuminosity = 0;          
    mPrimaryVertex = 0;       
    mSummary = 0;             
    mTracks = 0;              
    mVertices = 0;            
    mTpcHits = 0;             
    mSvtHits = 0;             
    mFtpcHits = 0;            
    mEmcTowerHits = 0;            
    mEmcPreShowerHits = 0;            
    mSmdPhiHits = 0;            
    mSmdEtaHits = 0;            
    mTriggerDetectors = 0;    
    mL0Trigger = 0;
    for (Int_t i=0; i<3; i++) {
    // Create the collections
    mSummary = new StDstEventSummary();
    mTracks = new StGlobalTrackCollection("Tracks");
    mVertices = new StVertexCollection("Vertices");
    mTpcHits = new StTpcHitCollection("TpcHits");
    mSvtHits = new StSvtHitCollection("SvtHits");
    mFtpcHits = new StFtpcHitCollection("FtpcHits");
    mTriggerDetectors = new StTriggerDetectorCollection();
    mL0Trigger = new StL0Trigger();

      mFtpcHits = new StFtpcHitCollection("FtpcHits");
      mTriggerDetectors = new StTriggerDetectorCollection();
      mL0Trigger = new StL0Trigger();
    //
    // Attention it would be more (CPU) efficient if we
    mEmcTowerHits = new StEmcTowerHitCollection("EmcTowerHits");        
    mEmcPreShowerHits = new StEmcPreShowerHitCollection("EmcPreShowerHits");
    mSmdPhiHits = new StSmdPhiHitCollection("SmdPhiHits");
    mSmdEtaHits = new StSmdEtaHitCollection("SmdEtaHits");
      mSmdPhiHits = new StSmdPhiHitCollection("SmdPhiHits");
      mSmdEtaHits = new StSmdEtaHitCollection("SmdEtaHits");
    }
StEvent::initToZero() { /* noop */ }
//______________________________________________________________________________
void StEvent::Browse(TBrowser *b)
     if (mRun)              b->Add(mRun);                
  // Browse this dataset (called by TBrowser).
   if (b) {
     //     if (mRun)              b->Add(mRun);                
     if (mPrimaryVertex)    b->Add(mPrimaryVertex);
     if (mSummary)          b->Add(mSummary);
     if (mTracks)           b->Add(mTracks);
     if (mVertices)         b->Add(mVertices);
     if (mTpcHits)          b->Add(mTpcHits);
     if (mSvtHits)          b->Add(mSvtHits);
     if (mFtpcHits)         b->Add(mFtpcHits);
     if (mTriggerDetectors) b->Add(mTriggerDetectors);
     if (mL0Trigger)        b->Add(mL0Trigger);
     if (mEmcTowerHits)     b->Add(mEmcTowerHits);
     if (mEmcPreShowerHits) b->Add(mEmcPreShowerHits);
     if (mSmdPhiHits)       b->Add(mSmdPhiHits);
     if (mSmdEtaHits)       b->Add(mSmdEtaHits);
   }
  St_DataSet::Browse(b);
}
//______________________________________________________________________________
void StEvent::Print(Option_t *opt)
{
  cout << endl;
  cout << *this << endl;
}
//______________________________________________________________________________

Int_t StEvent::operator==(const StEvent& e) const
{
    return e.mId.first == mId.first &&
	   e.mId.second == mId.second; 
}
    _lookup(info, mContent);
Int_t StEvent::operator!=(const StEvent& e) const
{
    return !(e == *this);   // invoke operator==()
}
    _lookup(summary, mContent);
    os << "Type: " << e.type().Data() << endl;
{
    os << "Id: "   << e.id().first << ", " << e.id().second << endl;
    os << "Type: " << e.type() << endl;
    os << "Run: "  << e.runNumber() << endl;
    os << "Time: " << e.time() << endl;
    os << "Luminosity: " << e.luminosity() << endl;
    return os;
}

void StEvent::setType(const Char_t* val) { mType = val; }

void StEvent::setId(const pairL& val)
{
    mId.first = val.first;
    mId.second = val.second;
}
    _lookup(hits, mContent);
    _lookup(hits, mContent);
void StEvent::setRunNumber(ULong_t val) { mRunNumber = val; }                
    _lookup(hits, mContent);
void StEvent::setTriggerMask(ULong_t val) { mTriggerMask = val; }              
    _lookup(hits, mContent);
void StEvent::setBunchCrossingNumber(ULong_t val) { mBunchCrossingNumber = val; }      
void StEvent::setRun(StRun* val) { mRun = val; }                            
void StEvent::setLuminosity(Double_t val) { mLuminosity = val; }               
    _lookup(emc, mContent);
//void StEvent::setRun(StRun* val) { mRun = val; }                            
StEvent::l3Trigger() const { return mL3Trigger; }
void StEvent::setPrimaryVertex(StVertex* val) {
  mPrimaryVertex = val;
  if (val) val->setType(primary);
StEvent::trackNodes() const { return mTrackNodes; }
    return *info;
void StEvent::setSummary(StDstEventSummary* val) { mSummary = val; }                        
        mContent[mTrackNodes] = new StSPtrVecTrackNode;
void StEvent::setTrackCollection(StGlobalTrackCollection* val) { mTracks = val; }                
    _lookupOrCreate(nodes, mContent);
void StEvent::setTpcHitCollection(StTpcHitCollection* val) { mTpcHits = val; }               

void StEvent::setSvtHitCollection(StSvtHitCollection* val) { mSvtHits = val; }               
    _lookupOrCreate(vertices, mContent);
void StEvent::setFtpcHitCollection(StFtpcHitCollection* val) { mFtpcHits = val; }              

void StEvent::setVertexCollection(StVertexCollection* val) { mVertices = val; }               
}
void StEvent::setTriggerDetectorCollection(StTriggerDetectorCollection* val) { mTriggerDetectors = val; }      
        return 0;
void StEvent::setL0Trigger(StL0Trigger* val) { mL0Trigger = val; }
    return *vertices;
void StEvent::setEmcTowerHitCollection(StEmcTowerHitCollection* val) { mEmcTowerHits = val; }               
    return *vertices;
void StEvent::setEmcPreShowerHitCollection(StEmcPreShowerHitCollection* val) { mEmcPreShowerHits = val; }               
StEvent::setType(const Char_t* val) { mType = val; }
void StEvent::setSmdPhiHitCollection(StSmdPhiHitCollection* val) { mSmdPhiHits = val; }                
    info->setId(val);
void StEvent::setSmdEtaHitCollection(StSmdEtaHitCollection* val) { mSmdEtaHits = val; }                
    static_cast<StEventInfo*>(mContent[mInfo])->setBunchCrossingNumber(val);
void StEvent::setBeamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis, Float_t val)
}
    if (dir == east)
	mBeamPolarizationEast[axis] = val;
 for (int i=0; t[i]; i++) {if (!isprint(t[i])) t[i]=0;}
 return  (const char*)mType; 
}                        
    if (mEmcCollection) delete mEmcCollection;
    mEmcCollection = val;
StEvent::setRichPixelCollection(StRichPixelCollection* val)
}
    if (mRichPixels) delete mRichPixels;
    mRichPixels = val;
    if (mContent[mEmcCollection]) delete mContent[mEmcCollection];
    if (mRichCollection) delete mRichCollection;
    mRichCollection = val;
{
    mPrimaryVertices.push_back(vertex);
            if (mPrimaryVertices[i]->numberOfDaughters() > mPrimaryVertices[i-1]->numberOfDaughters()) 
		swap(mPrimaryVertices[i], mPrimaryVertices[i-1]);
	    else
		break;
                swap(mPrimaryVertices[i], mPrimaryVertices[i-1]);
        if (!mContent[mPrimaryVertices])
            mContent[mPrimaryVertices] = new StSPtrVecPrimaryVertex;
    if (vertex) {
        for (int i=mPrimaryVertices->size()-1; i>0; i--) {
{StAutoBrowse::Browse(this,b);TDataSet::Browse(b);}
        //  Vertices are ordered according to number
        //  of daughter tracks in descending order.
        //
        for (int i=vertexVector->size()-1; i>0; i--) {
            if ((*vertexVector)[i]->numberOfDaughters() >
		(*vertexVector)[i-1]->numberOfDaughters())
                swap((*vertexVector)[i], (*vertexVector)[i-1]);
            else
                break;
        }
    }
}

void StEvent::Browse(TBrowser* b)
{
    StAutoBrowse::Browse(this,b);
    TDataSet::Browse(b);
}
