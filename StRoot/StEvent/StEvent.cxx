/***************************************************************************
 *
 * $Id: StEvent.cxx,v 1.2 1999/02/10 02:17:33 fisyak Exp $
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
 * Revision 1.2  1999/02/10 02:17:33  fisyak
 * Merging with new Torre stuff
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
 *
#include <iostream.h>
static const Char_t rcsid[] = "$Id: StEvent.cxx,v 1.2 1999/02/10 02:17:33 fisyak Exp $";
using namespace std;
#ifdef __ROOT__
ClassImp(StEvent)

#endif
StEvent::StEvent()
static const Char_t rcsid[] = "$Id: StEvent.cxx,v 1.2 1999/02/10 02:17:33 fisyak Exp $";
 * Changes due to the addition of the EMC to StEvent
StEvent::StEvent():St_DataSet("StEvent")
 * add rich pixel info/containers
StEvent::StEvent(StRun* run, dst_event_header_st& hdr, dst_event_summary_st& sum) {
StEvent::StEvent(StRun* run, dst_event_header_st& hdr, dst_event_summary_st& sum):
    mType = hdr.event_type;
#if 0
#include "StEmcCollection.h"
    init(run);
#endif
    mType = hdr.event_type;
    mTime = hdr.time;
    mId.second = hdr.n_event[1];
    mRunNumber = hdr.n_run;
    StThreeVectorF v;
    mBunchCrossingNumber = hdr.bunch_cross;
    dummyString = "Ditto!";
    cout << dummyString << endl;
#endif
    mRunId = evtHdr.exp_run_id;
    mId    = evtHdr.n_event;
StEvent::StEvent(const StEvent&) { /* noop */} // private

const StEvent&
StEvent::operator=(const StEvent&) { return *this;} // private
    // delete mRun;             Open question?
    // delete mPrimaryVertex;   No, is deleted below in vertex collection
    delete mSummary;
#if 0
    for(StTrackIterator it=mTracks->begin(); it != mTracks->end(); it++) delete *it;
#endif
    delete mTracks;            
#if 0
    for(StVertexIterator iv=mVertices->begin(); iv != mVertices->end(); iv++) delete *iv;
#endif
    delete mVertices;
    delete mTpcHits;           
    delete mSvtHits;           
    delete mFtpcHits;          
    delete mTriggerDetectors;  
    delete mL0Trigger;         
    delete mEmcHits;         
    delete mSmdHits;         
    SafeDelete(mTracks);
    SafeDelete(mVertices);
    SafeDelete(mL0Trigger);
}
    mRun = run;
{
    mTime = 0;                
    mType = "Unknown";
    //    mRun = run;
    mTriggerMask = 0;         
    mBunchCrossingNumber = 0; 
    mTime.Set();
    mLuminosity = 0;          
    mPrimaryVertex = 0;       
    mSummary = 0;             
    mTracks = 0;              
    mVertices = 0;            
    mEmcHits = 0;            
    mSmdHits = 0;            
    mEmcPreShowerHits = 0;            
    mSmdPhiHits = 0;            
    mSmdEtaHits = 0;            
    mTriggerDetectors = 0;    
    mL0Trigger = 0;
    for (Int_t i=0; i<3; i++) {
    // Create the collections
    mSummary = new StDstEventSummary();
    mTracks = new StTrackCollection();
    mVertices = new StVertexCollection();
    mTpcHits = new StTpcHitCollection();
    mSvtHits = new StSvtHitCollection();
    mFtpcHits = new StFtpcHitCollection();
    mEmcHits = new StEmcHitCollection();
    mSmdHits = new StSmdHitCollection();
    mTriggerDetectors = new StTriggerDetectorCollection();
    mL0Trigger = new StL0Trigger();
{
  cout << *this << endl;
}
//______________________________________________________________________________
#if 0

Int_t StEvent::operator==(const StEvent& e) const
#else
    return 1;
#endif
{
    return e.mId.first == mId.first &&
	   e.mId.second == mId.second; 
}
    _lookup(info, mContent);
Int_t StEvent::operator!=(const StEvent& e) const
{
    return !(e == *this);   // invoke operator==()
}
#if 0
    os << "Id: " << e.id().first << ", " << e.id().second << endl;
    os << "Type: " << e.type() << endl;
#endif
    os << "Run: " << e.runNumber() << endl;
    Long_t theTime = e.time();
    os << "Time: " << ctime(&theTime);  // \n provided by ctime()
    os << "Type: " << e.type() << endl;
    os << "Run: "  << e.runNumber() << endl;
    os << "Time: " << e.time() << endl;
    os << "Luminosity: " << e.luminosity() << endl;
#if 0
void StEvent::setType(const TString val) { mType = val; }
void StEvent::setId(const pair<Long_t, Long_t>& val)
void StEvent::setType(const Char_t* val) { mType = val; }

void StEvent::setId(const pairL& val)
{
#endif
void StEvent::setTime(Long_t val) { mTime = val; }
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
void StEvent::setPrimaryVertex(StVertex* val) { mPrimaryVertex = val; }                  
  mPrimaryVertex = val;
  if (val) val->setType(primary);
StEvent::trackNodes() const { return mTrackNodes; }
void StEvent::setTrackCollection(StTrackCollection* val) { mTracks = val; }                
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
void StEvent::setEmcHitCollection(StEmcHitCollection* val) { mEmcHits = val; }               
void StEvent::setEmcPreShowerHitCollection(StEmcPreShowerHitCollection* val) { mEmcPreShowerHits = val; }               
void StEvent::setSmdHitCollection(StSmdHitCollection* val) { mSmdHits = val; }                
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
