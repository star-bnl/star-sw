/***************************************************************************
 *
 * $Id: StEvent.cc,v 1.2 1999/01/15 22:53:39 wenaus Exp $
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
 * $Log: StEvent.cc,v $
 * Revision 1.2  1999/01/15 22:53:39  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.3  1999/01/21 20:51:41  wenaus
 * Fix for Sun compiler peculiarity
 * Revision 1.2  1999/01/15 22:53:39  wenaus
static const char rcsid[] = "$Id: StEvent.cc,v 1.2 1999/01/15 22:53:39 wenaus Exp $";
 *
#if !defined(ST_NO_NAMESPACES)
static const char rcsid[] = "$Id: StEvent.cc,v 1.2 1999/01/15 22:53:39 wenaus Exp $";
#endif

static const char rcsid[] = "$Id: StEvent.cc,v 1.2 1999/01/15 22:53:39 wenaus Exp $";
StEvent::StEvent(StRun* run, dst_event_header_st* hdr, dst_event_summary_st* sum) {
StEvent::StEvent()
    mType = hdr->event_type;
    mId.first = hdr->n_event[0];
    mId.second = hdr->n_event[1];
    mRunNumber = hdr->n_run;
    mTime = hdr->time;
    mTriggerMask = hdr->trig_mask;
    mBunchCrossingNumber = hdr->bunch_cross;
    mTriggerMask = hdr.trig_mask;
    mBunchCrossingNumber = hdr.bunch_cross;
    string dummyString;
    dummyString = "Ditto!";
    cout << dummyString << endl;
#endif
}

StEvent::StEvent(const StEvent&) { /* noop */} // private

const StEvent&
    delete mSummary;
    for(StTrackIterator it=mTracks->begin(); it != mTracks->end(); it++) delete *it;
    delete mTracks;            
    for(StVertexIterator iv=mVertices->begin(); iv != mVertices->end(); iv++) delete *iv;
    delete mVertices;
    delete mTpcHits;           
    delete mSvtHits;           
    delete mFtpcHits;          
    delete mTriggerDetectors;  
    delete mL0Trigger;         
    delete mEmcHits;         
    delete mSmdHits;         
    delete mTriggerDetectors; mTriggerDetectors=0;
    delete mL0Trigger; mL0Trigger=0;
    delete mEmcTowerHits; mEmcTowerHits=0;   // collection contains hits by value, this kills them all
    delete mEmcPreShowerHits; mEmcPreShowerHits=0;
    delete mSmdPhiHits; mSmdPhiHits=0;
    delete mSmdEtaHits; mSmdEtaHits=0;
}

void StEvent::init(StRun* run)
{
    mRun = run;
    mRunNumber = 0;           
    mTime = 0;                
    mTriggerMask = 0;         
    mBunchCrossingNumber = 0; 
    mLuminosity = 0;          
    mPrimaryVertex = 0;       
    mEmcHits = 0;            
    mSmdHits = 0;            
    mSvtHits = 0;             
    mFtpcHits = 0;            
    mEmcTowerHits = 0;            
    mEmcPreShowerHits = 0;            
    mSmdPhiHits = 0;            
    mSmdEtaHits = 0;            
    mTriggerDetectors = 0;    
    mL0Trigger = 0;
    for (int i=0; i<3; i++) {
	mBeamPolarizationEast[i] = 0;
	mBeamPolarizationWest[i] = 0;
    }
    // Create the collections
    mEmcHits = new StEmcHitCollection();
    mSmdHits = new StSmdHitCollection();
    mSummary = new StDstEventSummary();
    mTracks = new StTrackCollection();
    // with a given size.  tu
    //
    mEmcTowerHits = new StEmcTowerHitCollection();        
    mEmcPreShowerHits = new StEmcPreShowerHitCollection();
    mSmdPhiHits = new StSmdPhiHitCollection();
    mSmdEtaHits = new StSmdEtaHitCollection();
}

int StEvent::operator==(const StEvent& e) const
{
    return e.mId.first == mId.first &&
	   e.mId.second == mId.second; 
}

int StEvent::operator!=(const StEvent& e) const
{
    return !(e == *this);   // invoke operator==()
}

#if defined(__SUNPRO_CC)    
#else
    os << "Type: " << e.type() << endl;
#endif
    os << "Run: " << e.runNumber() << endl;
    time_t theTime = e.time();
    os << "Time: " << ctime(&theTime);  // \n provided by ctime()
    os << "Luminosity: " << e.luminosity() << endl;
    return os;
}

void StEvent::setType(const char* val) { mType = val; }

void StEvent::setId(const pair<long, long>& val)
{
    mId.first = val.first;
    mId.second = val.second;
}

void StEvent::setTime(time_t val) { mTime = val; }

void StEvent::setRunNumber(unsigned long val) { mRunNumber = val; }                

void StEvent::setTriggerMask(unsigned long val) { mTriggerMask = val; }              

void StEvent::setPrimaryVertex(StVertex* val) { mPrimaryVertex = val; }                  
void StEvent::setRun(StRun* val) { mRun = val; }                            

void StEvent::setPrimaryVertex(StVertex* val) {
  mPrimaryVertex = val;
  if (val) val->setType(primary);
}

void StEvent::setSummary(StDstEventSummary* val) { mSummary = val; }                        

void StEvent::setTrackCollection(StTrackCollection* val) { mTracks = val; }                

void StEvent::setTpcHitCollection(StTpcHitCollection* val) { mTpcHits = val; }               

void StEvent::setSvtHitCollection(StSvtHitCollection* val) { mSvtHits = val; }               

void StEvent::setFtpcHitCollection(StFtpcHitCollection* val) { mFtpcHits = val; }              

void StEvent::setEmcHitCollection(StEmcHitCollection* val) { mEmcHits = val; }               

void StEvent::setSmdHitCollection(StSmdHitCollection* val) { mSmdHits = val; }                

void StEvent::setEmcPreShowerHitCollection(StEmcPreShowerHitCollection* val) { mEmcPreShowerHits = val; }               

void StEvent::setSmdPhiHitCollection(StSmdPhiHitCollection* val) { mSmdPhiHits = val; }                

void StEvent::setSmdEtaHitCollection(StSmdEtaHitCollection* val) { mSmdEtaHits = val; }                

void StEvent::setBeamPolarization(StBeamDirection dir, StBeamPolarizationAxis axis, float val)
{
    if (dir == east)
	mBeamPolarizationEast[axis] = val;
    else
	mBeamPolarizationWest[axis] = val;
}
