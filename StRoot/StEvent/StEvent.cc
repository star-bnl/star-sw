/***************************************************************************
 *
 * $Id: StEvent.cc,v 1.11 1999/03/04 18:11:41 ullrich Exp $
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
 * Revision 1.11  1999/03/04 18:11:41  ullrich
 * Mods to cope with CC5
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
#if !defined(ST_NO_NAMESPACES)
static const char rcsid[] = "$Id: StEvent.cc,v 1.11 1999/03/04 18:11:41 ullrich Exp $";
#endif

static const char rcsid[] = "$Id: StEvent.cc,v 1.11 1999/03/04 18:11:41 ullrich Exp $";

StEvent::StEvent()
{
    init();
}

StEvent::StEvent(StRun* run, dst_event_header_st& hdr, dst_event_summary_st& sum) {
    init(run);
    mType = hdr.event_type;
    mId.first = hdr.n_event[0];
    mId.second = hdr.n_event[1];
    mRunNumber = hdr.n_run;
    mTime = hdr.time;
    mTriggerMask = hdr.trig_mask;
    mBunchCrossingNumber = hdr.bunch_cross;
    string dummyString;
    dummyString = "Ditto!";
    cout << dummyString << endl;
#endif
}

StEvent::StEvent(const StEvent&) { /* noop */} // private

const StEvent&
StEvent::operator=(const StEvent&) { return *this;} // private

StEvent::~StEvent()
{
    // delete mRun;             Open question?
    // delete mPrimaryVertex;   No, is deleted below in vertex collection
    delete mSummary; mSummary=0;
    if (mTracks) for(StTrackIterator it=mTracks->begin(); it != mTracks->end(); it++) delete *it;
    delete mTracks; mTracks=0;
    if (mVertices) for(StVertexIterator iv=mVertices->begin(); iv != mVertices->end(); iv++) delete *iv;
    delete mVertices; mVertices=0;
    if (mTpcHits) for(StTpcHitIterator iht=mTpcHits->begin(); iht != mTpcHits->end(); iht++) delete *iht;
    delete mTpcHits; mTpcHits=0;
    if (mSvtHits) for(StSvtHitIterator ihs=mSvtHits->begin(); ihs != mSvtHits->end(); ihs++) delete *ihs;
    delete mSvtHits; mSvtHits=0;
    if (mFtpcHits) for(StFtpcHitIterator ihf=mFtpcHits->begin(); ihf != mFtpcHits->end(); ihf++) delete *ihf;
    delete mFtpcHits; mFtpcHits=0;
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
    for (int i=0; i<3; i++) {
	mBeamPolarizationEast[i] = 0;
	mBeamPolarizationWest[i] = 0;
    }
    // Create the collections
    mSummary = new StDstEventSummary();
    mTracks = new StTrackCollection();
    mVertices = new StVertexCollection();
    mTpcHits = new StTpcHitCollection();
    mSvtHits = new StSvtHitCollection();
    mFtpcHits = new StFtpcHitCollection();
    mTriggerDetectors = new StTriggerDetectorCollection();
    mL0Trigger = new StL0Trigger();

    //
    // Attention it would be more (CPU) efficient if we
    // would allocate the EMC related collections
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

ostream&  operator<<(ostream& os, const StEvent& e)
{
    os << "Id: " << e.id().first << ", " << e.id().second << endl;
#if defined(__SUNPRO_CC)    
    os << "Type: " << e.type().c_str() << endl;
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

void StEvent::setBunchCrossingNumber(unsigned long val) { mBunchCrossingNumber = val; }      

void StEvent::setLuminosity(double val) { mLuminosity = val; }               

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

void StEvent::setVertexCollection(StVertexCollection* val) { mVertices = val; }               

void StEvent::setTriggerDetectorCollection(StTriggerDetectorCollection* val) { mTriggerDetectors = val; }      

void StEvent::setL0Trigger(StL0Trigger* val) { mL0Trigger = val; }

void StEvent::setEmcTowerHitCollection(StEmcTowerHitCollection* val) { mEmcTowerHits = val; }               

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
