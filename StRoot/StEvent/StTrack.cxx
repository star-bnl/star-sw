/***************************************************************************
 *
 * $Id: StTrack.cxx,v 1.2 1999/02/09 21:19:37 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.cxx,v $
 * Revision 1.2  1999/02/09 21:19:37  fisyak
 * Import new Torre staff
 *
 * Revision 1.3  1999/02/12 02:01:19  wenaus
 * New track constructor to load helix params independently of table
 *
 * Revision 1.2  1999/01/15 22:54:02  wenaus
 * version with constructors for table-based loading
 *
static const Char_t rcsid[] = "$Id: StTrack.cxx,v 1.2 1999/02/09 21:19:37 fisyak Exp $";
 * New decoding for dst_track::method. New enum added.
#ifdef __ROOT__
 *
static const Char_t rcsid[] = "$Id: StTrack.cxx,v 1.2 1999/02/09 21:19:37 fisyak Exp $";
#endif
StTrack::StTrack() : mHelix(0, 0, 0, StThreeVectorF())
ClassImp(StTrack)
ClassImp(StTrack)
StTrack::StTrack() : mHelix(0, 0, 0, StThreeVectorD())
static const char rcsid[] = "$Id: StTrack.cxx,v 1.2 1999/02/09 21:19:37 fisyak Exp $";
    mStartVertex = 0;
StTrack::StTrack(dst_track_st* trk) : mFitTraits(trk), mHelix((double )0 , (double )0, (double )0 , StThreeVectorF())
StTrack::StTrack(dst_track_st* trk) : 
    mStartVertex = 0;
    mStopVertex   = 0;
  mHelix(0, 0, 0, StThreeVectorD()), mFitTraits(trk), 
{  
StTrack::operator=(const StTrack& track)
{
        mEncodedMethod = track.mEncodedMethod;
StTrack::~StTrack() { /* noop */ }
        mNumberOfPossiblePoints = track.mNumberOfPossiblePoints;
Int_t StTrack::operator==(const StTrack& t) const
const StTrackGeometry*
    return t.mHelix == mHelix;

StTrackGeometry*
Int_t StTrack::operator!=(const StTrack& t) const

void StTrack::setHelix(const StPhysicalHelix& val) { mHelix = val; }
{
    return pid(*this, mPidTraitsVec);
void StTrack::setHelix(const StPhysicalHelixD& val) { mHelix = val; }
StTrackNode*
void StTrack::setStartVertex(StVertex* val) { mStartVertex = val; }
void
void StTrack::setStopVertex(StVertex* val) { mStopVertex = val; }
void
StTrack::setEncodedMethod(UShort_t val) { mEncodedMethod = val; }

void
StTrack::setImpactParameter(Float_t val) { mImpactParameter = val; }

void
StTrack::setLength(Float_t val) { mLength = val; }

void
StTrack::setTopologyMap(const StTrackTopologyMap& val) { mTopologyMap = val; }

void
StTrack::setGeometry(StTrackGeometry* val)
{
    if (mGeometry) delete mGeometry;
    mGeometry = val;
}

void
StTrack::setFitTraits(const StTrackFitTraits& val) { mFitTraits = val; }

void
StTrack::addPidTraits(StTrackPidTraits* val) { mPidTraitsVec.push_back(val); }

void
StTrack::setDetectorInfo(StTrackDetectorInfo* val) { mDetectorInfo = val; }

void
StTrack::setNode(StTrackNode* val) { mNode = val; }
