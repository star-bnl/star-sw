/***************************************************************************
 *
 * $Id: StTrack.h,v 1.1 1999/01/30 03:58:08 fisyak Exp $
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
 * $Log: StTrack.h,v $
 * Revision 1.1  1999/01/30 03:58:08  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/01/30 23:03:16  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:54:02  wenaus
 * version with constructors for table-based loading
 *
#ifdef __ROOT__
#include "TObject.h"
#endif
 * Added non-const method pidTraits().
#include "StTrackPidTraits.h"
#define StTrack_hh 
#include "dst_track.h"
#include "StPhysicalHelixD.hh"
class StTrack : public TObject {
 * Revision 2.2  1999/11/01 12:45:06  ullrich
 *
 * Revision 2.0  1999/10/12 18:42:56  ullrich
 * Completely Revised for New Version
            Double_t phase,
            StThreeVectorD& origin,
	    Int_t h);
    // StTrack(const StTrack&);                     use default
    // const StTrack & operator=(const StTrack&);   use default
    StTrack(const StTrack&);
    StPhysicalHelixD&  helix();
    StVertex*         startVertex();
    StVertex*         stopVertex();
    StTrackFitTraits& fitTraits();
    StTrackPidTraits& pidTraits();
    virtual StVertex*         startVertex();
    void setHelix(const StPhysicalHelixD&);
    void setStartVertex(StVertex*);
    void setStopVertex(StVertex*);
    virtual void setStopVertex(StVertex*);
    virtual void setLength(float length){mLength=length;};
    virtual float length(){return mLength;};
    StVertex*        mStartVertex; //!
    StVertex*        mStopVertex;  //!
    StPhysicalHelixD  mHelix;
    StTrackPidTraits mPidTraits;
#ifdef __ROOT__
	ClassDef(StTrack,1)  //StTrack structure
#endif
    StTrackFitTraits mFitTraits;
    float            mLength;
  ClassDef(StTrack,1)  //StTrack structure
    void         setGeometry(StTrackGeometry*);

inline StPhysicalHelixD& StTrack::helix() { return mHelix; }

inline StVertex* StTrack::startVertex() { return mStartVertex; }


inline StTrackPidTraits& StTrack::pidTraits() { return mPidTraits; }
inline StVertex* StTrack::stopVertex() { return mStopVertex; }

inline StTrackFitTraits& StTrack::fitTraits() { return mFitTraits; }

    
    UChar_t                 mReconstructionMethod;
    void         setNode(StTrackNode*);
    
protected:
    UShort_t                mKey;
    Short_t                 mFlag;
    UChar_t                 mEncodedMethod;
    Float_t                 mImpactParameter;
    Float_t                 mLength;
    UShort_t                mNumberOfPossiblePoints;
    StTrackTopologyMap      mTopologyMap;
    StTrackFitTraits        mFitTraits;
    StTrackGeometry         *mGeometry;
    StTrackDetectorInfo     *mDetectorInfo;         //$LINK
    StTrackNode             *mNode;                 //$LINK
    StSPtrVecTrackPidTraits mPidTraitsVec;

    virtual StObject*       clone() = 0;
    ClassDef(StTrack,1)
};
#endif
