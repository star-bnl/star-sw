/***************************************************************************
 *
 * $Id: StTrack.h,v 1.3 1999/04/27 01:24:27 fisyak Exp $
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
 * Revision 1.3  1999/04/27 01:24:27  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.8  1999/04/08 14:58:38  ullrich
 * Moved PID traits from StTrack to StGlobalTrack.
 *
 * Revision 1.7  1999/03/23 21:47:39  ullrich
 * Member function made virtual
 *
 * Revision 1.6  1999/02/24 12:49:01  ullrich
 * Added argument (h) to constructor needed to instatiate helix
 *
 * Revision 1.5  1999/02/15 16:17:04  wenaus
 * fix Double_t& -> Double_t referencing bug
 *
 * Revision 1.4  1999/02/12 02:01:20  wenaus
 * New track constructor to load helix params independently of table
 *
 * Revision 1.3  1999/01/30 23:03:16  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.2  1999/01/15 22:54:02  wenaus
 * version with constructors for table-based loading
 *
#include "TObject.h"
 * Added non-const method pidTraits().
#include "StTrackPidTraits.h"
#define StTrack_hh 
#include "tables/dst_track.h"
#include "StPhysicalHelixD.hh"
 *
class StTrack : public TObject {
 * Revision 2.2  1999/11/01 12:45:06  ullrich
 *
 * Revision 2.0  1999/10/12 18:42:56  ullrich
 * Completely Revised for New Version
 *
#include "StObject.h"
    StTrack(dst_track_st*);
    StTrack(dst_track_st* trk,
            StThreeVectorF& origin,
            Double_t dip,
            Double_t phase,
            StThreeVectorD& origin,
	    Int_t h);
    // StTrack(const StTrack&);                     use default
    // const StTrack & operator=(const StTrack&);   use default
    StTrack(const StTrack&);
    Int_t operator==(const StTrack&) const;
    Int_t operator!=(const StTrack&) const;

    virtual StPhysicalHelixD&  helix();
    virtual StTrackPidTraits& pidTraits();
    virtual StVertex*         startVertex();
    virtual StVertex*         stopVertex();
    virtual StTrackFitTraits& fitTraits();

    virtual void setStopVertex(StVertex*);
    virtual void setLength(float length){mLength=length;};
    virtual float length(){return mLength;};
    StTrackGeometry*               geometry();
    const StTrackGeometry*         geometry() const;
    StPhysicalHelixD  mHelix;
    StTrackPidTraits mPidTraits;
    ClassDef(StTrack,1)  //StTrack structure
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
