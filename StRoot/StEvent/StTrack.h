/***************************************************************************
 *
 * $Id: StTrack.h,v 1.7 1999/09/24 01:23:02 fisyak Exp $
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
 * Revision 1.7  1999/09/24 01:23:02  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/24 01:23:02  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/06/23 15:06:16  perev
 * length of track is added
 *
 * Revision 1.5  1999/04/30 13:16:29  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:37  fisyak
 * New version with pointer instead referencies
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
 * Revision 2.6  1999/11/29 17:32:45  ullrich
 * Added non-const method pidTraits().
#define StTrack_hh 
 * Revision 2.5  1999/11/15 18:48:22  ullrich
#include "StPhysicalHelixD.hh"
 *
#include "dst_track.h"
 * Revision 2.2  1999/11/01 12:45:06  ullrich
 *
 * Revision 2.0  1999/10/12 18:42:56  ullrich
 * Completely Revised for New Version
 *
#include "StObject.h"
    StTrack(dst_track_st*);
    StTrack(dst_track_st* trk,
            Double_t curvature,
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
    virtual StVertex*         startVertex();
    virtual StVertex*         stopVertex();
    virtual StTrackFitTraits& fitTraits();

    virtual void setHelix(const StPhysicalHelixD&);
    virtual void setStartVertex(StVertex*);
    virtual void setStopVertex(StVertex*);
    virtual void setLength(float length){mLength=length;};
    virtual float length(){return mLength;};
    StTrackGeometry*               geometry();
    const StTrackGeometry*         geometry() const;
    StPhysicalHelixD  mHelix;
    StVertex*        mStartVertex;
    StVertex*        mStopVertex;
    StTrackFitTraits mFitTraits;
    float            mLength;
  ClassDef(StTrack,1)  //StTrack structure
    void         setGeometry(StTrackGeometry*);

inline StPhysicalHelixD& StTrack::helix() { return mHelix; }

inline StVertex* StTrack::startVertex() { return mStartVertex; }

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
