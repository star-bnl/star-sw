/***************************************************************************
 *
 * $Id: StTrack.h,v 2.7 1999/11/30 23:20:32 didenko Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.h,v $
 * Revision 2.7  1999/11/30 23:20:32  didenko
 * temporary solution to get library compiled
 *
 * Revision 2.7  1999/11/30 23:20:32  didenko
 * temporary solution to get library compiled
 *
 * Revision 2.6  1999/11/29 17:32:45  ullrich
 * Added non-const method pidTraits().
 *
 * Revision 2.5  1999/11/15 18:48:22  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.4  1999/11/05 15:27:07  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.3  1999/11/04 13:32:03  ullrich
 * Added non-const versions of some methods
 *
 * Revision 2.2  1999/11/01 12:45:06  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.1  1999/10/28 22:27:24  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:56  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTrack_hh
#define StTrack_hh
#include "StObject.h"
#include "StContainers.h"
#include "StEnumerations.h"
#include "StTrackTopologyMap.h"
#include "StFunctional.h"
#include "StTrackFitTraits.h"

class dst_track_st;
class StParticleDefinition;
class StVertex;
class StTrackGeometry;
class StTrackDetectorInfo;
class StTrackPidTraits;
class StTrackNode;

class StTrack : public StObject {
public:
    StTrack();
    StTrack(const dst_track_st&);
    StTrack(const StTrack&);
    StTrack & operator=(const StTrack&);
    virtual ~StTrack();

    virtual StTrackType            type() const = 0;
    virtual const StVertex*        vertex() const = 0;
    virtual UShort_t               key() const;
    Short_t                        flag() const;
    UShort_t                       encodedMethod() const;
//    StTrackFindingMethod           findingMethod() const;
//    StTrackQualityScheme           qualityScheme() const;
    StTrackFittingMethod           fittingMethod() const;
    Float_t                        impactParameter() const;
    Float_t                        length() const;
    UShort_t                       numberOfPossiblePoints() const;
    UShort_t                       numberOfPossiblePoints(StDetectorId) const;
    const StTrackTopologyMap&      topologyMap() const;
    StTrackGeometry*               geometry();
    const StTrackGeometry*         geometry() const;
    StTrackDetectorInfo*           detectorInfo();
    const StTrackDetectorInfo*     detectorInfo() const;
    const StTrackFitTraits&        fitTraits() const;
    const StSPtrVecTrackPidTraits& pidTraits() const;
    StSPtrVecTrackPidTraits&       pidTraits();
    StPtrVecTrackPidTraits         pidTraits(StDetectorId) const;
    const StParticleDefinition*    pidTraits(StPidAlgorithm&) const;
    StTrackNode*                   node();
    const StTrackNode*             node() const;
    
    void         setFlag(Short_t);
    void         setEncodedMethod(UShort_t);
    void         setImpactParameter(Float_t);
    void         setLength(Float_t);
    void         setTopologyMap(const StTrackTopologyMap&);
    void         setGeometry(StTrackGeometry*);
    void         setFitTraits(const StTrackFitTraits&);
    void         addPidTraits(StTrackPidTraits*);
    void         setDetectorInfo(StTrackDetectorInfo*);
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
