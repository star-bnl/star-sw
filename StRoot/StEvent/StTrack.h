/*!
 * \class StTrack 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrack.h,v 2.15 2002/03/14 17:42:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.h,v $
 * Revision 2.15  2002/03/14 17:42:15  ullrich
 * Added method to set mNumberOfPossiblePoints.
 *
 * Revision 2.14  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.13  2001/09/28 22:20:50  ullrich
 * Added helix geometry at last point.
 *
 * Revision 2.12  2001/05/30 17:45:55  perev
 * StEvent branching
 *
 * Revision 2.11  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.10  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.9  2001/03/16 20:56:46  ullrich
 * Added non-const version of fitTraits().
 *
 * Revision 2.8  1999/12/01 15:58:10  ullrich
 * New decoding for dst_track::method. New enum added.
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
    virtual unsigned short         key() const;
    short                          flag() const;
    unsigned short                 encodedMethod() const;
    bool                           finderMethod(StTrackFinderMethod) const;
    StTrackFittingMethod           fittingMethod() const;
    float                          impactParameter() const;
    float                          length() const;
    unsigned short                 numberOfPossiblePoints() const;
    unsigned short                 numberOfPossiblePoints(StDetectorId) const;
    const StTrackTopologyMap&      topologyMap() const;
    StTrackGeometry*               geometry();
    const StTrackGeometry*         geometry() const;
    StTrackGeometry*               outerGeometry();
    const StTrackGeometry*         outerGeometry() const;
    StTrackDetectorInfo*           detectorInfo();
    const StTrackDetectorInfo*     detectorInfo() const;
    StTrackFitTraits&              fitTraits();
    const StTrackFitTraits&        fitTraits() const;
    const StSPtrVecTrackPidTraits& pidTraits() const;
    StSPtrVecTrackPidTraits&       pidTraits();
    StPtrVecTrackPidTraits         pidTraits(StDetectorId) const;
    const StParticleDefinition*    pidTraits(StPidAlgorithm&) const;
    StTrackNode*                   node();
    const StTrackNode*             node() const;
    
    void         setFlag(short);
    void         setEncodedMethod(unsigned short);
    void         setImpactParameter(float);
    void         setLength(float);
    void         setTopologyMap(const StTrackTopologyMap&);
    void         setGeometry(StTrackGeometry*);
    void         setOuterGeometry(StTrackGeometry*);
    void         setFitTraits(const StTrackFitTraits&);
    void         addPidTraits(StTrackPidTraits*);
    void         setDetectorInfo(StTrackDetectorInfo*);
    void         setNumberOfPossiblePoints(unsigned short);
    void         setNode(StTrackNode*);
    
protected:
    UShort_t                mKey;
    Short_t                 mFlag;
    UShort_t                mEncodedMethod;
    UShort_t                mNumberOfPossiblePoints;
    Float_t                 mImpactParameter;
    Float_t                 mLength;
    StTrackTopologyMap      mTopologyMap;
    StTrackFitTraits        mFitTraits;
    StTrackGeometry         *mGeometry;
    StTrackGeometry         *mOuterGeometry;
//  StTrackDetectorInfo         *mDetectorInfo;         //$LINK
//  StTrackNode                 *mNode;                 //$LINK
#ifdef __CINT__
    StObjLink                    mDetectorInfo;         
    StObjLink      		 mNode;                 	
#else
    StLink<StTrackDetectorInfo>  mDetectorInfo;         
    StLink<StTrackNode>          mNode;                 	
#endif //__CINT__

    StSPtrVecTrackPidTraits mPidTraitsVec;

    virtual StObject*       clone() const = 0;
    ClassDef(StTrack,3)
};
#endif
