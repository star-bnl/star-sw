/***************************************************************************
 *
 * $Id: StVertex.h,v 2.0 1999/10/12 18:43:32 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.h,v $
 * Revision 2.0  1999/10/12 18:43:32  ullrich
 * Completely Revised for New Version
 *
 * Revision 2.4  2000/02/10 18:49:08  ullrich
 * Fixed typo introduced at last check-in.
 *
 * Revision 2.3  2000/02/10 16:32:21  ullrich
 * flag changed from unsigned to signed long
 *
 * Revision 2.2  2000/01/11 19:22:14  ullrich
 * Added non-const parent() method.
 *
#include "StArray.h"
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:32  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StVertex_hh
#define StVertex_hh
#include "StMeasuredPoint.h"
#include "StEnumerations.h"
#include "StMatrixF.hh"
#include "StContainers.h"

class dst_vertex_st;
class StTrack;
class StTrackFilter;

class StVertex : public StMeasuredPoint {
public:
    StVertex();
    StVertex(const dst_vertex_st&);
    // StVertex(const StVertex&);            use default
    // StVertex& operator=(const StVertex&); use default
    ULong_t                flag() const;
    Int_t operator==(const StVertex&) const;
    Int_t operator!=(const StVertex&) const;

    virtual StVertexId     type() const = 0;
    Long_t                 flag() const;
    Float_t                chiSquared() const;
    StMatrixF              covariantMatrix() const;  // overwrite inherited
    StThreeVectorF         positionError() const;    // overwrite inherited
    StTrack*               parent();
    const StTrack*         parent() const;
    virtual UInt_t         numberOfDaughters() const = 0;
    virtual StTrack*       daughter(UInt_t) = 0;
    virtual void setFlag(ULong_t);
    virtual StPtrVecTrack  daughters(StTrackFilter&) = 0;

    virtual void setFlag(Long_t);
    
    virtual void setChiSquared(Float_t);
    ULong_t       mFlag;
    virtual void addDaughter(StTrack*) = 0;
    virtual void removeDaughter(StTrack*) = 0;

protected:
    StVertexId    mType;
    Long_t        mFlag;
    Float_t       mCovariantMatrix[6];
    Float_t       mChiSquared;
    StTrack*      mParent;             //$LINK

    ClassDef(StVertex,1)
};
#endif
