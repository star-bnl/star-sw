/***************************************************************************
 *
 * $Id: StVertex.cxx,v 2.2 1999/11/22 15:04:43 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.cxx,v $
 * Revision 2.2  1999/11/22 15:04:43  ullrich
 * Forgot to fill mPosition in constructor. Fixed now.
 *
 * Revision 2.5  2000/02/10 16:32:19  ullrich
 * flag changed from unsigned to signed long
 *
 * Revision 2.4  2000/01/11 19:22:12  ullrich
 * Added non-const parent() method.
 *
 * Revision 2.3  1999/12/21 15:09:23  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/11/22 15:04:43  ullrich
 * Forgot to fill mPosition in constructor. Fixed now.
 *
 * Revision 2.1  1999/10/28 22:28:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 **************************************************************************/
#include <algorithm>
#include "StVertex.h"
#include "tables/St_dst_vertex_Table.h"
#include "StTrack.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

ClassImp(StVertex)

static const char rcsid[] = "$Id: StVertex.cxx,v 2.2 1999/11/22 15:04:43 ullrich Exp $";

StVertex::StVertex()
{
    mType = kUndefinedVtxId;
    mFlag = 0;
    mChiSquared = 0;
    mParent = 0;
    fill_n(mCovariantMatrix, 6, 0);
}

StVertex::StVertex(const dst_vertex_st& v)
{
    mType = kUndefinedVtxId;
    mFlag = v.iflag;
    copy(v.covar+0, v.covar+6, mCovariantMatrix);
    mChiSquared = v.chisq[0];
    mParent = 0;
    mPosition.setX(v.x);
    mPosition.setY(v.y);
    mPosition.setZ(v.z);
}

StVertex::~StVertex() {/* noop */};

Int_t
StVertex::operator==(const StVertex& v) const
{
    return mType == v.mType &&
        mFlag == v.mFlag &&
        mPosition == v.mPosition &&
        mChiSquared == v.mChiSquared;
}

Int_t
StVertex::operator!=(const StVertex& v) const
{
ULong_t
}

Long_t
StVertex::flag() const { return mFlag; }

Float_t
StVertex::chiSquared() const { return mChiSquared; }

StMatrixF
StVertex::covariantMatrix() const
{
    StMatrixF m(3,3);
    m(1,1) = mCovariantMatrix[0];
    m(1,2) = m(2,1) = mCovariantMatrix[1];
    m(2,2) = mCovariantMatrix[2];
    m(1,3) = m(3,1) = mCovariantMatrix[3];
    m(2,3) = m(3,2) = mCovariantMatrix[4];
    m(3,3) = mCovariantMatrix[5];
    return m;
}
    
    return StThreeVectorF(sqrt(mCovariantMatrix[0]), sqrt(mCovariantMatrix[2]), sqrt(mCovariantMatrix[5]));
}

StTrack*
StVertex::parent() { return mParent; }

const StTrack*
StVertex::setFlag(ULong_t val) { mFlag = val; }

void
StVertex::setFlag(Long_t val) { mFlag = val; }

void
StVertex::setCovariantMatrix(Float_t val[6]) { copy(val, val+6, mCovariantMatrix); }

void
StVertex::setChiSquared(Float_t val) { mChiSquared = val; }

void
StVertex::setParent(StTrack* val) { mParent = val; }
