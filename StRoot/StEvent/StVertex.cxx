/***************************************************************************
 *
 * $Id: StVertex.cxx,v 1.5 1999/06/24 17:33:01 fisyak Exp $
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
 * $Log: StVertex.cxx,v $
 * Revision 1.5  1999/06/24 17:33:01  fisyak
 * Replace Collection by value to Collection by pointer for TBrowser
 *
 * Revision 1.5  1999/06/24 17:33:01  fisyak
 * Replace Collection by value to Collection by pointer for TBrowser
 *
 * Revision 1.4  1999/04/28 22:27:39  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.4  1999/04/19 18:07:35  genevb
 * Fixed vertex constructor
 *
 * Revision 1.3  1999/04/19 15:54:10  genevb
 * Added momentum() to vertex classes
 *
 * Revision 1.2  1999/01/15 22:54:22  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.5  2000/02/10 16:32:19  ullrich
 *
#include "StGlobalTrack.h"

static const Char_t rcsid[] = "$Id: StVertex.cxx,v 1.5 1999/06/24 17:33:01 fisyak Exp $";
 *
#include "tables/dst_vertex.h"
 * Modified to cope with new compiler version on Sun (CC5.0).
StCollectionImp(Vertex)
  StVertex::StVertex():mType(undefined),mDaughters(new StVecPtrGlobalTrack),mParent(0),mQualityBitmask(0),mChiSquared(0){}
 * Revision 2.2  1999/11/22 15:04:43  ullrich
StVertex::StVertex(dst_vertex_st* vtx):mType(undefined),mDaughters(new StVecPtrGlobalTrack),mParent(0)
 *
  mPosition.setX(vtx->x);
  mPosition.setY(vtx->y);
  mPosition.setZ(vtx->z);
  mPositionError.setX(vtx->sigma[0]);
  mPositionError.setY(vtx->sigma[1]);
  mPositionError.setZ(vtx->sigma[2]);
  mChiSquared = vtx->pchi2;
  mQualityBitmask = vtx->iflag;
#include "tables/St_dst_vertex_Table.h"
#include "StTrack.h"
StVertex::~StVertex() { SafeDelete(mDaughters);}

{
Int_t StVertex::operator==(const StVertex& v) const
}
    return mType == v.mType &&mPosition == v.mPosition;
    mFlag = v.iflag;
    copy(v.covar+0, v.covar+6, mCovariantMatrix);
Int_t StVertex::operator!=(const StVertex& v) const
    mPosition.setX(v.x);
    mPosition.setY(v.y);
    mPosition.setZ(v.z);
}
void StVertex::setType(StVertexType val) { mType = val; }           
{
void StVertex::setParent(StGlobalTrack*  val) { mParent = val; }         
StMatrixF
void StVertex::setPosition(const StThreeVectorF& val) { mPosition = val; }       
    StMatrixF m(3,3);
void StVertex::setPositionError(const StThreeVectorF& val) { mPositionError = val; }  
    m(2,2) = mCovariantMatrix[2];
void StVertex::setQualityBitmask(ULong_t val) { mQualityBitmask = val; } 
    m(3,3) = mCovariantMatrix[5];
void StVertex::setChiSquared(Float_t val) { mChiSquared = val; }     
    
StThreeVectorF StVertex::momentum(Double_t B)
{
    if (mParent) {
      return mParent->helix().momentum(B);
    } else {
      StThreeVectorF mMomentum;
      for (Int_t i=0; i<numberOfDaughters(); i++) {
        mMomentum += daughter(i)->helix().momentum(B);
      }
      return mMomentum;
    }
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
