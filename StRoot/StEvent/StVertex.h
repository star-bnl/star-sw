/***************************************************************************
 *
 * $Id: StVertex.h,v 1.8 1999/09/24 01:23:02 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 01/30/1999 T. Wenaus  Add index method to allow indexed access when in
 *                       a list
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.h,v $
 * Revision 1.8  1999/09/24 01:23:02  fisyak
 * Reduced Include Path
 *
 * Revision 1.8  1999/09/24 01:23:02  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/10 09:16:12  ullrich
 * Made position() and positionError() const methods.
 *
 * Revision 1.6  1999/06/24 17:33:01  fisyak
 * Replace Collection by value to Collection by pointer for TBrowser
 *
 * Revision 1.5  1999/04/30 13:16:31  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:39  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.7  1999/04/19 15:54:10  genevb
 * Added momentum() to vertex classes
 *
 * Revision 1.6  1999/03/23 21:47:45  ullrich
 * Member function made virtual
 *
 * Revision 1.5  1999/02/17 11:04:52  ullrich
 * Added numberOfDaughters() and daughter(i) methods.
 *
 * Revision 1.4  1999/01/30 23:03:18  wenaus
 * table load intfc change; include ref change
 *
 * Revision 1.3  1999/01/27 12:53:39  ullrich
 * Made setType() virtual. See StV0Vertex for reason.
 *
 * Revision 1.2  1999/01/15 22:54:22  wenaus
 * version with constructors for table-based loading
 *
 * Revision 2.4  2000/02/10 18:49:08  ullrich
 * Fixed typo introduced at last check-in.
 *
 * Revision 2.2  2000/01/11 19:22:14  ullrich
#include "StEnumerations.h"
#include "StThreeVectorF.hh"
#include "StGlobalTrack.h"
#include "dst_vertex.h"
 * Added non-const parent() method.
class StVertex : public StObject {
protected:
    ULong_t                mIndex;
    StVertexType           mType;
    StVecPtrGlobalTrack   *mDaughters;
    StGlobalTrack         *mParent;
    StThreeVectorF         mPosition;
    StThreeVectorF         mPositionError;
    ULong_t                mQualityBitmask;
    Float_t                mChiSquared;                
 * Completely Revised for New Version
 *
    StVertex(dst_vertex_st*);
    // StVertex(const StVertex&);       use default
    // const StVertex & operator=(const StVertex&);
#include "StMeasuredPoint.h"
#include "StEnumerations.h"
#include "StMatrixF.hh"
#include "StContainers.h"

    virtual StVertexType                type() {return mType;};
    virtual StVecPtrGlobalTrack&        daughters() {return *mDaughters;};
    virtual UInt_t                numberOfDaughters() {return mDaughters ? mDaughters->size() : 0; };
    virtual StGlobalTrack*              daughter(UInt_t);
    virtual const StGlobalTrack*        parent(){ return mParent; };
    virtual const StThreeVectorF& position() const{ return mPosition; } ;
    virtual const StThreeVectorF& positionError() const { return mPositionError; };
    virtual StThreeVectorF        momentum(Double_t);
    virtual ULong_t               qualityBitmask(){ return mQualityBitmask; } ;
    virtual Float_t                       chiSquared(){ return mChiSquared; };         
    virtual Long_t                        index() {return mIndex;};
    ULong_t                flag() const;
    virtual void setType(StVertexType);           
    virtual void setParent(StGlobalTrack* );         
    virtual void setPosition(const StThreeVectorF&);       
    virtual void setPositionError(const StThreeVectorF&);  
    virtual void setQualityBitmask(ULong_t); 
    virtual void setChiSquared(Float_t);     
    virtual void setIndex(Long_t ii) {mIndex = ii;};
    StMatrixF              covariantMatrix() const;  // overwrite inherited
  ClassDef(StVertex,1)  //StVertex structure
    virtual void setFlag(Long_t);
StCollectionDef(Vertex)
inline StGlobalTrack* StVertex::daughter(UInt_t i)
{
  return (mDaughters && i < mDaughters->size() ? *(StGlobalTrack **)mDaughters->GetCell(i) : 0);
}

    
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
