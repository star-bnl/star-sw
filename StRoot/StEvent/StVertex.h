/***************************************************************************
 *
 * $Id: StVertex.h,v 1.1 1999/01/30 03:58:10 fisyak Exp $
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
 * $Log: StVertex.h,v $
 * Revision 1.1  1999/01/30 03:58:10  fisyak
 * Root Version of StEvent
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
#ifdef __ROOT__
#include "TObject.h"
#endif
 * version with constructors for table-based loading
 *
#ifndef StGlobalTrack_hh
#include "StGlobalTrack.h"
#endif
#ifndef StVecPtrGlobalTrack_hh
#include "StVecPtrGlobalTrack.h"
#endif

#include "dst_vertex.h"
 *
class StVertex : public TObject {
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
    StVertexType                type();
    StVecPtrGlobalTrack&        daughters();
    const StGlobalTrack*        parent();
    const StThreeVectorF& position();
    const StThreeVectorF& positionError();
    ULong_t               qualityBitmask();
    Float_t                       chiSquared();         
    virtual const StThreeVectorF& position() const{ return mPosition; } ;
    virtual const StThreeVectorF& positionError() const { return mPositionError; };
    void setParent(StGlobalTrack* );         
    void setPosition(const StThreeVectorF&);       
    void setPositionError(const StThreeVectorF&);  
    void setQualityBitmask(ULong_t); 
    void setChiSquared(Float_t);     
    virtual void setParent(StGlobalTrack* );         
protected:
    StVertexType           mType;
    StVecPtrGlobalTrack    mDaughters;
    StGlobalTrack*         mParent;
    StThreeVectorF   mPosition;
    StThreeVectorF   mPositionError;
    ULong_t          mQualityBitmask;
    Float_t                  mChiSquared;                
#ifdef __ROOT__
	ClassDef(StVertex,1)  //StVertex structure
#endif
    virtual void setPositionError(const StThreeVectorF&);  

inline StVertexType StVertex::type() { return mType; }

inline StVecPtrGlobalTrack& StVertex::daughters(){ return mDaughters; }       

inline const StGlobalTrack* StVertex::parent(){ return mParent; }          

inline const StThreeVectorF& StVertex::position(){ return mPosition; }        

inline const StThreeVectorF& StVertex::positionError(){ return mPositionError; }   

inline ULong_t StVertex::qualityBitmask(){ return mQualityBitmask; }  

inline Float_t StVertex::chiSquared(){ return mChiSquared; }      
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
