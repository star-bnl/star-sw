/***************************************************************************
 *
 * $Id: StVertex.h,v 1.5 1999/04/30 13:16:31 fisyak Exp $
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
 * Revision 1.5  1999/04/30 13:16:31  fisyak
 * add StArray for StRootEvent
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
    virtual StVertexType                type();
    virtual StVecPtrGlobalTrack&        daughters();
    virtual UInt_t                numberOfDaughters();
#include "StMatrixF.hh"
    virtual const StGlobalTrack*        parent();
    virtual const StThreeVectorF& position();
    virtual const StThreeVectorF& positionError();
    virtual StVecPtrGlobalTrack&        daughters() {return *mDaughters;};
    virtual ULong_t               qualityBitmask();
    virtual Float_t                       chiSquared();         
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
protected:
    ULong_t          mIndex;
    StVertexType           mType;
    StVecPtrGlobalTrack    mDaughters;
    StGlobalTrack*         mParent;
    StThreeVectorF   mPosition;
    StThreeVectorF   mPositionError;
    ULong_t          mQualityBitmask;
    Float_t                  mChiSquared;                
    virtual void setPosition(const StThreeVectorF&);       
    virtual void setPositionError(const StThreeVectorF&);  
    virtual void setQualityBitmask(ULong_t); 

inline StVertexType StVertex::type() { return mType; }

inline StVecPtrGlobalTrack& StVertex::daughters(){ return mDaughters; }       

inline UInt_t StVertex::numberOfDaughters() { return mDaughters.size(); }

    virtual void setChiSquared(Float_t);     
    virtual void setIndex(Long_t ii) {mIndex = ii;};
    return (i < mDaughters.size() ? mDaughters[i] : 0);
  ClassDef(StVertex,1)  //StVertex structure

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
