/***************************************************************************
 *
 * $Id: StFgtHitCollection.h,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: A collection of StFgtHit classes for StEvent.
 * Basically a wrapper for an StSPtrVecFgtHit.  Note, one instance of
 * this class corresponds to one disc.
 *
 ***************************************************************************
 *
 * $Log: StFgtHitCollection.h,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_HIT_COLLECTION_H_
#define _ST_FGT_HIT_COLLECTION_H_

#include "StObject.h"
#include "StContainers.h"

class StFgtHit;

class StFgtHitCollection : public StObject {
public:
    // constructors
    StFgtHitCollection( short discId = -1 );
    // StFgtHitCollection( const StFgtHitCollection& other );            ---> use default
    // StFgtHitCollection& operator=( const StFgtHitCollection& other ); ---> use default 
    
    // deconstructor
    ~StFgtHitCollection();
    
    // accessors/modifiers for the underlying vector
    StSPtrVecFgtHit& getHitVec();
    const StSPtrVecFgtHit& getHitVec() const;
    
    size_t getNumHits() const;
    short getDisc() const;
    void setDisc( short discId );
    
    // Clear
    void Clear( Option_t *opt = "" );
    
protected:
    // the data member
    Short_t mDisc;
    StSPtrVecFgtHit mHitVec;
    
private:   
    ClassDef(StFgtHitCollection,1);
}; 


// inline functions

inline StFgtHitCollection::StFgtHitCollection( short discId ) : StObject(), mDisc( discId ) {
   // nothing else to do
};

inline StSPtrVecFgtHit& StFgtHitCollection::getHitVec() {
   return mHitVec;
};

inline void StFgtHitCollection::setDisc( short discId ) {
   mDisc = discId;
};

inline short StFgtHitCollection::getDisc() const {
   return mDisc;
};

inline const StSPtrVecFgtHit& StFgtHitCollection::getHitVec() const{
   return mHitVec;
};

inline size_t StFgtHitCollection::getNumHits() const {
   return mHitVec.size();
};

#endif

