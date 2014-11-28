/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHitCollection
 *
 ***************************************************************************
 *
 * Description: A collection of StGmtHit classes for StEvent.
 * Basically a wrapper for an StSPtrVecGmtHit.  Note, one instance of
 * this class corresponds to one module.
 *
 ***************************************************************************/

#ifndef _ST_GMT_HIT_COLLECTION_H_
#define _ST_GMT_HIT_COLLECTION_H_

#include "StObject.h"
#include "StContainers.h"

class StGmtHit;

class StGmtHitCollection : public StObject {
public:
    // constructors
    StGmtHitCollection( short moduleId = -1 );
    // StGmtHitCollection( const StGmtHitCollection& other );            ---> use default
    // StGmtHitCollection& operator=( const StGmtHitCollection& other ); ---> use default 
    
    // deconstructor
    ~StGmtHitCollection();
    
    // accessors/modifiers for the underlying vector
    StSPtrVecGmtHit& getHitVec();
    const StSPtrVecGmtHit& getHitVec() const;
    
    size_t getNumHits() const;
    short getModule() const;
    void setModule( short moduleId );
    
    // Clear
    void Clear( Option_t *opt = "" );
    
protected:
    // the data member
    Short_t mModule;
    StSPtrVecGmtHit mHitVec;
    
private:   
    ClassDef(StGmtHitCollection,1)
}; 


// inline functions

inline StGmtHitCollection::StGmtHitCollection( short moduleId ) : StObject(), mModule( moduleId ) {
   // nothing else to do
};

inline StSPtrVecGmtHit& StGmtHitCollection::getHitVec() {
   return mHitVec;
};

inline void StGmtHitCollection::setModule( short moduleId ) {
   mModule = moduleId;
};

inline short StGmtHitCollection::getModule() const {
   return mModule;
};

inline const StSPtrVecGmtHit& StGmtHitCollection::getHitVec() const{
   return mHitVec;
};

inline size_t StGmtHitCollection::getNumHits() const {
   return mHitVec.size();
};

#endif

