/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtCollection
 *
 ***************************************************************************
 *
 * Description: GMT data collection for StEvent.
 *
 ***************************************************************************/

#ifndef _ST_GMT_COLLECTION_H_
#define _ST_GMT_COLLECTION_H_

#include "StObject.h"
#include "StGmtStripCollection.h"
#include "StGmtHitCollection.h"
#include "StGmtPointCollection.h"
#include "StEnumerations.h"

class StGmtCollection : public StObject {
public:
    // constructors
    StGmtCollection();
    // StGmtCollection( const StGmtCollection& other );            ---> use default
    // StGmtCollection& operator=( const StGmtCollection& other ); ---> use default 
    
    // deconstructor
    ~StGmtCollection();
    
    size_t getNumModules() const;
    size_t getNumStrips() const;                   // overall
    size_t getNumStrips( unsigned short moduleIdx) const;  // per module
    size_t getNumHits() const;                     // overall
    size_t getNumHits( unsigned short moduleIdx ) const;   // per module
    size_t getNumPoints() const;
    
    // note: ownership of all pointers is retained by the containers.
    // Do not deleted any pointers received from this class.
    
    StGmtStripCollection* getStripCollection( unsigned short moduleIdx );
    const StGmtStripCollection* getStripCollection( unsigned short moduleIdx ) const;

    StGmtHitCollection* getHitCollection( unsigned short moduleIdx );
    const StGmtHitCollection* getHitCollection( unsigned short moduleIdx ) const;
    
    StGmtPointCollection* getPointCollection();
    const StGmtPointCollection* getPointCollection() const;
    
    void Clear( Option_t *opts = "" );
    
protected:
    friend class StMuDstMaker; // needed for StMuDstMaker
    
    StGmtStripCollection mStripCollection[kGmtNumModules];
    StGmtHitCollection mHitCollection[kGmtNumModules];
    StGmtPointCollection mPointCollection;
    
private:   
    ClassDef(StGmtCollection,1)
}; 


// inline functions

inline StGmtStripCollection* StGmtCollection::getStripCollection( unsigned short moduleIdx ) {
    return (moduleIdx < kGmtNumModules ? &mStripCollection[moduleIdx] : 0 );
};

inline const StGmtStripCollection* StGmtCollection::getStripCollection( unsigned short moduleIdx ) const {
    return (moduleIdx < kGmtNumModules ? &mStripCollection[moduleIdx] : 0 );
};

inline StGmtHitCollection* StGmtCollection::getHitCollection( unsigned short moduleIdx ) {
    return (moduleIdx < kGmtNumModules ? &mHitCollection[moduleIdx] : 0 );
};

inline const StGmtHitCollection* StGmtCollection::getHitCollection( unsigned short moduleIdx ) const {
    return (moduleIdx < kGmtNumModules ? &mHitCollection[moduleIdx] : 0 );
};

inline size_t StGmtCollection::getNumModules() const{
    return kGmtNumModules;
};

// sum of all the strips over all modules
inline size_t StGmtCollection::getNumStrips() const {
    size_t n = 0;
    for( const StGmtStripCollection* ptr = &mStripCollection[0]; ptr != &mStripCollection[kGmtNumModules]; ++ptr )
        n += ptr->getNumStrips();
    return n;
};

// number of strips hit on one module
inline size_t StGmtCollection::getNumStrips( unsigned short moduleIdx ) const{
    return (moduleIdx < kGmtNumModules ? mStripCollection[moduleIdx].getNumStrips() : 0 );
};

// sum of all the hits over all modules
inline size_t StGmtCollection::getNumHits() const {
    size_t n = 0;
    for( const StGmtHitCollection* ptr = &mHitCollection[0]; ptr != &mHitCollection[kGmtNumModules]; ++ptr )
        n += ptr->getNumHits();
    return n;
};

// number of hits hit on one module
inline size_t StGmtCollection::getNumHits( unsigned short moduleIdx ) const{
    return (moduleIdx < kGmtNumModules ? mHitCollection[moduleIdx].getNumHits() : 0 );
};

inline StGmtPointCollection* StGmtCollection::getPointCollection() {
    return &mPointCollection;
};

inline const StGmtPointCollection* StGmtCollection::getPointCollection() const {
    return &mPointCollection;
};

// number of points in one module
inline size_t StGmtCollection::getNumPoints() const{
    return mPointCollection.getNumPoints();
};

#endif
