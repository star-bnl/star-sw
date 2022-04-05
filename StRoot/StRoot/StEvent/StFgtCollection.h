/***************************************************************************
 *
 * $Id: StFgtCollection.h,v 2.2 2013/01/08 19:54:03 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: FGT data collection for StEvent.
 *
 ***************************************************************************
 *
 * $Log: StFgtCollection.h,v $
 * Revision 2.2  2013/01/08 19:54:03  ullrich
 * Added mNumTimeBins and access functions.
 *
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_COLLECTION_H_
#define _ST_FGT_COLLECTION_H_

#include "StObject.h"
#include "StFgtStripCollection.h"
#include "StFgtHitCollection.h"
#include "StFgtPointCollection.h"
#include "StEnumerations.h"

class StFgtCollection : public StObject {
public:
    // constructors
    StFgtCollection();
    // StFgtCollection( const StFgtCollection& other );            ---> use default
    // StFgtCollection& operator=( const StFgtCollection& other ); ---> use default 
    
    // deconstructor
    ~StFgtCollection();
    
    size_t getNumDiscs() const;
    size_t getNumStrips() const;                   // overall
    size_t getNumStrips( unsigned short discIdx) const;  // per disc
    size_t getNumHits() const;                     // overall
    size_t getNumHits( unsigned short discIdx ) const;   // per disc
    size_t getNumPoints() const;
    size_t getNumTimeBins() const;
    void setNumTimeBins(size_t nTimebin);    
    // note: ownership of all pointers is retained by the containers.
    // Do not deleted any pointers received from this class.
    
    StFgtStripCollection* getStripCollection( unsigned short discIdx );
    const StFgtStripCollection* getStripCollection( unsigned short discIdx ) const;
    
    StFgtHitCollection* getHitCollection( unsigned short discIdx );
    const StFgtHitCollection* getHitCollection( unsigned short discIdx ) const;
    
    StFgtPointCollection* getPointCollection();
    const StFgtPointCollection* getPointCollection() const;
    
    void Clear( Option_t *opts = "" );
    
protected:
    friend class StMuDstMaker; // needed for StMuDstMaker
    
    StFgtStripCollection mStripCollection[kFgtNumDiscs];
    StFgtHitCollection mHitCollection[kFgtNumDiscs];
    StFgtPointCollection mPointCollection;
    size_t mNumTimeBins;
private:   
    ClassDef(StFgtCollection,2);
}; 


// inline functions

inline StFgtStripCollection* StFgtCollection::getStripCollection( unsigned short discIdx ) {
    return (discIdx < kFgtNumDiscs ? &mStripCollection[discIdx] : 0 );
};

inline size_t StFgtCollection::getNumTimeBins() const {
    return mNumTimeBins;
};

inline void StFgtCollection::setNumTimeBins(size_t nTimeBins) {
    mNumTimeBins=nTimeBins;
};

inline const StFgtStripCollection* StFgtCollection::getStripCollection( unsigned short discIdx ) const {
    return (discIdx < kFgtNumDiscs ? &mStripCollection[discIdx] : 0 );
};

inline StFgtHitCollection* StFgtCollection::getHitCollection( unsigned short discIdx ) {
    return (discIdx < kFgtNumDiscs ? &mHitCollection[discIdx] : 0 );
};

inline const StFgtHitCollection* StFgtCollection::getHitCollection( unsigned short discIdx ) const {
    return (discIdx < kFgtNumDiscs ? &mHitCollection[discIdx] : 0 );
};

inline size_t StFgtCollection::getNumDiscs() const{
    return kFgtNumDiscs;
};

// sum of all the strips over all discs
inline size_t StFgtCollection::getNumStrips() const {
    size_t n = 0;
    for( const StFgtStripCollection* ptr = &mStripCollection[0]; ptr != &mStripCollection[kFgtNumDiscs]; ++ptr )
        n += ptr->getNumStrips();
    return n;
};

// number of strips hit on one disc
inline size_t StFgtCollection::getNumStrips( unsigned short discIdx ) const{
    return (discIdx < kFgtNumDiscs ? mStripCollection[discIdx].getNumStrips() : 0 );
};

// sum of all the hits over all discs
inline size_t StFgtCollection::getNumHits() const {
    size_t n = 0;
    for( const StFgtHitCollection* ptr = &mHitCollection[0]; ptr != &mHitCollection[kFgtNumDiscs]; ++ptr )
        n += ptr->getNumHits();
    return n;
};

// number of hits hit on one disc
inline size_t StFgtCollection::getNumHits( unsigned short discIdx ) const{
    return (discIdx < kFgtNumDiscs ? mHitCollection[discIdx].getNumHits() : 0 );
};

inline StFgtPointCollection* StFgtCollection::getPointCollection() {
    return &mPointCollection;
};

inline const StFgtPointCollection* StFgtCollection::getPointCollection() const {
    return &mPointCollection;
};

// number of points on one disc
inline size_t StFgtCollection::getNumPoints() const{
    return mPointCollection.getNumPoints();
};

#endif
