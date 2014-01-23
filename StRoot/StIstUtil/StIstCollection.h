/***************************************************************************
*
* $Id: StIstCollection.h,v 1.1 2014/01/23 20:11:31 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* A data collection for StIstRawHitCollection and StIstClusterCollection 
* classes, and not written into StEvent.
****************************************************************************
*
* $Log: StIstCollection.h,v $
* Revision 1.1  2014/01/23 20:11:31  ypwang
* adding scripts
*
*
****************************************************************************
* StIstCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstCollection_hh
#define StIstCollection_hh

#include "StObject.h"
#include "StIstRawHitCollection.h"
#include "StIstClusterCollection.h"
#include "StIstConsts.h"

class StIstCollection : public StObject {
public:
    // constructors
    StIstCollection();
    
    // deconstructor
    ~StIstCollection();
    
    size_t getNumLadders() const;
    size_t getNumRawHits() const;                   // overall
    size_t getNumRawHits( unsigned char ladder) const;      // per ladder
    size_t getNumClusters() const;                  // overall
    size_t getNumClusters( unsigned char ladder ) const;    // per ladder
    size_t getNumTimeBins() const;
    void setNumTimeBins(size_t nTimebin);    
    
    StIstRawHitCollection* getRawHitCollection( unsigned char ladder );
    const StIstRawHitCollection* getRawHitCollection( unsigned char ladder ) const;
    
    StIstClusterCollection* getClusterCollection( unsigned char ladder );
    const StIstClusterCollection* getClusterCollection( unsigned char ladder ) const;
    
    void Clear( Option_t *opts = "" );
    
protected:
    friend class StMuDstMaker; // needed for StMuDstMaker
    
    StIstRawHitCollection mRawHitCollection[kIstNumLadders];
    StIstClusterCollection mClusterCollection[kIstNumLadders];
    size_t mNumTimeBins;
    
private:   
    ClassDef(StIstCollection,1);
}; 


// inline functions
inline size_t StIstCollection::getNumLadders() const{
    return kIstNumLadders;
};

inline size_t StIstCollection::getNumTimeBins() const {
    return mNumTimeBins;
};

inline void StIstCollection::setNumTimeBins(size_t nTimeBins) {
    mNumTimeBins=nTimeBins;
};

inline StIstRawHitCollection* StIstCollection::getRawHitCollection( unsigned char ladder ) {
    return (ladder < kIstNumLadders ? &mRawHitCollection[ladder] : 0 );
};

inline const StIstRawHitCollection* StIstCollection::getRawHitCollection( unsigned char ladder ) const {
    return (ladder < kIstNumLadders ? &mRawHitCollection[ladder] : 0  );
};

inline StIstClusterCollection* StIstCollection::getClusterCollection( unsigned char ladder ) {
    return (ladder < kIstNumLadders ? &mClusterCollection[ladder] : 0 );
};

inline const StIstClusterCollection* StIstCollection::getClusterCollection( unsigned char ladder ) const {
    return (ladder < kIstNumLadders ? &mClusterCollection[ladder] : 0 );
};

// sum of all the raw hits over all ladders
inline size_t StIstCollection::getNumRawHits() const {
    size_t n = 0;
    for( const StIstRawHitCollection* ptr = &mRawHitCollection[0]; ptr != &mRawHitCollection[kIstNumLadders]; ++ptr )
        n += ptr->getNumRawHits();
    return n;
};

// number of raw hits on one ladder
inline size_t StIstCollection::getNumRawHits( unsigned char ladder ) const{
    return (ladder < kIstNumLadders ? mRawHitCollection[ladder].getNumRawHits() : 0 );
};

// sum of all the clusters over all ladders
inline size_t StIstCollection::getNumClusters() const {
    size_t n = 0;
    for( const StIstClusterCollection* ptr = &mClusterCollection[0]; ptr != &mClusterCollection[kIstNumLadders]; ++ptr )
        n += ptr->getNumClusters();
    return n;
};

// number of clusters on one ladder
inline size_t StIstCollection::getNumClusters( unsigned char ladder ) const{
    return (ladder < kIstNumLadders ? mClusterCollection[ladder].getNumClusters() : 0 );
};
#endif
