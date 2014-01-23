/***************************************************************************
*
* $Id: StIstRawHitCollection.h,v 1.1 2014/01/23 20:11:31 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* A collection of StIstRawHit classes, and basically is a wrapper for a 
* raw hits vector. One instance corresponds to one ladder.
****************************************************************************
*
* $Log: StIstRawHitCollection.h,v $
* Revision 1.1  2014/01/23 20:11:31  ypwang
* adding scripts
*
*
****************************************************************************
* StIstRawHitCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstRawHitCollection_hh
#define StIstRawHitCollection_hh

#include "StObject.h"
#include "StIstRawHit.h"
#include <algorithm> 

class StIstRawHitCollection : public StObject {
public:
    // constructors
    StIstRawHitCollection(unsigned char ladder = 0);
    // deconstructor
    ~StIstRawHitCollection();
    
    vector<StIstRawHit*>& getRawHitVec();
    const vector<StIstRawHit*>& getRawHitVec() const;
    
    // sort internal vector by raw hit geometry ID
    void sortByGeoId();
    
    // remove all hits with negative geometry IDs
    void removeFlagged();
        
    // size of internal vector
    size_t getNumRawHits() const;
    
    // modify/access the ladder
    unsigned char getLadder() const;
    void setLadder( unsigned char ladder );
    
    // Clear
    void Clear( Option_t *opt = "" );
    
    // Get pointer to a raw hit by channel ID
    StIstRawHit* getRawHit( int elecId );
        
protected:  
    // function used for sorting raw hits by geoId
    static bool rawHitIdLessThan( const StIstRawHit* h1, const StIstRawHit* h2 );
    
protected:
    // data members
    unsigned char mLadder;
    std::vector<StIstRawHit*> mRawHitVec;
    
    // temporary copy of the pointers, indexed by elec Id.
    std::vector<StIstRawHit*> mRawHitElecIdVec;
        
private:   
    ClassDef(StIstRawHitCollection,1);
}; 


// inline functions
inline StIstRawHitCollection::StIstRawHitCollection( unsigned char ladder ) : StObject(), mLadder( ladder ) {
    mRawHitElecIdVec.resize( kIstNumElecIds );
    for (unsigned int i=0; i<mRawHitElecIdVec.size(); i++)
        mRawHitElecIdVec[i] = static_cast< StIstRawHit* >(0);
};

inline vector<StIstRawHit*>& StIstRawHitCollection::getRawHitVec() {
    return mRawHitVec;
};

inline const vector<StIstRawHit*>& StIstRawHitCollection::getRawHitVec() const{
    return mRawHitVec;
};

inline void StIstRawHitCollection::sortByGeoId(){
    std::sort( mRawHitVec.begin(), mRawHitVec.end(), &StIstRawHitCollection::rawHitIdLessThan );
    return;
};

inline size_t StIstRawHitCollection::getNumRawHits() const {
    return mRawHitVec.size();
};

inline void StIstRawHitCollection::setLadder( unsigned char ladder ) {
    mLadder = ladder;
};

inline unsigned char StIstRawHitCollection::getLadder() const {
    return mLadder;
};

#endif
