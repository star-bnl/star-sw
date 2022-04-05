/***************************************************************************
 *
 * $Id: StFgtStripCollection.h,v 2.2 2013/01/08 19:52:43 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: A collection of StFgtStrip classes for StEvent.
 * Basically a wrapper for an StSPtrVecFgtStrip
 *
 ***************************************************************************
 *
 * $Log: StFgtStripCollection.h,v $
 * Revision 2.2  2013/01/08 19:52:43  ullrich
 * Changes in streamer.
 *
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_STRIP_COLLECTION_H_
#define _ST_FGT_STRIP_COLLECTION_H_

#include "StObject.h"
#include "StContainers.h"
#include "StFgtStrip.h"

using namespace std;

class StFgtStripCollection : public StObject {
public:
    // constructors
    StFgtStripCollection( short disc = 0 );
    // StFgtStripCollection( const StFgtStripCollection& other );            ---> use default
    // StFgtStripCollection& operator=( const StFgtStripCollection& other ); ---> use default 
    
    // deconstructor
    ~StFgtStripCollection();
    
    // accessors for the underlying vector.  WARNING: never use
    // getStripVec().push_back() or equivelants. Instead use
    // StFgtStripCollection::getStrip to add a new strip.
    StSPtrVecFgtStrip& getStripVec();
    const StSPtrVecFgtStrip& getStripVec() const;
    
    // sort internal vector by geoId
    void sortByGeoId();
    
    // remove all hits with negative geoIds
    void removeFlagged();
    
    // size of internal vector
    size_t getNumStrips() const;
    
    // modify/access the discId
    short getDisc() const;
    void setDisc( short disc );
    
    // Clear
    void Clear( Option_t *opt = "" );
    
    // Get pointer to a strip -- note: this is the only way to modify a
    // strip.  New strip is created if it does not exist, but only
    // using StFgtStrip() constructor.  Ownership is retained by the
    // collection.
    StFgtStrip* getStrip( int elecId );
    
protected:  
    // function used for sorting strips by geoId
    static bool hitGeoIdLessThan( const StFgtStrip* h1, const StFgtStrip* h2 ); 
    
protected:
    // the data members
    Short_t mDisc;
    StSPtrVecFgtStrip mStripVec;    
    
    // temporary copy of the pointers, indexed by elec Id.
    // used for the addStripInfo class
    StPtrVecFgtStrip mStripElecIdVec; //!  do not save none structural containers
    
private:   
    ClassDef(StFgtStripCollection,2);
}; 


// inline functions

inline StFgtStripCollection::StFgtStripCollection( short disc ) : StObject(), mDisc( disc ) {
    mStripElecIdVec.resize( kFgtNumElecIds );
    for (unsigned int i=0; i<mStripElecIdVec.size(); i++)
        mStripElecIdVec[i] = static_cast< StFgtStrip* >(0);
};

inline StSPtrVecFgtStrip& StFgtStripCollection::getStripVec() {
    return mStripVec;
};

inline const StSPtrVecFgtStrip& StFgtStripCollection::getStripVec() const{
    return mStripVec;
};

// sort by geoId
inline void StFgtStripCollection::sortByGeoId(){
    std::sort( mStripVec.begin(), mStripVec.end(), &StFgtStripCollection::hitGeoIdLessThan );
    return;
};

inline size_t StFgtStripCollection::getNumStrips() const {
    return mStripVec.size();
};

inline void StFgtStripCollection::setDisc( short discId ) {
    mDisc = discId;
};

inline short StFgtStripCollection::getDisc() const {
    return mDisc;
};

#endif
