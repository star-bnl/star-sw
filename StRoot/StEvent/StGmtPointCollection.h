/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtPointCollection
 *
 ***************************************************************************
 *
 * Description: A collection of StGmtPoint classes for StEvent.
 * Basically a wrapper for an StSPtrVecGmtPoint
 *
 ***************************************************************************/

#ifndef _ST_GMT_POINT_COLLECTION_H_
#define _ST_GMT_POINT_COLLECTION_H_

#include "StObject.h"
#include "StContainers.h"

class StGmtPoint;

class StGmtPointCollection : public StObject {
public:
    // constructors
    StGmtPointCollection();
    // StGmtPointCollection( const StGmtPointCollection& other );            ---> use default
    // StGmtPointCollection& operator=( const StGmtPointCollection& other ); ---> use default 
    
    // deconstructor
    ~StGmtPointCollection();
    
    // accessors/modifiers for the underlying vector
    StSPtrVecGmtPoint& getPointVec();
    const StSPtrVecGmtPoint& getPointVec() const;
    
    size_t getNumPoints() const;
    
    // Clear
    void Clear( Option_t *opt = "" );
    
protected:
    // the data member
    StSPtrVecGmtPoint mPointVec;
    
private:   
    ClassDef(StGmtPointCollection,1)
}; 


// inline functions

inline StGmtPointCollection::StGmtPointCollection() : StObject() {
   // nothing else to do
};

inline StSPtrVecGmtPoint& StGmtPointCollection::getPointVec() {
   return mPointVec;
};

inline const StSPtrVecGmtPoint& StGmtPointCollection::getPointVec() const{
   return mPointVec;
};

inline size_t StGmtPointCollection::getNumPoints() const {
   return mPointVec.size();
};

#endif

