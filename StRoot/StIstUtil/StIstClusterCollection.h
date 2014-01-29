/***************************************************************************
*
* $Id: StIstClusterCollection.h,v 1.2 2014/01/29 18:25:03 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* A collection of StIstCluster classes, and basically is a wrapper for a 
* clusters vector. One instance corresponds to one ladder.
****************************************************************************
*
* $Log: StIstClusterCollection.h,v $
* Revision 1.2  2014/01/29 18:25:03  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstClusterCollection_hh
#define StIstClusterCollection_hh

#include "StObject.h"
#include "StIstCluster.h"

class StIstClusterCollection : public StObject {
public:
    // constructors
    StIstClusterCollection(unsigned char ladder = 0);
    
    // deconstructor
    ~StIstClusterCollection();
    
    vector<StIstCluster*>& getClusterVec();
    const vector<StIstCluster*>& getClusterVec() const;
    
    // size of internal vector
    size_t getNumClusters() const;
    
    // modify/access the ladder
    unsigned char getLadder() const;
    void setLadder( unsigned char ladder );
    
    // Clear
    void Clear( Option_t *opt = "" );
            
protected:
    // data members
    unsigned char mLadder;
    std::vector<StIstCluster*> mClusterVec;
    
private:   
    ClassDef(StIstClusterCollection,1);
}; 

// inline functions
inline StIstClusterCollection::StIstClusterCollection( unsigned char ladder ) : StObject(), mLadder( ladder ) {
/* no op */
};

inline vector<StIstCluster*>& StIstClusterCollection::getClusterVec() {
    return mClusterVec;
};

inline const vector<StIstCluster*>& StIstClusterCollection::getClusterVec() const{
    return mClusterVec;
};

inline size_t StIstClusterCollection::getNumClusters() const {
    return mClusterVec.size();
};

inline void StIstClusterCollection::setLadder( unsigned char ladder ) {
    mLadder = ladder;
};

inline unsigned char StIstClusterCollection::getLadder() const {
    return mLadder;
};

#endif
