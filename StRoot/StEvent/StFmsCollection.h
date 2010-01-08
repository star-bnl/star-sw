/***************************************************************************
 *
 * $Id: StFmsCollection.h,v 2.1 2010/01/08 22:42:31 ullrich Exp $
 *
 * Author: Jingguo Ma, Dec 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsCollection.h,v $
 * Revision 2.1  2010/01/08 22:42:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFmsCollection_hh
#define StFmsCollection_hh

#include "StObject.h"
#include "StContainers.h"

class StFmsHit;
//class StFmsCluster;
//class StFmsPoint;

class StFmsCollection : public StObject {
public:
    StFmsCollection();
    ~StFmsCollection();
    
    void          addHit(StFmsHit*);
    unsigned int  numberOfHits() const;

    //void          addCluster(StFmsCluster*);
    //void          addPoint(StFmsPoint*);
    //unsigned int  nClusters() const;
    //unsigned int  nPoints() const;
    
    StSPtrVecFmsHit&              hits();
    const StSPtrVecFmsHit&        hits() const;
    
    //StSPtrVecFmsCluster&        clusters();
    //const StSPtrVecFmsCluster&  clusters() const;
    //StSPtrVecFmsPoint&          points();
    //const StSPtrVecFmsPoint&    points() const;
    
private:
    StSPtrVecFmsHit mHits;
    //StSPtrVecFmsCluster mClusters;
    //StSPtrVecFmsPoint mPoints;
    
    ClassDef(StFmsCollection,1)
};
#endif
