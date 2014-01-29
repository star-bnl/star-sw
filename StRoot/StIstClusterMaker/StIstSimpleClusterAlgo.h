/***************************************************************************
*
* $Id: StIstSimpleClusterAlgo.h,v 1.2 2014/01/29 18:25:01 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* 1) Reads all raw hits per ladder (six sensors) and writes into a vector.
* 2) Starts from the 1st raw hit, and loops the vector to look for 
* neighboring raw hits (in a sensor area) and do clustering. The found 
* cluster will be filled into a ladder cluster collection.
* 3) A case-by-case splitting algorithm can be enabled/disabled for the 
* found clusters (here only works for cases with cluster size <= 4).
****************************************************************************
*
* $Log: StIstSimpleClusterAlgo.h,v $
* Revision 1.2  2014/01/29 18:25:01  ypwang
* updating scripts
*
*
****************************************************************************
* StIstSimpleClusterAlgo.h,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstSimpleClusterAlgo_hh
#define StIstSimpleClusterAlgo_hh

#include "StIstIClusterAlgo.h"

class StIstRawHit;
class StIstCluster;
class StIstCollection;
class StIstRawHitCollection;
class StIstClusterCollection;

class StIstSimpleClusterAlgo :public StIstIClusterAlgo
{

public:
    StIstSimpleClusterAlgo();
    virtual Int_t doClustering(const StIstCollection& istCollection, StIstRawHitCollection& rawHits, StIstClusterCollection& clusters );
    virtual Int_t Init();

    virtual void setUsedTimeBin(unsigned char tb = -1);    
    virtual void setSplitFlag( bool splitFlag = 1);
    
    virtual ~StIstSimpleClusterAlgo();

protected:
    Bool_t mSplitCluster;
    UChar_t mTimeBin;
    enum {kIstSimpleClusterAlgo=1};

    Int_t doSplitting(StIstClusterCollection& clusters);
    Int_t splitCluster(int cSize, int clusterSizeList[], StIstRawHit *rawHitPtr[], StIstCluster *clusterIt, StIstClusterCollection& clusters);

 private:
  ClassDef(StIstSimpleClusterAlgo,1);
};

inline void StIstSimpleClusterAlgo::setSplitFlag( bool splitFlag )	{ mSplitCluster = splitFlag; };
inline void StIstSimpleClusterAlgo::setUsedTimeBin(unsigned char tb)	{ mTimeBin = tb; };
#endif
