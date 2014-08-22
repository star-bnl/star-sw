/***************************************************************************
*
* $Id: StIstSimpleClusterAlgo.h,v 1.7 2014/08/22 15:55:15 smirnovd Exp $
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
***************************************************************************/

#ifndef StIstSimpleClusterAlgo_hh
#define StIstSimpleClusterAlgo_hh

#include "StIstIClusterAlgo.h"

class StIstRawHit;
class StIstCluster;
class StIstCollection;
class StIstRawHitCollection;
class StIstClusterCollection;

class StIstSimpleClusterAlgo : public StIstIClusterAlgo
{

public:
   StIstSimpleClusterAlgo();
   Int_t doClustering(const StIstCollection &istCollection, StIstRawHitCollection &rawHits, StIstClusterCollection &clusters );
   Int_t Init();

   void setUsedTimeBin(unsigned char tb = -1);
   void setSplitFlag( bool splitFlag = 1);

protected:
   Bool_t mSplitCluster;
   UChar_t mTimeBin;
   enum {kIstSimpleClusterAlgo = 1};

   Int_t doSplitting(StIstClusterCollection &clusters, unsigned char numTimeBins);
   Int_t splitCluster(int cSize, int clusterSizeList[], StIstRawHit *rawHitPtr[], StIstCluster *clusterIt, StIstClusterCollection &clusters, unsigned char numTimeBins);

private:
   ClassDef(StIstSimpleClusterAlgo, 1);
};

inline void StIstSimpleClusterAlgo::setSplitFlag( bool splitFlag )      { mSplitCluster = splitFlag; };
inline void StIstSimpleClusterAlgo::setUsedTimeBin(unsigned char tb)    { mTimeBin = tb; };
#endif


/***************************************************************************
*
* $Log: StIstSimpleClusterAlgo.h,v $
* Revision 1.7  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.6  2014/08/22 15:50:00  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.5  2014/02/16 21:42:54  ypwang
* getting number of time bins used in current event by StIstCollection::getNumTimeBins() function
*
* Revision 1.4  2014/02/10 16:33:44  smirnovd
* Trimmed trailing spaces, expanded tabs to eight spaces
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstSimpleClusterAlgo.h,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/
