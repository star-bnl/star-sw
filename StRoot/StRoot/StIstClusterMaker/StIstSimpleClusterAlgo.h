#ifndef StIstSimpleClusterAlgo_hh
#define StIstSimpleClusterAlgo_hh

#include "StIstClusterMaker/StIstIClusterAlgo.h"

class StIstRawHit;
class StIstCluster;
class StIstCollection;
class StIstRawHitCollection;
class StIstClusterCollection;


/**
 * Concrete implementation of a simple algorithm for clustering of the raw hits
 * registered by the 2D IST sensors. The clustering is done by searching in the
 * 2D sensor plane indexed by row and column id's.
 *
 * 1) Reads all raw hits per ladder (six sensors) and writes into a vector.
 * 2) Starts from the 1st raw hit, and loops over the vector looking for
 * neighboring raw hits (in a sensor area) and do clustering. The found
 * cluster will be filled into a ladder cluster collection.
 * 3) A case-by-case splitting algorithm can be enabled/disabled for the
 * found clusters (here only works for cases with cluster size <= 4).
 *
 * \author Yaping Wang \date March 2013
 * \author Dmitri Smirnov
 */
class StIstSimpleClusterAlgo : public StIstIClusterAlgo
{
   ~StIstSimpleClusterAlgo(){}
protected:
   enum {kIstSimpleClusterAlgo = 1};

   virtual Int_t doClustering(const StIstCollection &istCollection, StIstRawHitCollection &rawHits, StIstClusterCollection &clusters );
   Int_t doSplitting(StIstClusterCollection &clusters, unsigned char numTimeBins);
   Int_t splitCluster(int cSize, int clusterSizeList[], StIstRawHit *rawHitPtr[], StIstCluster *clusterIt, StIstClusterCollection &clusters, unsigned char numTimeBins);
};

#endif


/***************************************************************************
*
* $Log: StIstSimpleClusterAlgo.h,v $
* Revision 1.17  2018/01/04 17:34:37  smirnovd
* [Cosmetic] Remove StRoot/ from include path
*
* $STAR/StRoot is already in the default path search
*
* Revision 1.16  2015/11/02 20:15:29  perev
* New compile, destructor added
*
* Revision 1.15  2014/09/17 20:39:45  smirnovd
* Squashed commit of the following:
*
* commit 37d3d404a31c9b152811232af55d37177162269d
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 16:11:22 2014 -0400
*
*     Added an author to reflect on contributions
*
* commit 6ceacb443d2d35bc21295b81a3d25b7433d40260
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 16:09:48 2014 -0400
*
*     [Minor] Reversed the logic and saved one level of intentation
*
* commit 4bc24031445ecce9f19b940697d13cc8a755aaf1
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 16:06:42 2014 -0400
*
*     Do not use standard ROOT's dictionary macroses since the classes are transient by design
*
* Revision 1.14  2014/09/17 20:36:20  smirnovd
* Simplified public interface by reducing the number of unnecessarily required parameters
*
* Revision 1.13  2014/09/17 20:33:32  smirnovd
* Squashed commit of the following:
*
* commit 72dc19a6663ea31c719c1a61f6d2b4752dd766aa
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:42 2014 -0400
*
*     Minor code refactoring, clean up
*
* commit e083a10a9fb60b7dcce692ef8043b9227c12768b
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:18:16 2014 -0400
*
*     Removed pointless comments
*
* commit 88d51857362c91c954704cec4a31a0b0fa7fccc5
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:17:26 2014 -0400
*
*     Updated description in doxygen comments
*
* commit eb09527489179fc7dab6aa7f23fd132b25185bb1
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 9 15:15:56 2014 -0400
*
*     StIstScanClusterAlgo: Removed unused variable
*
* commit 1a8df63533c71a0e2ba4d8275ebf89f4e3004765
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Fri Aug 22 16:04:47 2014 -0400
*
*     Neatened headers: Removed unused, spelled paths in includes explicitly as it slightly helps in identifying dependencies
*
* commit 972e8ed41403bd680ade5ecc509f8bca004e86ee
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:20 2014 -0400
*
*     Minor stylistic changes
*
* commit 57daf5a1e0b3246fd12f1dd1c2ca089b62930c83
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 16 16:29:14 2014 -0400
*
*     Improved doxygen comments
*
* Revision 1.12  2014/09/07 13:54:45  ypwang
* move setUsedTimeBin() and setSplitFlag() setters from inherited classes to their base class StIstIClusterAlgo.h
*
* Revision 1.11  2014/09/07 11:41:36  ypwang
* ClassDef version updated from 1 to 0, and remove Init() function
*
* Revision 1.10  2014/08/22 21:38:20  smirnovd
* Restored declaration removed by mistake
*
* Revision 1.9  2014/08/22 21:32:45  smirnovd
* Moved doxygen comment to where it belongs
*
* Revision 1.8  2014/08/22 21:27:27  smirnovd
* Decalred methods virtual as that what they are. Makes the code better to understand
*
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
