#ifndef StIstClusterMaker_hh
#define StIstClusterMaker_hh

#include <climits>

#include "StMaker.h"
#include "StIstIClusterAlgo.h"

class StIstCollection;


/**
 * Maker to build clusters from raw IST hits. By default we use clustering
 * algorithm implemented in StIstClusterMaker/StIstScanClusterAlgo. Other
 * concrete implementations of can be provided by the user.
 *
 * \author Yaping Wang
 * \date March 2013
 */
class StIstClusterMaker : public StMaker
{
public:
   StIstClusterMaker( const char *name = "ist_cluster");
   ~StIstClusterMaker();
   Int_t Init();
   Int_t Make();
   void Clear( Option_t *opts = "" );

   void setClusterAlgo(StIstIClusterAlgo *);
   void setUsedTimeBin(unsigned char tb=UCHAR_MAX) { mTimeBin = tb; }
   void setClusterSplitFlag(bool splitFlag=true) { mSplitCluster = splitFlag; }

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstClusterMaker.h,v 1.15 2015/07/27 18:50:31 huangbc Exp $ built " __DATE__ " " __TIME__  ; return cvs;}

protected:
   StIstCollection *mIstCollectionPtr;
   StIstIClusterAlgo *mClusterAlgoPtr;

   UChar_t mTimeBin;     ///< Time bin to be used
   Bool_t mSplitCluster; ///< Flag to split clusters

   ClassDef(StIstClusterMaker, 0);
};

#endif


/***************************************************************************
*
* $Log: StIstClusterMaker.h,v $
* Revision 1.15  2015/07/27 18:50:31  huangbc
* Add space before and after "__DATE__" and "__TIME__" for compling under gcc4.8.2
*
* Revision 1.14  2015/05/20 20:53:53  smirnovd
* Set default value of unsigned variables in a more explicit way
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
* Revision 1.12  2014/09/07 11:31:29  ypwang
* update the setClusterAlgo() returning void instead of Int_t type
*
* Revision 1.11  2014/09/07 08:15:18  ypwang
* destructor was added for the mIstCollectionPtr and mClusterAlgoPtr objects killing
*
* Revision 1.10  2014/08/22 21:27:19  smirnovd
* Remove inline keyword and move the methods inside the definition. Let the compiler optimize the code as it should not be a problem with these one-liners
*
* Revision 1.9  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.8  2014/08/21 17:51:08  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.7  2014/08/12 23:04:53  ypwang
* remove the raw hit number cut per ladder before doing clustering, due to chip occupancy cut was added in raw hit maker which can do the bad column rejection; simplfy the code by removing the InitRun() function
*
* Revision 1.6  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.5  2014/02/15 20:02:37  ypwang
* Clear() member function added, and mIstCollectionPtr data member defined
*
* Revision 1.4  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstClusterMaker.h,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/
