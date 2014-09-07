/***************************************************************************
*
* $Id: StIstClusterMaker.h,v 1.11 2014/09/07 08:15:18 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* IST cluster maker by calling implemented algorithms.
***************************************************************************/

#ifndef StIstClusterMaker_hh
#define StIstClusterMaker_hh

#include "StMaker.h"
#include "StIstIClusterAlgo.h"

class StIstCollection;

class StIstClusterMaker : public StMaker
{
public:
   StIstClusterMaker( const char *name = "ist_cluster");
   ~StIstClusterMaker();
   Int_t Init();
   Int_t Make();
   void Clear( Option_t *opts = "" );

   Int_t setClusterAlgo(StIstIClusterAlgo *);
   /// time bin to be used
   void setUsedTimeBin(unsigned char tb=-1) { mTimeBin = tb; }
   /// cluster splitting switch
   void setClusterSplitFlag(bool splitFlag=true) { mSplitCluster = splitFlag; }

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstClusterMaker.h,v 1.11 2014/09/07 08:15:18 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

protected:
   StIstCollection *mIstCollectionPtr;
   StIstIClusterAlgo *mClusterAlgoPtr;

   UChar_t mTimeBin;
   Bool_t mSplitCluster;

private:
   ClassDef(StIstClusterMaker, 1);
};

#endif


/***************************************************************************
*
* $Log: StIstClusterMaker.h,v $
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
