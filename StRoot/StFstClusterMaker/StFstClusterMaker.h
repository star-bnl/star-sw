#ifndef StFstClusterMaker_hh
#define StFstClusterMaker_hh

#include <climits>

#include "StMaker.h"
#include "StFstIClusterAlgo.h"

class StFstCollection;


/**
 * Maker to build clusters from raw FST hits. By default we use clustering
 * algorithm implemented in StFstClusterMaker/StFstScanRadiusClusterAlgo. Other
 * concrete implementations of can be provided by the user.
 *
 * \author Shenghui Zhang
 * \date Sep 2021
 */
class StFstClusterMaker : public StMaker
{
public:
   StFstClusterMaker( const char *name = "fst_cluster");
   ~StFstClusterMaker();
   Int_t Init();
   Int_t Make();
   void Clear( Option_t *opts = "" );

   void setClusterAlgo(StFstIClusterAlgo *);
   void setUsedTimeBin(unsigned char tb=UCHAR_MAX) { mTimeBin = tb; }
   void setClusterSplitFlag(bool splitFlag=true) { mSplitCluster = splitFlag; }

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name: SL20a $ $Id: StFstClusterMaker.h,v 1.15 2015/07/27 18:50:31 huangbc Exp $ built " __DATE__ " " __TIME__  ; return cvs;}

protected:
   StFstCollection *mFstCollectionPtr;
   StFstIClusterAlgo *mClusterAlgoPtr;

   UChar_t mTimeBin;     ///< Time bin to be used
   Bool_t mSplitCluster; ///< Flag to split clusters

   ClassDef(StFstClusterMaker, 0);
};

#endif


/***************************************************************************
* StFstClusterMaker.h,v 1.0
* Revision 1.0 2021/09/21 Shenghui Zhang
* Initial version
****************************************************************************/
