/*!
 * \class StPxlClusterMaker
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterMaker.h,v 1.4 2014/01/28 19:29:35 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun
 ***************************************************************************
 *
 * Description:
 * Group neighboring pixel raw hits from into clusters.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterMaker.h,v $
 * Revision 1.4  2014/01/28 19:29:35  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlClusterMaker_hh
#define StPxlClusterMaker_hh

#include "StMaker.h"
#include <bitset>
#include "StPxlUtil/StPxlConstants.h"
#include "StPxlCluster.h"

class StPxlClusterCollection;
class StPxlRawHit;

class StPxlClusterMaker : public StMaker
{
public:
   StPxlClusterMaker(const char *name = "pxl_cluster");
   void Clear(const Option_t * = "");
   Int_t Make();
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlClusterMaker.h,v 1.4 2014/01/28 19:29:35 qiuh Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

protected:
   //! Start from (column, row), look at 8 neighboring pixels for fired pixels (raw hits)
   //! If a raw hit is found nearby, continue to look from that pixel, until all neighboring raw hits are found
   //! Fill the neighboring raw hits into a cluster
   void findCluster(StPxlCluster *cluster, Int_t column, Int_t row);

   StPxlClusterCollection *mPxlClusterCollection; ///< pointer to the pxl cluster collection
   const StPxlRawHit *mRawHitMap[kNumberOfPxlRowsOnSensor][kNumberOfPxlColumnsOnSensor]; ///< map of fired pixels in a sensor

   ClassDef(StPxlClusterMaker, 0)
};

#endif
