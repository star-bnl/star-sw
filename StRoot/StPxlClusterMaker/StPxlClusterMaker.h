/*!
 * \class StPxlClusterMaker
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterMaker.h,v 1.7 2014/08/06 11:43:34 jeromel Exp $
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
 * Revision 1.7  2014/08/06 11:43:34  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/02/27 00:44:25  smirnovd
 * Minor corrections
 *
 * Revision 1.5  2014/02/21 21:11:06  smirnovd
 * Minor style and empty space adjustments
 *
 * Revision 1.4  2014/01/28 19:29:35  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlClusterMaker_hh
#define StPxlClusterMaker_hh

#include <bitset>

#include "StMaker.h"
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
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlClusterMaker.h,v 1.7 2014/08/06 11:43:34 jeromel Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   void findCluster(StPxlCluster *cluster, Int_t column, Int_t row);

   StPxlClusterCollection *mPxlClusterCollection; ///< pointer to the pxl cluster collection

private:
   const StPxlRawHit * mRawHitMap[kNumberOfPxlRowsOnSensor][kNumberOfPxlColumnsOnSensor]; ///< map of fired pixels in a sensor

   ClassDef(StPxlClusterMaker, 0)
};

#endif
