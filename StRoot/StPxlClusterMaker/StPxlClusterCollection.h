/*!
 * \class StPxlClusterCollection
 * \author Qiu Hao, March 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlClusterCollection.h,v 1.5 2017/09/01 02:58:33 dongx Exp $
 *
 * Author: Qiu Hao, March 2013
 ***************************************************************************
 *
 * Description:
 * pxl cluster collection
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterCollection.h,v $
 * Revision 1.5  2017/09/01 02:58:33  dongx
 * Update to ensure idTruth is preserved for MC hits for overlapping scenarios between MC/data and two or more MC hits
 *
 * Revision 1.4  2014/08/06 11:43:34  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2014/01/28 19:29:35  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef StPxlClusterCollection_hh
#define StPxlClusterCollection_hh

#include "StObject.h"
#include "StPxlCluster.h"
#include "StPxlUtil/StPxlConstants.h"

class StPxlClusterCollection : public StObject
{
public:
   StPxlClusterCollection();
   void addCluster(Int_t sector, Int_t ladder, Int_t sensor, const StPxlCluster &cluster); ///< add a cluster to the collection
   Int_t numberOfClusters(Int_t sector, Int_t ladder, Int_t sensor) const; ///< number of clusters in a sensor
   Int_t numberOfClusters() const;
   const StPxlCluster *cluster(Int_t sector, Int_t ladder, Int_t sensor, Int_t clusterIndex) const; ///< pointer to a cluster in the collection
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlClusterCollection.h,v 1.5 2017/09/01 02:58:33 dongx Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   vector<StPxlCluster> mClusterVec[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder]; ///< vectors to store clusters

   ClassDef(StPxlClusterCollection, 1)
};

#endif
