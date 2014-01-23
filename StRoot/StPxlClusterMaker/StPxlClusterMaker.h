/*!
 * \class StPxlClusterMaker 
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlClusterMaker.h,v 1.2 2014/01/23 01:04:43 qiuh Exp $
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
 * Revision 1.2  2014/01/23 01:04:43  qiuh
 * *** empty log message ***
 *
 * 
 **************************************************************************/ 

#ifndef StPxlClusterMaker_hh
#define StPxlClusterMaker_hh

#include "StMaker.h"
#include <bitset>
#include "StPxlUtil/StPxlConstants.h"

class StPxlCluster;
class StPxlClusterCollection;

class StPxlClusterMaker : public StMaker {
public:
    StPxlClusterMaker(const char *name="pxl_cluster");
    ~StPxlClusterMaker();
    
    Int_t Init();
    Int_t Make();
    void findCluster(StPxlCluster* cluster, Int_t column, Int_t row); ///< start from (column, row), repeat looking at 8 neighboring pixels to make clusters
    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StPxlClusterMaker.h,v 1.2 2014/01/23 01:04:43 qiuh Exp $ built "__DATE__" "__TIME__ ;
        return cvs;
    }

protected:
    StPxlClusterCollection* mPxlClusterCollection; ///< pointer to pxl cluster collection
    bitset<nPxlColumnsOnSensor> mBitMap[nPxlRowsOnSensor]; ///< bit map of fired pixels in a sensor
    Int_t mMapIdTruth[nPxlRowsOnSensor][nPxlColumnsOnSensor]; ///< map of idTruth of pixels in a sensor
    
    ClassDef(StPxlClusterMaker,0)
};

#endif
