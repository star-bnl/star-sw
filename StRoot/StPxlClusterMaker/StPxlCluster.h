/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlCluster.h,v 1.1 2014/01/23 01:04:43 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * a group of neighboring pixel raw hits
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlCluster.h,v $
 * Revision 1.1  2014/01/23 01:04:43  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlCluster_hh
#define StPxlCluster_hh

#include "Rtypes.h"

class StPxlCluster{
public:
    StPxlCluster();
    ~StPxlCluster();
    Int_t nRawHits();  ///< number of raw hits
    void addRawHit(Int_t column, Int_t row, Int_t idTruthAdd); ///< add a raw hit to the cluster
    void summarize(); ///< calculate column center, row center, and most frequent idTruth
    Float_t columnCenter() {return mColumnCenter;} ///< average raw hit column
    Float_t rowCenter() {return mRowCenter;} ///< average raw hit row
    Int_t idTruth() {return mIdTruth;} ///< for embedding, 0 as background, most frequent raw hit idTruth as idTruth of the cluster
    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StPxlCluster.h,v 1.1 2014/01/23 01:04:43 qiuh Exp $ built "__DATE__" "__TIME__ ;
        return cvs;
    }

protected:
    std::vector<int> mColumnVec; ///< vector of raw hit column
    std::vector<int> mRowVec; ///< vector of raw hit row
    std::vector<int> mIdVec; ///< vector of raw hit idTruth
    Float_t mColumnCenter; ///< average raw hit column
    Float_t mRowCenter; ///< average raw hit row
    Int_t mIdTruth; ///< for embedding, 0 as background, most frequent raw hit idTruth as idTruth of the cluster
    ClassDef(StPxlCluster,1)
};

#endif 
