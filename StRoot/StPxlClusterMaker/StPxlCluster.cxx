/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 * 
 * $Id: StPxlCluster.cxx,v 1.1 2014/01/23 01:04:43 qiuh Exp $
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
 * $Log: StPxlCluster.cxx,v $
 * Revision 1.1  2014/01/23 01:04:43  qiuh
 * *** empty log message ***
 *
 * 
 **************************************************************************/ 

#include "StPxlCluster.h"

ClassImp(StPxlCluster)

StPxlCluster::StPxlCluster(){
}

StPxlCluster::~StPxlCluster()
{
    mColumnVec.clear();
    mRowVec.clear();
}

Int_t StPxlCluster::nRawHits()
{
    return mColumnVec.size();
}

void StPxlCluster::addRawHit(Int_t columnAdd, Int_t rowAdd, Int_t idTruthAdd){
    mColumnVec.push_back(columnAdd);
    mRowVec.push_back(rowAdd);
    mIdVec.push_back(idTruthAdd);
}

void StPxlCluster::summarize(){
    float columnSum=0;
    float rowSum=0;
    int nRawHits_ = nRawHits();

    std::vector<int> differentIdVec;
    std::vector<int> differentIdCountVec;
    for(int i=0;i<nRawHits_;i++){
        columnSum += mColumnVec[i];
        rowSum += mRowVec[i];

        int idRegistered = 0;
        for(unsigned int j=0; j<differentIdVec.size(); j++)
            {
                if(mIdVec[i] == differentIdVec[j])
                    {
                        differentIdCountVec[j]++;
                        idRegistered = 1;
                        break;
                    }
            }
        if(!idRegistered)
            {
                differentIdVec.push_back(mIdVec[i]);
                differentIdCountVec.push_back(1);
            }
    }

    mColumnCenter = columnSum/float(nRawHits_);
    mRowCenter = rowSum/float(nRawHits_);

    int maxIdCount = 0;
    int maxIdCountIndex = 0;
    for(unsigned int i=0; i<differentIdVec.size(); i++)
        {
            if(maxIdCount<differentIdCountVec[i])
                {
                    maxIdCount = differentIdCountVec[i];
                    maxIdCountIndex = i;
                }
        }
    mIdTruth = differentIdVec[maxIdCountIndex];

}




