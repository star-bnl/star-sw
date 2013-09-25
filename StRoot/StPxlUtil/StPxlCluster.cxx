/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlCluster.cxx,v 1.7 2013/09/25 11:54:09 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * a group of neighboring pixel raw hits
 *
 ***************************************************************************
 *
 * $Log: StPxlCluster.cxx,v $
 * Revision 1.7  2013/09/25 11:54:09  qiuh
 * *** empty log message ***
 *
 * Revision 1.6  2013/05/23 21:58:15  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/ 

#include "StPxlCluster.h"

ClassImp(StPxlCluster)

StPxlCluster::StPxlCluster(){
}

StPxlCluster::~StPxlCluster()
{
    columnVec.clear();
    rowVec.clear();
}

int StPxlCluster::nRawHits()
{
    return columnVec.size();
}

void StPxlCluster::AddRawHit(int columnAdd, int rowAdd, int idTruthAdd){
    columnVec.push_back(columnAdd);
    rowVec.push_back(rowAdd);
    idVec.push_back(idTruthAdd);
}

void StPxlCluster::Summarize(){
    float columnSum=0;
    float rowSum=0;
    int nRawHits_ = nRawHits();

    std::vector<int> differentIdVec;
    std::vector<int> differentIdCountVec;
    for(int i=0;i<nRawHits_;i++){
        columnSum += columnVec[i];
        rowSum += rowVec[i];

        int idRegistered = 0;
        for(int j=0; j<differentIdVec.size(); j++)
            {
                if(idVec[i] == differentIdVec[j])
                    {
                        differentIdCountVec[j]++;
                        idRegistered = 1;
                        break;
                    }
            }
        if(!idRegistered)
            {
                differentIdVec.push_back(idVec[i]);
                differentIdCountVec.push_back(1);
            }
    }

    columnCenter = columnSum/float(nRawHits_);
    rowCenter = rowSum/float(nRawHits_);

    int maxIdCount = 0;
    int maxIdCountIndex = 0;
    for(int i=0; i<differentIdVec.size(); i++)
        {
            if(maxIdCount<differentIdCountVec[i])
                {
                    maxIdCount = differentIdCountVec[i];
                    maxIdCountIndex = i;
                }
        }
    idTruth = differentIdVec[maxIdCountIndex];

}




