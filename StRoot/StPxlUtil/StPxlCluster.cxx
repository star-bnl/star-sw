/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlCluster.cxx,v 1.5 2013/05/23 21:28:43 qiuh Exp $
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
 * Revision 1.5  2013/05/23 21:28:43  qiuh
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
    if(idTruthAdd) idTruth = idTruthAdd;
}

void StPxlCluster::Centering(){
    float columnSum=0;
    float rowSum=0;
    int nRawHits_ = nRawHits();
    for(int i=0;i<nRawHits_;i++){
        columnSum += columnVec[i];
        rowSum += rowVec[i];
    }
    columnCenter = columnSum/float(nRawHits_);
    rowCenter = rowSum/float(nRawHits_);
}




