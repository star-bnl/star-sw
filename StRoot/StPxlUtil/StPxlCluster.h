/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlCluster.h,v 1.7 2013/09/25 11:54:09 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * a group of neighboring pixel raw hits
 *
 ***************************************************************************
 *
 * $Log: StPxlCluster.h,v $
 * Revision 1.7  2013/09/25 11:54:09  qiuh
 * *** empty log message ***
 *
 * Revision 1.6  2013/05/23 21:40:38  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/

#ifndef StPxlCluster_hh
#define StPxlCluster_hh

#include "Rtypes.h"

class StPxlCluster{
public:
    StPxlCluster();
    ~StPxlCluster();
    int nRawHits();
    void AddRawHit(int column, int row, int idTruthAdd);
    void Summarize();
    std::vector<int> columnVec; 
    std::vector<int> rowVec; 
    std::vector<int> idVec;
    float columnCenter;
    float rowCenter;
    int idTruth;
    ClassDef(StPxlCluster,1)
};

#endif 
