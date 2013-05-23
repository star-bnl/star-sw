/*!
 * \class StPxlCluster
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlCluster.h,v 1.1 2013/05/23 20:57:33 qiuh Exp $
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
 * Revision 1.1  2013/05/23 20:57:33  qiuh
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
    void Centering();
    std::vector<int> columnVec; 
    std::vector<int> rowVec; 
    float columnCenter;
    float rowCenter;
    int idTruth;
    ClassDef(StPxlCluster,1)
};

#endif 
