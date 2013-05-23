/*!
 * \class StPxlClusterMaker 
 * \author Qiu Hao, Jan 2013, according codes from Xiangming Sun
 */
/***************************************************************************
 * 
 * $Id: StPxlClusterMaker.h,v 1.1 2013/05/23 20:57:17 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2013, according codes from Xiangming Sun 
 ***************************************************************************
 *
 * Description:
 * Group neighboring pixel raw hits from into clusters.
 *
 ***************************************************************************
 *
 * $Log: StPxlClusterMaker.h,v $
 * Revision 1.1  2013/05/23 20:57:17  qiuh
 * *** empty log message ***
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
    virtual ~StPxlClusterMaker();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual void FindCluster(StPxlCluster* cluster, int column, int row);
    virtual const Char_t *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPxlClusterMaker.h,v 1.1 2013/05/23 20:57:17 qiuh Exp $ built "__DATE__" "__TIME__;
    return cvs;
  }
    StPxlClusterCollection* m_pxlClusterCollection;
    TObjectSet* m_pxlClusterDataSet;

 protected:
    bitset<nPxlColumnsOnSensor> bitMap[nPxlRowsOnSensor];
    int mapIdTruth[nPxlRowsOnSensor][nPxlColumnsOnSensor];

 private:

  ClassDef(StPxlClusterMaker,0)
};

#endif
