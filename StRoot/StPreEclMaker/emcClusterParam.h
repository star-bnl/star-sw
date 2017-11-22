#ifndef __emcClusterParam_st__
#define __emcClusterParam_st__
#include "TDataSet.h"
#include "TTable.h"
#include "Ttypes.h"
/*
*********************************************************
* Table:       emcClusterParam.idl
* Description: define parametres for EMC cluster finder
* Author     : Aleksei Pavlinov, WSU
*********************************************************
*/
  struct  emcClusterParam_st{
    short detector;        /* detector number => see StEmcUtil/emcInternalDef.h */
    float energySeed;      /* seed energy for cluster */
    float energyAdd;       /* */
    float energyThreshold; /* energy threshold for cluster */
    short sizeMax;         /* max cluster size */ 
  };                                                                            
class St_emcClusterParam : public TTable {
 public:
  ClassDefTable(St_emcClusterParam,emcClusterParam_st)
  ClassDef(St_emcClusterParam,1) //C++ container for chain/makers status 
};

#endif /* __emcClusterParam_st__ */
