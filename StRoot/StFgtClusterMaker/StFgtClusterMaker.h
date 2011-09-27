//
// First Cluster Maker
//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id
//   $Log
//

#ifndef STAR_StFgtClusterMaker_HH
#define STAR_StFgtClusterMaker_HH

#include <TStopwatch.h>
#include <math.h>
#include <TString.h>

#include "StMaker.h"
#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StFgtIClusterAlgo.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"


class StFgtClusterMaker : public StMaker
{
 public:
  StFgtClusterMaker(const Char_t* name="FgtCluster");
  virtual ~StFgtClusterMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );

 protected:
  StFgtIClusterAlgo* pClusterAlgo;

  ClassDef(StFgtClusterMaker,1);


}
#endif
