//
// First Cluster Maker
//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.h,v 1.7 2011/10/17 21:42:02 balewski Exp $
//   $Log: StFgtClusterMaker.h,v $
//   Revision 1.7  2011/10/17 21:42:02  balewski
//   added tmp interface to fgt-simu-maker
//
//   Revision 1.6  2011/10/10 20:35:08  avossen
//   fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
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
//#include "StRoot/St_base/Stypes.h"


class StFgtClusterMaker : public StMaker
{
 public:
  StFgtClusterMaker(const Char_t* rawBaseMakerName,const Char_t* name="FgtCluster");
  virtual ~StFgtClusterMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );

  Int_t setClusterAlgo(StFgtIClusterAlgo*);

 private:
  Bool_t mIsInitialized;

 protected:
  StFgtIClusterAlgo* pClusterAlgo;
  StFgtEvent *mFgtEventPtr;
  std::string mFgtEventMakerName;

  ClassDef(StFgtClusterMaker,1);

};
#endif
