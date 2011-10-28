//
// First Cluster Maker
//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id: StFgtClusterMaker.h,v 1.12 2011/10/28 14:29:43 sgliske Exp $
//   $Log: StFgtClusterMaker.h,v $
//   Revision 1.12  2011/10/28 14:29:43  sgliske
//   fixed CVS tags
//
//   Revision 1.11  2011/10/28 14:28:41  sgliske
//   Cleaned up prepareEnvironment (no functional change).
//   Removed old methods of getting data pointer.
//   Also pClusterAlgo changed to mClusterAlgoPtr to conform with STAR guidelines.
//
//   Revision 1.10  2011/10/26 20:56:50  avossen
//   use geoIds to determine if two strips are adjacent
//
//   Revision 1.9  2011/10/20 17:30:37  balewski
//   revert
//
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
  StFgtClusterMaker( const Char_t* name="FgtCluster");
  virtual ~StFgtClusterMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );

  Int_t setClusterAlgo(StFgtIClusterAlgo*);

 protected:
  StFgtEvent *mFgtEventPtr;
  StFgtIClusterAlgo* mClusterAlgoPtr;

  Int_t prepareEnvironment();

  ClassDef(StFgtClusterMaker,1);

};
#endif
