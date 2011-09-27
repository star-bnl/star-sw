// $Id: StFgtIClusterAlgo.h,v 1.4 2011/09/27 22:14:27 avossen Exp $
// $Log: StFgtIClusterAlgo.h,v $
// Revision 1.4  2011/09/27 22:14:27  avossen
// cluster maker compiles
//
// Revision 1.3  2011/09/27 17:19:05  avossen
// simple cluster makers
//
// Revision 1.2  2011/08/24 14:30:44  avossen
// Continued raw maker development
//
// Revision 1.1  2011/08/23 03:05:09  avossen
// *** empty log message ***
//
//
//author Anselm Vossen
//
//abstract base class for cluster algorithm implementation
//
//
#ifndef STAR_StFgtIClusterAlgo_HH
#define STAR_StFgtIClusterAlgo_HH

#include "StRoot/StEvent/StFgtEvent/StFgtEvent.h"
#include "StRoot/StEvent/StFgtEvent/StFgtClusterArray.h"
#include "StRoot/StEvent/StFgtEvent/StFgtRawHitArray.h"

class StFgtIClusterAlgo
{
 public:
  //subclasses must implement this function that takes raw hits from StEvent and fills the Cluster collection
  virtual Int_t doClustering(const StFgtRawHitArray&, StFgtClusterArray&)=0;
  virtual Int_t Init(StFgtEvent* mEvent)=0;
  
};

#endif
