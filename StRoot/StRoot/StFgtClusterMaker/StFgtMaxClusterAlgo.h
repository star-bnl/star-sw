//
//  $Id: StFgtMaxClusterAlgo.h,v 1.9 2013/02/20 01:32:27 avossen Exp $
//  $Log: StFgtMaxClusterAlgo.h,v $
//  Revision 1.9  2013/02/20 01:32:27  avossen
//  added n strips before and after cluster
//
//  Revision 1.8  2013/02/19 18:29:05  avossen
//  signature of max cluster algo now updated to conform with interface
//
//  Revision 1.7  2012/03/08 17:43:40  avossen
//  added default cluster algo, made StFgtIClusterAlgo destructor =0
//
//  Revision 1.6  2012/03/07 03:57:23  avossen
//  various updates
//
//  Revision 1.5  2012/02/28 19:32:25  avossen
//  many changes to enable new clustering algo: New strip fields, identification of seed strips, passing neighboring strips, new order in strip collections
//
//  Revision 1.4  2011/11/01 18:46:30  sgliske
//  Updated to correspond with StEvent containers, take 2.
//
//  Revision 1.3  2011/10/10 20:35:08  avossen
//  fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//
// \class StFgtMaxClusterAlgo
// \author Anselm Vossen (avossen@indiana.edu)
//
// This class finds clusters defined as the max hit in each layer
//

#ifndef STAR_StFgtMaxClusterAlgo_HH
#define STAR_StFgtMaxClusterAlgo_HH

#include "StFgtIClusterAlgo.h"

/**
Implements the StFgtIClusterAlgo interface. The doClustering method looks for the strip with the highest charge in each layer.
This is meant as a fallback/test solution as it is fast and does not get confused by large clusters and the like.
It will only find one cluster per layer.
Also, no errors are assigned to charge or location.

Copy constructor and assignment operator omitted deliberately 
*/
class StFgtMaxClusterAlgo :public StFgtIClusterAlgo
{

 public:
  StFgtMaxClusterAlgo();
  ///main work functions getting strips above pedestal for each disk
  virtual Int_t doClustering(const StFgtCollection& fgtCollection, StFgtStripCollection& strips, StFgtHitCollection& clusters );
  virtual Int_t Init();
  void setDb(StFgtDb* pDb);
  virtual ~StFgtMaxClusterAlgo();

 protected:

 private:
  ClassDef(StFgtMaxClusterAlgo,1);
};
inline void StFgtMaxClusterAlgo::setDb(StFgtDb* pDb)
{
}

#endif
