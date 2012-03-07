///
// $Id: StFgtSeededClusterAlgo.h,v 1.3 2012/03/07 03:57:23 avossen Exp $
// $Log: StFgtSeededClusterAlgo.h,v $
// Revision 1.3  2012/03/07 03:57:23  avossen
// various updates
//
// Revision 1.2  2012/03/01 16:38:13  avossen
// implemented tweaks to clustering
//
// Revision 1.1  2012/02/28 19:34:29  avossen
//  added new cluster maker
//
// Revision 1.6  2011/11/01 18:46:30  sgliske
// Updated to correspond with StEvent containers, take 2.
//
// Revision 1.5  2011/10/10 20:35:08  avossen
// fixed strip-cluster association in MaxCluster algo, made other files cvs compliant
//
//
//  \author Anselm Vossen (avossen@indiana.edu) 
//  \class StFgtSeededClusterAlgo
//
// Seeded Clustering algorithm taking the hits from the StFgtEvent and adding Clusters
//
//
//


#ifndef STAR_StFgtSeededClusterAlgo_HH
#define STAR_StFgtSeededClusterAlgo_HH

//#include "StRoot/StEvent/StFgtHit.h"
#include "StFgtIClusterAlgo.h"
//#include "StRoot/StEvent/StFgtStripCollection.h"
class StFgtHit;
class StFgtStrip;
class StFgtStripCollection;

/**
This class implements the IClusterAlgo interface, in particular the doClustering function.
The implemented algo (simple) agregates all strips that are above threshold to clusters. It respects the fact that at the inner radius only every second P-Strip exist.
There is a cutoff on the maximum numbers of strips per cluster as a safety in case of noisy data.

*/


class StFgtSeededClusterAlgo :public StFgtIClusterAlgo
{

 public:
  StFgtSeededClusterAlgo();
  ///the main function, using a collection of strips tht fired to build clusters of neighbouring strips
  virtual Int_t doClustering(  StFgtStripCollection& strips, StFgtHitCollection& clusters );
  virtual Int_t Init();

 protected:
  ///migrated to A2C maker
  //  Bool_t checkPulse(StFgtHit* pClus);
  Int_t addStrips2Cluster(StFgtHit* clus, StFgtStrip** itSeed, StFgtStrip** itVecBegin, StFgtStrip** itVecEnd,Bool_t direction, Int_t sidedSize, Char_t seedLayer);
  Bool_t isSameCluster(StFgtStrip** itSeed,StFgtStrip** nextStrip);
  void FillClusterInfo(StFgtHit* cluster);
 private:
  Bool_t up;
  Bool_t down;
  ClassDef(StFgtSeededClusterAlgo,1);
};
#endif
