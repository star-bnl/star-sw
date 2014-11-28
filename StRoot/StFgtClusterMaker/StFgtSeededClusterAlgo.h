///
// $Id: StFgtSeededClusterAlgo.h,v 1.10 2013/03/13 15:57:35 akio Exp $
// $Log: StFgtSeededClusterAlgo.h,v $
// Revision 1.10  2013/03/13 15:57:35  akio
// Fix a bug with ZS data and phi-even strip clustering logic
// Also remove some kStFgtNumTimebins and use dynamic local mMaxTimeBin from StFgtCollection
//
// Revision 1.9  2013/02/20 01:32:27  avossen
// added n strips before and after cluster
//
// Revision 1.8  2013/02/19 18:24:04  avossen
// *** empty log message ***
//
// Revision 1.7  2012/12/10 23:18:01  avossen
// merged cluster finder
//
// Revision 1.6  2012/11/27 18:00:07  akio
// - Filling NStrip/SeedType/MaxTimebin/EvenOddChargeAsy in StFgtHit
// - Accepting kFgtSeedTypes4 & 5 for clustring
// - Setting kFgtClusterTooBig instead of kFgtClusterNo into strips when cluster is too big
//   (Seedtype of the seed strip will not be overwritten)
// - Adding setThreshold2AddStrip() [proposed default 2 ~ 3 from cosmic data... no idea for run12 data]
//   This will be used in isSameCluster() and this number times ChargeUncert() will be the threshold on charge
//   (timebin sum) for a strip to be included in cluster if neighbore. It was hard coded to be 1.0 before.
//   Too low threshold sometimes kill cluster because "cluster too big"
// - Slight code change to make trying different weight for getting R/PHI easy...
// - Slight code change for phi/even strip logic... hopefully improved
//
// Revision 1.5  2012/03/16 19:41:15  avossen
// added option to allow to jump strips
//
// Revision 1.4  2012/03/08 17:43:40  avossen
// added default cluster algo, made StFgtIClusterAlgo destructor =0
//
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
#include "TH1D.h"
#include "TH2D.h"

#include "StFgtIClusterAlgo.h"
//#include "StRoot/StEvent/StFgtHit.h"

//#include "StRoot/StEvent/StFgtStripCollection.h"


class StFgtHit;
class StFgtStrip;
class StFgtStripCollection;
class StFgtDb;

/**
This class implements the IClusterAlgo interface, in particular the doClustering function.
The implemented algo (simple) agregates all strips that are above threshold to clusters. It respects the fact that at the inner radius only every second P-Strip exist.
There is a cutoff on the maximum numbers of strips per cluster as a safety in case of noisy data.

Copy constructor and assignment operator omitted deliberately 
*/


class StFgtSeededClusterAlgo :public StFgtIClusterAlgo
{
 public:
  StFgtSeededClusterAlgo();
  ///the main function, using a collection of strips tht fired to build clusters of neighbouring strips
  virtual Int_t doClustering(const StFgtCollection& fgtCollection, StFgtStripCollection& strips, StFgtHitCollection& clusters );
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual ~StFgtSeededClusterAlgo();
  virtual void setJumpSingleStrip(Bool_t jump);
  void setThreshold2AddStrip(Float_t v); // this value * charge uncertaintly is threshold on strip charge to add to the cluster
  void setDb(StFgtDb* pDb);

 protected:
  ///migrated to A2C maker
  //  Bool_t checkPulse(StFgtHit* pClus);
  Int_t addStrips2Cluster(StFgtHit* clus, StFgtStrip** itSeed, StFgtStrip** itVecBegin, StFgtStrip** itVecEnd,Bool_t direction, Int_t sidedSize);
  Bool_t isSameCluster(StFgtStrip** itSeed,StFgtStrip** nextStrip);
  void FillClusterInfo(StFgtHit* cluster,StFgtStripCollection& allStrips);
  void doStripFit(void* stripsT);
  Float_t doClusterShapeFit(void* stripsT);
  void setNumAdditionalStrips(Int_t numStrips);
   // pointer to the DB
  Int_t numAdditionalStrips;
  StFgtDb* mDb;
  int mMaxTimeBin;

 private:
  Bool_t up;
  Bool_t down;
  Bool_t stepTwo;
  Float_t mThreshold2AddStrip;
  /// the number of strips to save on both sides of the cluster



  TH1D* hGaussFitStatus;
  TH1D* hGaussFitChi2;
  TH1D* hTbFitStatus;
  TH1D* hTbFitChi2;

  TH2D* hTbMaxCorr;
  TH1D* hTbMaxRatio;

  TH2D* hTbSideCorr;
  TH1D* hTbSideRatio;

  ClassDef(StFgtSeededClusterAlgo,1);

};

inline void StFgtSeededClusterAlgo::setJumpSingleStrip(Bool_t jump)
{
  stepTwo=jump;
}
inline void StFgtSeededClusterAlgo::setThreshold2AddStrip(Float_t v){ mThreshold2AddStrip=v;}

inline void StFgtSeededClusterAlgo::setNumAdditionalStrips(Int_t numStrips){numAdditionalStrips=numStrips;}
inline void StFgtSeededClusterAlgo::setDb(StFgtDb* pDb){mDb=pDb;}


#endif

