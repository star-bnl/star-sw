//
// First Cluster Maker
// \class StGmtClusterMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtClusterMaker


#ifndef STAR_StGmtClusterMaker_HH
#define STAR_StGmtClusterMaker_HH

#include "StMaker.h"
#include "StGmtTrivia.h"
#include "TTree.h"
#include "TFile.h"
#include "StRoot/StChain/StRTSBaseMaker.h"

class StGmtIClusterAlgo;
class StGmtTrivia;

/**
The cluster maker. It uses an external algorithm which has to implement StGmtIClusterAlgo to do the actual clustering by calling doClustering with the gmt hits in StEvent.

*/
class StGmtClusterMaker :  public StRTSBaseMaker//public StMaker
{
  //omitted assignment operator and copy constructor on purpose
 public:
  StGmtClusterMaker( const Char_t* name="GmtCluster");
  ~StGmtClusterMaker();
  /// Init function. Checks if there is a cluster algo and initializes the same.
  Int_t Init();
  /**
     The make function. Uses the cluster algo member to do the actual clustering. Then it does some post processing using the info on the absolute position of the disk that the algo does not have.
In addition the cluster error is computed for the orthogonal direction (e.g. for r clusters in phi) from the strip length.
  */
  Int_t Make();
  Int_t Finish();
  ///clear function is empty at the moment
  //  virtual void Clear( Option_t *opts = "" );

  /**sets the clustering algorithm. Currently there is the simple Clustering algorithm and the max cluster algorithm. 
The simple cluster algorithm is the default one. The max cluster only selects one hit stip per plane, the one with the highest charge
   */
  Int_t setClusterAlgo(StGmtIClusterAlgo*);

//   virtual const char *GetCVS() const
//   {static const char cvs[]="Tag $Name:  $ $Id: StGmtClusterMaker.h,v 1.1.1.1 2013/09/02 15:01:02 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  static Int_t gmtStat;
 protected:
  StGmtIClusterAlgo* mClusterAlgoPtr;

  TFile * ftriviafile;
  TTree * ftriviatree;
  StGmtTrivia * ftriviahit;
  


  ClassDef(StGmtClusterMaker,1);

};
#endif
