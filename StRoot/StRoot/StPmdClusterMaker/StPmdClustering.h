/*!
 * \class StPmdClustering
 * \author
 */
/******************************************************
 *
 * $Id: StPmdClustering.h,v 1.11 2010/05/29 00:47:10 rashmi Exp $
 *
 * Author: Dr. S.C. Phatak
 *         Dipak Mishra
 ******************************************************
 *
 * Description: Base class for PMD clusters
 *
 * $Log: StPmdClustering.h,v $
 * Revision 1.11  2010/05/29 00:47:10  rashmi
 * Added a call to new clustering routines in StPmdClustering
 *
 * Revision 1.10  2010/04/15 06:52:32  rashmi
 * Clustering with option to turn calibration refineclustering on/off
 *
 * Revision 1.9  2007/11/02 11:00:14  rashmi
 * Applying hitcalibration; eta,phi wrt primary vertex
 *
 * Revision 1.8  2007/08/31 10:52:00  rashmi
 * Defined cutoff, setting it using inline SetAdcCutOff()
 *
 * Revision 1.7  2004/09/07 23:02:43  fisyak
 * Add missing default ctors
 *
 * Revision 1.6  2004/07/19 13:23:27  subhasis
 * checks applied on clust_cell dimension
 *
 * Revision 1.5  2004/06/24 13:43:02  subhasis
 * several changes in clustering code
 *
 *
 ******************************************************/
#ifndef STAR_StPmdClustering
#define STAR_StPmdClustering
#include <TH2.h>
#include<math.h>
#include <TH1.h>
#include <TCanvas.h>
#include <TArrayF.h>
#include <TArrayI.h>
#include "StPmdAbsClustering.h"
#include "StThreeVectorF.hh"
class StPmdHit;
class StPmdCluster;
class StPmdModule;
class StPmdDetector;
class StPmdClusterCollection;
class StPmdClustering:public StPmdAbsClustering
{

  private:
 
  // These options are passed to this maker by StPmdClusterMaker
  Bool_t mOptCalibrate;
  Bool_t mOptSimulate;
  Bool_t mOptRefineCluster;
  Double_t cutoff;
  StThreeVectorF mVertexPos;

  StPmdClusterCollection * pmdclus;

 //Made static so that I to declare this only once and not for each module
  static const Int_t NColumnMax = 96;; //Maximum number of cell along x axis
  static const Int_t NRowMax = 72; //Maximum number of cells along y axis
  // static StPmdHit* hitmap[NRowMax][NColumnMax];//stores addresses of hits  
  static const Int_t CutOff = 7;  
  static const Int_t MaxHits = 15000; // max hits in an event
  static const Int_t MaxSuperSize = 2000;
  static const Int_t MaxLocalPeaks = 700;
  static const Int_t MaxClusterSize = 2000;
  static const Int_t MaxNeighLP = 35;
  //  static const Float_t logweight = 4.5;
  //  Float_t logweight;
  static const Int_t debug = 0; // 0 for no cout; 1 for cout;

  //  TClonesArray * mPmdSuperClusters;
  void CellXY(Int_t, Int_t, Float_t&, Float_t&);

  //checks whether the hit position in  question is within the SM boundary
  Int_t CheckBoundary(Int_t, Int_t, Int_t); 


 protected:
  
  
 public:
  // functions for clustering
  //!constructor 
  StPmdClustering(StPmdDetector *pmd_det = 0, StPmdDetector* cpv_det = 0);
  //!destructor
  virtual ~StPmdClustering();
  
  // These are the new cluster maker functions

  //! for Pmd clusters                     
  //  void findPmdClusters();  // new method
  Bool_t findPmdClusters2(StPmdDetector *);
  // Make clusters our of superclusters => refclust
  Int_t MakeClusters(TClonesArray *);
  //Int_t MakeClusters(TClonesArray *, StPmdClusterCollection*);
  // Make SuperClusters => crclust
  Int_t MakeSuperClusters(Int_t, StPmdModule*,TClonesArray*);
  // Helping BreakSuperClusters
  Int_t GetLocalPeaks(Int_t*,TList *);
  // Breaks the supercluster by associating cells with each localPeak
  Bool_t BreakSuperCluster(Int_t, Int_t*, TList * );
  //Bool_t BreakSuperCluster(Int_t, Int_t*, TList * , StPmdClusterCollection *);
  Float_t BuildCluster(StPmdCluster*, Float_t*);
  // GetCluster Properties from hits =>CentroidCal
  void Cell_eta_phi(Float_t, Float_t, Float_t &, Float_t &);

  // These are the old cluster maker functions

  //! for Pmd clusters                     
  //  void findPmdClusters(); // old method
  void findPmdClusters(StPmdDetector *);
  
  //! crude clustering
  Int_t crclust(Double_t , Double_t, Int_t, Int_t); 
  //! refined clustering
  void refclust(StPmdDetector*,Int_t, Int_t, Int_t,StPmdClusterCollection*);
  
  //! for ordering
  void order(Int_t);  
  //! ordering 
  void arrange(Int_t);
  Double_t Dist(Double_t, Double_t, Double_t, Double_t);  //! distance between two clusters
  
  //! for parametrization : not used any more
  //  void gaussfit(Int_t, Int_t, Double_t &, Double_t &, Double_t &, Double_t &, Double_t &,Double_t &,Double_t & ); 
  //! printing clusters
  void printclust(Int_t,Int_t, StPmdCluster*); 
  //! for Calculating cluster properties, those clusters having more then two cells
  Int_t CentroidCal(Int_t,Int_t,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&,Double_t&);
  void SetAdcCutOff(Double_t adccutoff);
  void SetVertexPos(const StThreeVectorF&);
  void Cluster_Eta_Phi(Float_t, Float_t, Float_t,Float_t&, Float_t&);
  

  void SetOptCalibrate(Bool_t a=kTRUE){mOptCalibrate = a;}  // Default is on; YES Calibrate
  void SetOptSimulate(Bool_t a=kFALSE){mOptSimulate = a;}    // Default is off; No Simulation
  void SetOptRefineCluster(Bool_t a=kFALSE){mOptRefineCluster = a;} // Default is off; NO Refine Clustering
  
  
  //! for getting hits of each cluster
  //  StPmdHit* GetHit(StPmdDetector*, Int_t, Int_t, Int_t);
  StPmdHit* GetHit(StPmdDetector*, Int_t, Double_t, Double_t);
  
  ClassDef(StPmdClustering, 1) 
    };
    
    inline void StPmdClustering::SetAdcCutOff(Double_t adccutoff){
      cutoff = adccutoff;
    }

inline void StPmdClustering::SetVertexPos(const StThreeVectorF& vertexPos){
  mVertexPos = vertexPos;
}




#endif





