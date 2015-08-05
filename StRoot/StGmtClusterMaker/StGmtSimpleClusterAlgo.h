//
// \class StGmtSimpleClusterAlgo
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtSimpleClusterAlgo
//
// Simple Clustering algorithm taking the hits from the StFgtEvent and adding Clusters
//
//
//
#ifndef STAR_StGmtSimpleClusterAlgo_HH
#define STAR_StGmtSimpleClusterAlgo_HH
/**
   This class implements the IClustberAlgo interface, in particular the doClustering function.
   The implemented algo (simple) agregates all strips that are above threshold to clusters. It respects the fact that at the inner radius only every second P-Strip exist.
   There is a cutoff on the maximum numbers of strips per cluster as a safety in case of noisy data.
   Copy constructor and assignment operator omitted deliberately 
*/
#include "Stypes.h"
class StGmtStripCollection;
class StGmtHitCollection;

class StGmtSimpleClusterAlgo  {

 public:
  StGmtSimpleClusterAlgo() {}
  ///the main function, using a collection of strips tht fired to build clusters of neighbouring strips
  virtual Int_t doClustering(UInt_t module, StGmtStripCollection& strips, StGmtHitCollection& clusters);
  virtual Int_t Init();
  virtual ~StGmtSimpleClusterAlgo() {}
  virtual Bool_t IsPedOk()const {return mCalcOk;}

 protected:
  ///migrated to A2C maker
  //  Bool_t checkPulse(StGmtHit* pClus);
  //   void subtractPedestals( StGmtStripCollection& strips, UInt_t module, Int_t isY );
  void subtractPedestals( StGmtStripCollection& strips, UInt_t module);//event by event technic, do not use.
  void findClusters( StGmtStripCollection& strips, UInt_t module, StGmtHitCollection& clusters);
  void getMaximumElement( StGmtStripCollection& strips, Int_t module, Int_t isy, Int_t *element, Int_t *tb, Double_t *maxADC );
  Double_t getMeanPosition( StGmtStripCollection& strips, Int_t module, Int_t isy, Int_t Maxstr, Int_t Maxtb );
  //new
  void checkPedestals( StGmtStripCollection& strips, UInt_t module );
  void applyPedestals( StGmtStripCollection& strips, UInt_t module );
  void calculatePedestals( StGmtStripCollection& strips, UInt_t module );
  
  //  Double_t ** mSum;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  //  Double_t ** mSqrSum;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Double_t ** mPedestals;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Double_t ** mPedestalStdDev;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Double_t ** mPedestalRMS;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  //  Int_t * mCounters;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  Bool_t ** mCalculated;//[kGmtNumConnectedStripsX + kGmtNumConnectedStripsY]; 
  
  Int_t mEv; // event counter
  Bool_t * mCalcOk; //is ped ok?

 private:
  ClassDef(StGmtSimpleClusterAlgo,1);
};


#endif
