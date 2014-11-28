#ifndef __StEEmcPi0Maker_h__
#define __StEEmcPi0Maker_h__

#include "StMaker.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcGenericClusterMaker.h"
#include "StEEmcPool/StEEmcPointMaker/StEEmcGenericPointMaker.h"

class TH1F;
class TH2F;
class TTree;
class TClonesArray;
class TFile;
class TTree;

class StEEmcMixEvent;

#include "StEEmcPair.h"

class StEEmcPi0Maker : public StMaker
{

 public:

  StEEmcPi0Maker( const Char_t              *name="pi0maker",
		  StEEmcA2EMaker            *aemk=NULL,
		  StEEmcGenericClusterMaker *clmk=NULL,
		  StEEmcGenericPointMaker   *ptmk=NULL );
  ~StEEmcPi0Maker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  /// Return a copy of the list of diphoton candidates
  StEEmcPairVec_t pairs(){ return mPairs; }
  Int_t numberOfPairs(){ return (Int_t) mPairs.size(); }
  StEEmcPair pair( Int_t i ){ return mPairs[i]; }

  void addTrigger( Int_t t );
  void setCheckTrigger(Bool_t t);
  Bool_t checkTrigger();

  void   setFile( TFile *file );
  TTree *tree();

 private:
 protected:

  TFile *mFile;
  TTree *mTree;

  StEEmcMixEvent *mPi0Event;

  std::vector<Int_t> mTriggerList;
  Bool_t mCheckTrigger;

  StEEmcA2EMaker            *mEEanalysis; /**< endcap adc --> energy maker */
  StEEmcGenericClusterMaker *mEEclusters; /**< endcap cluster maker        */
  StEEmcGenericPointMaker   *mEEpoints;   /**< endcap point maker          */

  StEEmcPairVec_t mPairs;

  TH2F *hMass;
  TH1F *hPT;
  TH1F *hXF;
  TH1F *hEnergy;
  TH1F *hEta;
  TH1F *hPhi;
  TH1F *hZgg;
  TH1F *hZvertex;

  TH1F *hEChi2;
  TH1F *hE1Chi2;
  TH1F *hE2Chi2;
  TH1F *hEChi2_low;
  TH1F *hE1Chi2_low;
  TH1F *hE2Chi2_low;
  TH1F *hEChi2_hi;
  TH1F *hE1Chi2_hi;
  TH1F *hE2Chi2_hi;

  TH1F *hRatio;
  TH1F *hRatio_low;
  TH1F *hRatio_hi;

  TH2F *hMass_cut;
  TH1F *hPT_cut;
  TH2F *hMass_split;
  TH1F *hZgg_cut;
  TH2F *hdEds;

  TH1F *hEvents;

  ClassDef(StEEmcPi0Maker,1);

};

#endif
