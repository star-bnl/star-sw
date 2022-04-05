#ifndef __StEEmcQAMaker_h__
#define __StEEmcQAMaker_h__

#include "StMaker.h"
class StEEmcA2EMaker;
class StMuDstMaker;
class TH1F;
class TH2F;

#include <vector>

class StEEmcQAMaker : public StMaker
{
public:
  StEEmcQAMaker(const Char_t *name);
  virtual ~StEEmcQAMaker(){ /* nada */ };

  /// Initialize the maker
  virtual Int_t Init();
  /// Process one event
  virtual Int_t Make();

  /// sets pointer to adc-->energy maker 
  void analysis(const Char_t *name);
  /// sets pointer to the muDst maker
  void mudst(const Char_t *name); 
  /// add a trigger to the trigger list
  void trigger(Int_t t){ mTriggerList.push_back(t); }
  
  Int_t    nVertexMax; /**<- Maximum number of vertices */
  Int_t    nVertexMin; /**<- Minimum number of vertices */
  Float_t  zVertexMin; /**<- Minimum z vertex */
  Float_t  zVertexMax; /**<- Maximum z vertex */

  void softTrigger( Float_t s ) { mSoftTrig = s; }

protected:

  /// Tower sampling fraction
  Float_t mSamplingFractionT;
  /// SMD sampling fraction
  Float_t mSamplingFractionU;
  /// SMD sampling fraction
  Float_t mSamplingFractionV;

  /// ADC --> Energy
  StEEmcA2EMaker *mEEanalysis; 
  /// MuDst
  StMuDstMaker *mMuDst; 
  /// List of triggers to process
  std::vector<Int_t> mTriggerList; //!

  Bool_t CheckTriggers(); /**<- analyse triggers */
  Bool_t CheckVertex();   /**<- analyse vertex */
  Bool_t CheckTracks();   /**<- check tracks */
  Bool_t EEmcResponse();  /**<- analyse eemc response */

  /// Histogram for counting events
  TH1F *hEventCounter;

  /// Histograms for counting triggers (one per trigger ID)
  std::vector<TH1F *> hTriggers; 
  std::vector<TH1F *> hTriggersHard;

  /// Software trigger threshold
  Float_t mSoftTrig;
  
  Int_t mSectorTrigger; /**<- index of sector in which trigger fired */

  /// Frequency tower is highest on EEMC
  TH2F               *hFrequencyT;     //!
  /// Frequency pre1 is highest on EEMC
  TH2F               *hFrequencyP;     //!
  /// Frequency pre2 is highest on EEMC
  TH2F               *hFrequencyQ;     //!
  /// Frequency post is highest on EEMC
  TH2F               *hFrequencyR;     //! 

  /// Energy deposited in layer
  std::vector<TH1F *> hEnergyDepositT; //! [MeV]
  /// Energy deposited in layer
  std::vector<TH1F *> hEnergyDepositP; //! [MeV]
  /// Energy deposited in layer
  std::vector<TH1F *> hEnergyDepositQ; //! [MeV]
  /// Energy deposited in layer
  std::vector<TH1F *> hEnergyDepositR; //! [MeV]
  /// Energy deposited in layer
  std::vector<TH1F *> hEnergyDepositU; //! [MeV]
  /// Energy deposited in layer
  std::vector<TH1F *> hEnergyDepositV; //! [MeV]

  /// Multiplicity in layer
  std::vector<TH1F *> hMultiplicityT;  //! 
  /// Multiplicity in layer
  std::vector<TH1F *> hMultiplicityP;  //! 
  /// Multiplicity in layer
  std::vector<TH1F *> hMultiplicityQ;  //! 
  /// Multiplicity in layer
  std::vector<TH1F *> hMultiplicityR;  //! 
  /// Multiplicity in layer
  std::vector<TH1F *> hMultiplicityU;  //! 
  /// Multiplicity in layer
  std::vector<TH1F *> hMultiplicityV;  //! 

  /// ADC 
  std::vector<TH1F *> hAdcT;           //!
  /// ADC 
  std::vector<TH1F *> hAdcP;           //!
  /// ADC 
  std::vector<TH1F *> hAdcQ;           //!
  /// ADC 
  std::vector<TH1F *> hAdcR;           //!
  /// ADC 
  std::vector<TH1F *> hAdcU;           //!
  /// ADC 
  std::vector<TH1F *> hAdcV;           //!


  /// Number of global tracks
  std::vector<TH1F *> hNglobal;        //!
  /// Number of primary tracks
  std::vector<TH1F *> hNprimary;       //!
  /// Number of vertices
  std::vector<TH1F *> hNvertex;        //!
  /// Z vertex
  std::vector<TH1F *> hZvertex;        //! 
  /// Error on z vertex
  std::vector<TH1F *> hZvertexErr;     //! 
  /// Rank of vertex
  std::vector<TH1F *> hRankVertex;     //!
  /// Number of tracks matched to vertex
  std::vector<TH1F *> hNtrackVertex;   //!
  /// Number of tracks matched to eemc and vertex
  std::vector<TH1F *> hNtrackVertexEE; //! 
  /// Scalar sum of PT of tracks associated with vertex
  std::vector<TH1F *> hPTsumVertex;    //! 

  ClassDef(StEEmcQAMaker,1);
};

#endif
