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
  ~StEEmcQAMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");

  /// sets pointer to adc-->energy maker 
  void analysis(const Char_t *name);
  /// sets pointer to the muDst maker
  void mudst(const Char_t *name); 
  /// add a trigger to the trigger list
  void trigger(Int_t t){ mTriggerList.push_back(t); }


  /// Cuts on primary vertex (see constructor for defaults)
  Int_t    nVertexMax;
  Int_t    nVertexMin;
  Float_t  zVertexMin;
  Float_t  zVertexMax;  


 private:
 protected:

  Float_t mSamplingFractionT;
  Float_t mSamplingFractionP;
  Float_t mSamplingFractionQ;
  Float_t mSamplingFractionR;
  Float_t mSamplingFractionU;
  Float_t mSamplingFractionV;

  /// ADC --> Energy
  StEEmcA2EMaker *mEEanalysis; 
  /// MuDst
  StMuDstMaker *mMuDst; 
  /// List of triggers to process
  std::vector<Int_t> mTriggerList; //!

  /// Analysis/histograming methods
  Bool_t CheckTriggers(); 
  Bool_t CheckVertex(); 
  Bool_t CheckTracks();
  Bool_t EEmcResponse();

  //////////////////////////////// HISTOGRAMS ///////////////////////////////

  TH1F *hEventCounter;

  /// EEMC response

  std::vector<TH1F *> hTriggers; 

  Int_t mSectorTrigger; // index of sector in which trigger fired

  TH2F               *hFrequencyT;     //!
  TH2F               *hFrequencyP;     //!
  TH2F               *hFrequencyQ;     //!
  TH2F               *hFrequencyR;     //! 

  std::vector<TH1F *> hEnergyDepositT; //! [MeV]
  std::vector<TH1F *> hEnergyDepositP; //! [MeV]
  std::vector<TH1F *> hEnergyDepositQ; //! [MeV]
  std::vector<TH1F *> hEnergyDepositR; //! [MeV]
  std::vector<TH1F *> hEnergyDepositU; //! [MeV]
  std::vector<TH1F *> hEnergyDepositV; //! [MeV]

  std::vector<TH1F *> hMultiplicityT;  //! 
  std::vector<TH1F *> hMultiplicityP;  //! 
  std::vector<TH1F *> hMultiplicityQ;  //! 
  std::vector<TH1F *> hMultiplicityR;  //! 
  std::vector<TH1F *> hMultiplicityU;  //! 
  std::vector<TH1F *> hMultiplicityV;  //! 

  std::vector<TH1F *> hAdcT;           //!
  std::vector<TH1F *> hAdcP;           //!
  std::vector<TH1F *> hAdcQ;           //!
  std::vector<TH1F *> hAdcR;           //!
  std::vector<TH1F *> hAdcU;           //!
  std::vector<TH1F *> hAdcV;           //!

  /// TPC response (primarys, globas, vertexing, etc...) 

  std::vector<TH1F *> hNglobal;        //!
  std::vector<TH1F *> hNprimary;       //!
  std::vector<TH1F *> hNvertex;        //!
  std::vector<TH1F *> hZvertex;        //! 
  std::vector<TH1F *> hZvertexErr;     //! 
  std::vector<TH1F *> hRankVertex;     //!
  std::vector<TH1F *> hNtrackVertex;   //!
  std::vector<TH1F *> hNtrackVertexEE; //! 
  std::vector<TH1F *> hPTsumVertex;    //! 


  ClassDef(StEEmcQAMaker,1);

};

#endif
