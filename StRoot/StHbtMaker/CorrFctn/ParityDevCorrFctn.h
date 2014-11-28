#ifndef ParityDevCorrFctn_hh
#define ParityDevCorrFctn_hh

#include "StHbtMaker/Infrastructure/StParityAnalysis.h"
#include "StHbtMaker/Infrastructure/StParityTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTagWriter.hh"
#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

class ParityDevCorrFctn : public StHbtCorrFctn {
public:
  ParityDevCorrFctn(const ParityDevCorrFctn& ); // copy constructor
  ParityDevCorrFctn();
  virtual ~ParityDevCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  void ParityCompute(ParityBuff*, ParityBuff*, int);

  virtual void Finish();
  ParityDevCorrFctn* Clone();

  /// histograms
  StHbt1DHisto* SameKMiHistox();
  StHbt1DHisto* SameKMiHistoy();
  StHbt1DHisto* SameKMiHistoz();
  StHbt1DHisto* SameJcHistox();
  StHbt1DHisto* SameJcHistoy();
  StHbt1DHisto* SameJcHistoz();
  StHbt1DHisto* SameJcKtHisto();
  StHbt1DHisto* SameKtwistKtHisto();

  StHbt1DHisto* MixedKMiHistox();
  StHbt1DHisto* MixedKMiHistoy();
  StHbt1DHisto* MixedKMiHistoz();
  StHbt1DHisto* MixedJcHistox();
  StHbt1DHisto* MixedJcHistoy();
  StHbt1DHisto* MixedJcHistoz();
  StHbt1DHisto* MixedJcKtHisto();
  StHbt1DHisto* MixedKtwistKtHisto();

  StHbt1DHisto* JcKtBinomialHisto();
  StHbt1DHisto* NumPairsBinHisto();

private:

  StHbt1DHisto* mKMiSamex;  
  StHbt1DHisto* mKMiSamey;  
  StHbt1DHisto* mKMiSamez;  
  StHbt1DHisto* mJcSamex;  
  StHbt1DHisto* mJcSamey;  
  StHbt1DHisto* mJcSamez;  
  StHbt1DHisto* mJcKtSame;  
  StHbt1DHisto* mKtwistKtSame;  

  StHbt1DHisto* mKMiMixedx;  
  StHbt1DHisto* mKMiMixedy;  
  StHbt1DHisto* mKMiMixedz;  
  StHbt1DHisto* mJcMixedx;  
  StHbt1DHisto* mJcMixedy;  
  StHbt1DHisto* mJcMixedz;  
  StHbt1DHisto* mJcKtMixed;  
  StHbt1DHisto* mKtwistKtMixed;  
 
  StHbt1DHisto* mJcKtBinomial;  
  StHbt1DHisto* mNumPairsBin;  

  StHbtTagWriter* mTagWriter;  //! <-- this is a singleton

#ifdef __ROOT__
  ClassDef(ParityDevCorrFctn, 1)   
#endif 
};


// histograms for parity analysis
inline StHbt1DHisto* ParityDevCorrFctn::SameKMiHistox(){return mKMiSamex;}
inline StHbt1DHisto* ParityDevCorrFctn::SameKMiHistoy(){return mKMiSamey;}
inline StHbt1DHisto* ParityDevCorrFctn::SameKMiHistoz(){return mKMiSamez;}
inline StHbt1DHisto* ParityDevCorrFctn::SameJcHistox(){return mJcSamex;}
inline StHbt1DHisto* ParityDevCorrFctn::SameJcHistoy(){return mJcSamey;}
inline StHbt1DHisto* ParityDevCorrFctn::SameJcHistoz(){return mJcSamez;}
inline StHbt1DHisto* ParityDevCorrFctn::SameJcKtHisto(){return mJcKtSame;}
inline StHbt1DHisto* ParityDevCorrFctn::SameKtwistKtHisto(){return mKtwistKtSame;}

inline StHbt1DHisto* ParityDevCorrFctn::MixedKMiHistox(){return mKMiMixedx;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedKMiHistoy(){return mKMiMixedy;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedKMiHistoz(){return mKMiMixedz;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedJcHistox(){return mJcMixedx;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedJcHistoy(){return mJcMixedy;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedJcHistoz(){return mJcMixedz;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedJcKtHisto(){return mJcKtMixed;}
inline StHbt1DHisto* ParityDevCorrFctn::MixedKtwistKtHisto(){return mKtwistKtMixed;}

inline StHbt1DHisto* ParityDevCorrFctn::JcKtBinomialHisto(){return mJcKtBinomial;}
inline StHbt1DHisto* ParityDevCorrFctn::NumPairsBinHisto(){return mNumPairsBin;}

// end parity histograms
inline ParityDevCorrFctn* ParityDevCorrFctn::Clone() { ParityDevCorrFctn* c = new ParityDevCorrFctn(*this); return c;}
inline ParityDevCorrFctn::ParityDevCorrFctn(const ParityDevCorrFctn& fctn) :StHbtCorrFctn() {
    mTagWriter = StHbtTagWriter::Instance();  
}

#endif

