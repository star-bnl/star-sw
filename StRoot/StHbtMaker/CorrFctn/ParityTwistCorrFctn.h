#ifndef ParityTwistCorrFctn_hh
#define ParityTwistCorrFctn_hh

#include "StHbtMaker/Infrastructure/StParityAnalysis.h"
#include "StHbtMaker/Infrastructure/StParityTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTagWriter.hh"
#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

class ParityTwistCorrFctn : public StHbtCorrFctn {
public:
  ParityTwistCorrFctn(const ParityTwistCorrFctn& ); // copy constructor
  ParityTwistCorrFctn();
  virtual ~ParityTwistCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void ParityCompute(ParityBuff*, ParityBuff*, int);

  virtual void Finish();
  ParityTwistCorrFctn* Clone();

  /// histograms
  /*
  StHbt1DHisto* SameTxxHisto();
  StHbt1DHisto* SameTxyHisto();
  StHbt1DHisto* SameTxzHisto();
  StHbt1DHisto* SameTyxHisto();
  StHbt1DHisto* SameTyyHisto();
  StHbt1DHisto* SameTyzHisto();
  StHbt1DHisto* SameTzxHisto();
  StHbt1DHisto* SameTzyHisto();
  */
  StHbt1DHisto* SameTzzHisto();

  /*
  StHbt1DHisto* MixedTxxHisto();
  StHbt1DHisto* MixedTxyHisto();
  StHbt1DHisto* MixedTxzHisto();
  StHbt1DHisto* MixedTyxHisto();
  StHbt1DHisto* MixedTyyHisto();
  StHbt1DHisto* MixedTyzHisto();
  StHbt1DHisto* MixedTzxHisto();
  StHbt1DHisto* MixedTzyHisto();
  */
  StHbt1DHisto* MixedTzzHisto();

private:

  /*  
  StHbt1DHisto* mSameTxx;  
  StHbt1DHisto* mSameTxy;  
  StHbt1DHisto* mSameTxz;  
  StHbt1DHisto* mSameTyx;  
  StHbt1DHisto* mSameTyy;  
  StHbt1DHisto* mSameTyz;  
  StHbt1DHisto* mSameTzx;  
  StHbt1DHisto* mSameTzy;  
  */
  StHbt1DHisto* mSameTzz;  

  /*
  StHbt1DHisto* mMixedTxx;  
  StHbt1DHisto* mMixedTxy;  
  StHbt1DHisto* mMixedTxz;  
  StHbt1DHisto* mMixedTyx;  
  StHbt1DHisto* mMixedTyy;  
  StHbt1DHisto* mMixedTyz;  
  StHbt1DHisto* mMixedTzx;  
  StHbt1DHisto* mMixedTzy;  
  */
  StHbt1DHisto* mMixedTzz;  

  StHbtTagWriter* mTagWriter;  //! <-- this is a singleton

#ifdef __ROOT__
  ClassDef(ParityTwistCorrFctn, 1)   
#endif 
};

/*
inline StHbt1DHisto* ParityTwistCorrFctn::SameTxxHisto(){return mSameTxx;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTxyHisto(){return mSameTxy;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTxzHisto(){return mSameTxz;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTyxHisto(){return mSameTyx;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTyyHisto(){return mSameTyy;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTyzHisto(){return mSameTyz;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTzxHisto(){return mSameTzx;}
inline StHbt1DHisto* ParityTwistCorrFctn::SameTzyHisto(){return mSameTzy;}
*/
inline StHbt1DHisto* ParityTwistCorrFctn::SameTzzHisto(){return mSameTzz;}

/*
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTxxHisto(){return mMixedTxx;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTxyHisto(){return mMixedTxy;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTxzHisto(){return mMixedTxz;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTyxHisto(){return mMixedTyx;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTyyHisto(){return mMixedTyy;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTyzHisto(){return mMixedTyz;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTzxHisto(){return mMixedTzx;}
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTzyHisto(){return mMixedTzy;}
*/
inline StHbt1DHisto* ParityTwistCorrFctn::MixedTzzHisto(){return mMixedTzz;}

// end parity histograms
inline ParityTwistCorrFctn* ParityTwistCorrFctn::Clone() { ParityTwistCorrFctn* c = new ParityTwistCorrFctn(*this); return c;}
inline ParityTwistCorrFctn::ParityTwistCorrFctn(const ParityTwistCorrFctn& fctn) :StHbtCorrFctn() {
    mTagWriter = StHbtTagWriter::Instance();  
}


#endif

