/***********************************************************************
 *
 * $Id: StDecayMode.hh,v 2.1 2000/06/09 22:17:09 genevb Exp $
 * $Log: StDecayMode.hh,v $
 * Revision 2.1  2000/06/09 22:17:09  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/05 05:19:38  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Singleton class for determining decay modes of MC vertices
 *
 ***********************************************************************/
#ifndef  STAR_StDecayMode
#define  STAR_StDecayMode
#include "StMcVertex.hh"
#include "StMcTrack.hh"

class StMcVertex;
class StMcTrack;

enum decayModeType {
  kWrongDecay                   = 0,
  kKPlus2MuNu                   = 1,
  kKPlus2PiPlusPiZero           = 2,             
  kKPlus2PiPlusPiPlusPiMinus    = 3,
  kKPlus2ENuPiZero              = 4,
  kKPlus2MuNuPiZero             = 5,
  kKPlus2PiPlusPiZeroPiZero     = 6,
  kKMinus2MuNu                  = 7,
  kKMinus2PiMinusPiZero         = 8, 
  kKMinus2PiPlusPiMinusPiMinus  = 9,
  kKMinus2ENuPiZero             =10,
  kKMinus2MuNuPiZero            =11,
  kKMinus2PiMinusPiZeroPiZero   =12,
  kKShort2PiPlusPiMinus         =13,
  kKShort2PiZeroPiZero          =14,
  kLambda2ProtonPiMinus         =15,
  kLambda2NeutronPiZero         =16,
  kAntiLambda2AntiProtonPiPlus  =17,
  kAntiLambda2AntiNeutronPiZero =18,
  kCascade2LambdaPiMinus        =19,
  kAntiCascade2AntiLambdaPiPlus =20,
  kOmega2LambdaKMinus           =21,
  kOmega2CascadePiMinus         =22,
  kOmega2CascadePiZero          =23,
  kAntiOmega2AntiLambdaKPlus    =24,
  kAntiOmega2AntiCascadePiPlus  =25,
  kAntiOmega2AntiCascadePiZero  =26
};


class StDecayMode {
public:


  StDecayMode();
  virtual ~StDecayMode();
  static StDecayMode* Instance();
  Int_t Process(StMcVertex* mcVertex);
  Int_t KPlusProcess(Int_t ID); 
  Int_t KMinusProcess(Int_t ID);  
  Int_t KShortProcess(Int_t ID);
  Int_t LambdaProcess(Int_t ID);
  Int_t AntiLambdaProcess(Int_t ID); 
  Int_t XiProcess(Int_t ID); 
  Int_t AntiXiProcess(Int_t ID); 
  Int_t OmegaProcess(Int_t ID); 
  Int_t AntiOmegaProcess(Int_t ID); 

private:
  static StDecayMode* mInstance;
  ClassDef(StDecayMode,1)

};

inline StDecayMode* StDecayMode::Instance()
       { if (!mInstance) mInstance = new StDecayMode();
         return mInstance; }

#endif
