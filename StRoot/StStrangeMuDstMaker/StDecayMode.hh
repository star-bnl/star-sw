/*!
  \class StDecayMode
  
  StDecayMode: Singleton class for determining decay modes of MC vertices
  \code
    if (StDecayMode::Instance()->Process(vtx) == kLambda2ProtonPiMinus) {...}
  \endcode
  \sa decayModeType

*/

/*! \file StDecayMode.hh */

#ifndef  STAR_StDecayMode
#define  STAR_StDecayMode
#include "StMcVertex.hh"
#include "StMcTrack.hh"

class StMcVertex;
class StMcTrack;

/// Enumeration of strange decay modes
enum decayModeType {
  kWrongDecay                           =  0,

  kKPlus2MuNu                           = 10,
  kKPlus2PiPlusPiZero                   = 11,             
  kKPlus2PiPlusPiPlusPiMinus            = 12,
  kKPlus2ENuPiZero                      = 13,
  kKPlus2MuNuPiZero                     = 14,
  kKPlus2PiPlusPiZeroPiZero             = 15,

  kKMinus2MuNu                          = 20,
  kKMinus2PiMinusPiZero                 = 21, 
  kKMinus2PiPlusPiMinusPiMinus          = 22,
  kKMinus2ENuPiZero                     = 23,
  kKMinus2MuNuPiZero                    = 24,
  kKMinus2PiMinusPiZeroPiZero           = 25,

  kKShort2PiPlusPiMinus                 = 30,
  kKShort2PiZeroPiZero                  = 31,
  kKShort2PiPlusPiMinusGamma            = 32,

  kLambda2ProtonPiMinus                 = 40,
  kLambda2NeutronPiZero                 = 41,
  kLambda2NeutronGamma                  = 42,
  kLambda2ProtonPiMinusGamma            = 43,
  kLambda2ProtonENu                     = 44,
  kLambda2ProtonMuNu                    = 45,

  kAntiLambda2AntiProtonPiPlus          = 50,
  kAntiLambda2AntiNeutronPiZero         = 51,
  kAntiLambda2AntiNeutronGamma          = 52,
  kAntiLambda2AntiProtonPiPlusGamma     = 53,
  kAntiLambda2AntiProtonENu             = 54,
  kAntiLambda2AntiProtonMuNu            = 55,

  kCascade2LambdaPiMinus                = 60,

  kAntiCascade2AntiLambdaPiPlus         = 70,

  kOmega2LambdaKMinus                   = 80,
  kOmega2CascadePiMinus                 = 81,
  kOmega2CascadePiZero                  = 82,

  kAntiOmega2AntiLambdaKPlus            = 90,
  kAntiOmega2AntiCascadePiPlus          = 91,
  kAntiOmega2AntiCascadePiZero          = 92,

  kKLong2PiPlusPiZeroPiMinus            =100,
  kKLong2PiPlusMuNu                     =101,
  kKLong2PiMinusMuNu                    =102,
  kKLong2PiMinusENu                     =103,
  kKLong2PiPlusENu                      =104,
  kKLong2PiPlusPiMinus                  =105,

  kPiPlus2MuNu                          =110,
  kPiPlus2MuNuGamma                     =111,
  kPiPlus2ENu                           =112,

  kPiMinus2MuNu                         =120,
  kPiMinus2MuNuGamma                    =121,
  kPiMinus2ENu                          =122,

  kMuPlus2ENuNu                         =130,
  kMuPlus2ENuNuGamma                    =131,

  kMuMinus2ENuNu                        =140,
  kMuMinus2ENuNuGamma                   =141,

  kNoDecay                              =10000
};


class StDecayMode {
public:


  StDecayMode();
  virtual ~StDecayMode();
  static StDecayMode* Instance();

  /// Returns the charge of the parent particle for a given decay process,
  /// where the decay modes are enumerated in ::decayModeType
  Int_t ParentCharge(Int_t mode);

  /// Returns the decay process for the vertex,
  /// where the decay modes are enumerated in ::decayModeType
  Int_t Process(StMcVertex* mcVertex);

  /// @name Process helper functions
  //@{
  Int_t KPlusProcess(Int_t ID); 
  Int_t KMinusProcess(Int_t ID);  
  Int_t KLongProcess(Int_t ID, Int_t ID2);
  Int_t KShortProcess(Int_t ID);
  Int_t PiPlusProcess(Int_t ID);
  Int_t PiMinusProcess(Int_t ID);
  Int_t MuPlusProcess(Int_t ID);
  Int_t MuMinusProcess(Int_t ID);
  Int_t LambdaProcess(Int_t ID);
  Int_t AntiLambdaProcess(Int_t ID, Int_t ID2); 
  Int_t XiProcess(Int_t ID); 
  Int_t AntiXiProcess(Int_t ID); 
  Int_t OmegaProcess(Int_t ID); 
  Int_t AntiOmegaProcess(Int_t ID); 
  //@}

private:
  static StDecayMode* mInstance;
  ClassDef(StDecayMode,0)

};

inline StDecayMode* StDecayMode::Instance()
       { if (!mInstance) mInstance = new StDecayMode();
         return mInstance; }

#endif

/***********************************************************************
 *
 * $Id: StDecayMode.hh,v 3.4 2003/05/30 21:20:18 genevb Exp $
 * $Log: StDecayMode.hh,v $
 * Revision 3.4  2003/05/30 21:20:18  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.3  2002/05/29 19:08:44  genevb
 * Some additional decay modes, better organization of enumeration
 *
 * Revision 3.2  2002/04/30 16:02:47  genevb
 * Common muDst, improved MC code, better kinks, StrangeCuts now a branch
 *
 * Revision 3.1  2001/05/04 20:15:13  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.0  2000/07/14 12:56:47  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.1  2000/06/09 22:17:09  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/05 05:19:38  genevb
 * New version of Strangeness micro DST package
 *
 ***********************************************************************/
