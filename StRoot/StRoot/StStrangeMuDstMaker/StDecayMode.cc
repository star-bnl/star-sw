/***********************************************************************
 *
 * StDecayMode: Singleton class for determining decay modes of MC vertices
 *
 ***********************************************************************/

#include "StDecayMode.hh"

StDecayMode* StDecayMode::mInstance = 0;

ClassImp(StDecayMode)

StDecayMode::StDecayMode() {}
StDecayMode::~StDecayMode() { mInstance=0; }

/*
    Note: reassign Gamma Id to -1
    Decay          Daughter Geant IDs    |sum / product (miss Nu)
11  K+ -> Mu Nu        5 4  (mu+)        |9 / 20 (5 / 5)
    K+ -> Pi+ Pi0      8 7               |15 / 56
    K+ -> Pi+ Pi+ Pi-  8 8 9             |25 / 576
    K+ -> E Nu Pi0     2 4 7 (positron)  |13 / 42 (9 / 14)
    K+ -> Mu Nu Pi0    5 4 7             |16 / 140 (12 / 35)
    K+ -> Pi+ Pi0 Pi0  8 7 7             |22 / 392

12  K- -> Mu Nu        6 4   (mu-)       |10 / 24 (6 / 6)
    K- -> Pi- Pi0      9 7               |16 / 63
    K- -> Pi+ Pi- Pi-  8 9 9             |26 / 648
    K- -> E Nu Pi0     3 4 7             |14 / 84 (10 / 21)
    K- -> Mu Nu Pi0    6 4 7             |17 / 168 (13 / 42)
    K- -> Pi- Pi0 Pi0  9 7 7             |23 / 441

10  K0l -> Pi+ Pi- Pi0    8 9 7          |24 / 504
    K0l -> Pi+ Mu- Nu     8 6 4          |18 / 192 (14 / 48)
    K0l -> Pi- Mu+ Nu     9 5 4          |18 / 180 (14 / 45)
    K0l -> Pi+ E Nu       8 3 4          |15 / 96 (11 / 24)
    K0l -> Pi- E Nu       9 2 4          |15 / 72 (11 / 18)
    K0l -> Pi- Pi+        9 8            |17 / 72

16  K0s -> Pi+ Pi-        8  9           |17 
    K0s -> Pi0 Pi0        7  7           |14
    K0s -> Pi+ Pi- Gamma  8  9 -1        |16 

 8  Pi+ -> Mu+ Nu         5 4            |9 (5)
    Pi+ -> Mu+ Nu Gamma   5 4 -1         |8 (4)
    Pi+ -> E Nu           2 4            |6 (2)

 9  Pi- -> Mu- Nu         6 4            |10 (6)
    Pi- -> Mu- Nu Gamma   6 4 -1         |9 (5)
    Pi- -> E Nu           3 4            |7 (3)

 5  Mu+ -> E Nu Nu        2 4 4          |10 (2,6)
    Mu+ -> E Nu Nu Gamma  2 4 4 -1       |9 (1,5)

 6  Mu- -> E Nu Nu        3 4 4          |11 (3,7)
    Mu- -> E Nu Nu Gamma  3 4 4 -1       |10 (2,6)

18  Lambda -> P Pi-        14 9          |23 / 126
    Lambda -> N Pi0        13 7          |20 / 91
    Lambda -> N Gamma      13 -1         |12 / -13
    Lambda -> P Pi- Gamma  14 9 -1       |22 / -126
    Lambda -> P E Nu       14 3 4        |21 / 168 (17 / 42)
    Lambda -> P Mu Nu      14 6 4        |20 / 336 (24 / 84)

26  AntiLambda -> AntiP Pi+        15 8       |23 / 120
    AntiLambda -> AntiN Pi0        25 7       |32 / 175
    AntiLambda -> AntiN Gamma      25 -1      |24 / -25
    AntiLambda -> AntiP Pi+ Gamma  15 8 -1    |22 / -120
    AntiLambda -> AntiP E Nu       15 2 4     |21 / 120 (17 / 30)
    AntiLambda -> AntiP Mu Nu      15 5 4     |20 / 300 (24 / 75)

23  Cascade     -> Lambda Pi-      18  9      |27  
31  AntiCascade -> AntiLambda Pi+  26  8      |34

24  Omega       -> Lambda  K-      18 12      |30 / 216
    Omega       -> Cascade0 Pi-    22  9      |31 / 198
    Omega       -> Cascade- Pi0    23  7      |30 / 161

32  AntiOmega   -> AntiLambda K+    26 11      |37 / 286
    AntiOmega   -> AntiCascade0 Pi+ 30  8      |38 / 240
    AntiOmega   -> AntiCascade+ Pi0 31  7      |38 / 217
 */
//____________________________________________________________________
Int_t StDecayMode::Process(StMcVertex* mcVertex) 
{
  Int_t ID = 0;
  Int_t ID2 = 1;
  if (!mcVertex->numberOfDaughters()) return kWrongDecay;    // No daughters
  if (mcVertex->geantProcess() != 5) return kWrongDecay; // Not a weak decay
  const StMcTrack* parent = mcVertex->parent();	

  if (parent) {
    Int_t parentId = parent->geantId();
    StSPtrVecMcTrack& Daughters = mcVertex->daughters();

    for (StMcTrackIterator DTrackIt = Daughters.begin();
                           DTrackIt != Daughters.end(); DTrackIt++) {
      Int_t daughterId = (*DTrackIt)->geantId();
      if (daughterId == 1) daughterId = -1;
      ID  += daughterId;
      ID2 *= daughterId;
    }
    
    switch (parentId) {
      case ( 5) : return MuPlusProcess(ID);
      case ( 6) : return MuMinusProcess(ID);
      case ( 8) : return PiPlusProcess(ID);
      case ( 9) : return PiMinusProcess(ID);
      case (10) : return KLongProcess(ID,ID2);
      case (11) : return KPlusProcess(ID2);
      case (12) : return KMinusProcess(ID2);
      case (16) : return KShortProcess(ID);
      case (18) : return LambdaProcess(ID2);
      case (26) : return AntiLambdaProcess(ID,ID2);
      case (23) : return XiProcess(ID);
      case (31) : return AntiXiProcess(ID);
      case (24) : return OmegaProcess(ID2);
      case (32) : return AntiOmegaProcess(ID2);
    }
  }	
  return kWrongDecay;
}
//_____________________________________________________________________  
Int_t StDecayMode::KPlusProcess(Int_t ID) 
{
  switch (ID) {
    case (  5) :
    case ( 20) : return kKPlus2MuNu;
    case ( 56) : return kKPlus2PiPlusPiZero;
    case (576) : return kKPlus2PiPlusPiPlusPiMinus;
    case ( 14) :
    case ( 42) : return kKPlus2ENuPiZero;
    case ( 35) :
    case (140) : return kKPlus2MuNuPiZero;
    case (392) : return kKPlus2PiPlusPiZeroPiZero;
    default    : return kWrongDecay;
  }
}

//_____________________________________________________________________
Int_t StDecayMode::KMinusProcess(Int_t ID)  
{
  switch (ID) {
    case (  6) :
    case ( 24) : return kKMinus2MuNu;
    case ( 63) : return kKMinus2PiMinusPiZero;
    case (648) : return kKMinus2PiPlusPiMinusPiMinus;
    case ( 21) :
    case ( 84) : return kKMinus2ENuPiZero;
    case ( 42) :
    case (168) : return kKMinus2MuNuPiZero;
    case (441) : return kKMinus2PiMinusPiZeroPiZero;
    default    : return kWrongDecay;
  }
}

//____________________________________________________________________
Int_t StDecayMode::KLongProcess(Int_t ID, Int_t ID2)
{
  switch (ID2) {
    case (504) : return kKLong2PiPlusPiZeroPiMinus;
    case ( 48) :
    case (192) : return kKLong2PiPlusMuNu;
    case ( 45) :
    case (180) : return kKLong2PiMinusMuNu;
    case ( 24) :
    case ( 96) : return kKLong2PiMinusENu;
    case ( 18) :
    case ( 72) : return ((ID==17) ? kKLong2PiPlusPiMinus : kKLong2PiPlusENu);
    default    : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::KShortProcess(Int_t ID)
{
  switch (ID) {
    case (17) : return kKShort2PiPlusPiMinus;
    case (14) : return kKShort2PiZeroPiZero;
    case (16) : return kKShort2PiPlusPiMinusGamma;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::PiPlusProcess(Int_t ID)
{
  switch (ID) {
    case (5) :
    case (9) : return kPiPlus2MuNu;
    case (4) :
    case (8) : return kPiPlus2MuNuGamma;
    case (2) :
    case (6) : return kPiPlus2ENu;
    default  : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::PiMinusProcess(Int_t ID)
{
  switch (ID) {
    case ( 6) :
    case (10) : return kPiMinus2MuNu;
    case ( 5) :
    case ( 9) : return kPiMinus2MuNuGamma;
    case ( 3) :
    case ( 7) : return kPiMinus2ENu;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::MuPlusProcess(Int_t ID)
{
  switch (ID) {
    case ( 2) :
    case ( 6) :
    case (10) : return kMuPlus2ENuNu;
    case ( 1) :
    case ( 5) :
    case ( 9) : return kMuPlus2ENuNuGamma;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::MuMinusProcess(Int_t ID)
{
  switch (ID) {
    case ( 3) :
    case ( 7) :
    case (11) : return kMuMinus2ENuNu;
    case ( 2) :
    case ( 6) :
    case (10) : return kMuMinus2ENuNuGamma;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::LambdaProcess(Int_t ID) 
{
  switch (ID) {
    case ( 126) : return kLambda2ProtonPiMinus;
    case (  91) : return kLambda2NeutronPiZero;
    case ( -13) : return kLambda2NeutronGamma;
    case (-126) : return kLambda2ProtonPiMinusGamma;
    case (  42) :
    case ( 168) : return kLambda2ProtonENu;
    case (  84) :
    case ( 336) : return kLambda2ProtonMuNu;
    default     : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::AntiLambdaProcess(Int_t ID, Int_t ID2) 
{
  switch (ID) {
    case (23) : return kAntiLambda2AntiProtonPiPlus;
    case (32) : return kAntiLambda2AntiNeutronPiZero;
    case (24) : if (ID2 == -25) return kAntiLambda2AntiNeutronGamma;
    case (20) : return kAntiLambda2AntiProtonMuNu;
    case (22) : return kAntiLambda2AntiProtonPiPlusGamma;
    case (17) :
    case (21) : return kAntiLambda2AntiProtonENu;
    default   : return kWrongDecay;
  }
}

//____________________________________________________________________
Int_t StDecayMode::XiProcess(Int_t ID) 
{
  switch (ID) {
    case (27) : return kCascade2LambdaPiMinus;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::AntiXiProcess(Int_t ID) 
{
  switch (ID) {
    case (34) : return kAntiCascade2AntiLambdaPiPlus;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::OmegaProcess(Int_t ID) 
{
  switch (ID) {
    case (216) : return kOmega2LambdaKMinus;
    case (198) : return kOmega2CascadePiMinus;
    case (161) : return kOmega2CascadePiZero;
    default    : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::AntiOmegaProcess(Int_t ID) 
{
  switch (ID) {
    case (286) : return kAntiOmega2AntiLambdaKPlus;
    case (240) : return kAntiOmega2AntiCascadePiPlus;
    case (217) : return kAntiOmega2AntiCascadePiZero;
    default    : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::ParentCharge(Int_t mode) 
{
  switch (mode) {
    case (kKPlus2MuNu)                   :
    case (kKPlus2PiPlusPiZero)           :
    case (kKPlus2PiPlusPiPlusPiMinus)    :
    case (kKPlus2ENuPiZero)              :
    case (kKPlus2MuNuPiZero)             :
    case (kKPlus2PiPlusPiZeroPiZero)     :
    case (kPiPlus2MuNu)                  :
    case (kPiPlus2MuNuGamma)             :
    case (kPiPlus2ENu)                   :
    case (kMuPlus2ENuNu)                 :
    case (kMuPlus2ENuNuGamma)            :
    case (kAntiCascade2AntiLambdaPiPlus) :
    case (kAntiOmega2AntiLambdaKPlus)    :
    case (kAntiOmega2AntiCascadePiPlus)  : return  1;
    case (kKMinus2MuNu)                  :
    case (kKMinus2PiMinusPiZero)         :
    case (kKMinus2PiPlusPiMinusPiMinus)  :
    case (kKMinus2ENuPiZero)             :
    case (kKMinus2MuNuPiZero)            :
    case (kKMinus2PiMinusPiZeroPiZero)   :
    case (kPiMinus2MuNu)                 :
    case (kPiMinus2MuNuGamma)            :
    case (kPiMinus2ENu)                  :
    case (kMuMinus2ENuNu)                :
    case (kMuMinus2ENuNuGamma)           :
    case (kCascade2LambdaPiMinus)        :
    case (kOmega2LambdaKMinus)           :
    case (kOmega2CascadePiMinus)         : return -1;
    default : {}
  }
  return 0;                                                                     
}

/***********************************************************************
 *
 * $Id: StDecayMode.cc,v 3.3 2002/05/29 19:08:44 genevb Exp $
 * $Log: StDecayMode.cc,v $
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
 * Revision 2.0  2000/06/05 05:19:37  genevb
 * New version of Strangeness micro DST package
 *
 ***********************************************************************/
