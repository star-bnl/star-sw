/***********************************************************************
 *
 * $Id: StDecayMode.cc,v 2.1 2000/06/09 22:17:09 genevb Exp $
 * $Log: StDecayMode.cc,v $
 * Revision 2.1  2000/06/09 22:17:09  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
 * Revision 2.0  2000/06/05 05:19:37  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Singleton class for determining decay modes of MC vertices
 *
 ***********************************************************************/
#include "StDecayMode.hh"

StDecayMode* StDecayMode::mInstance = 0;

ClassImp(StDecayMode)

StDecayMode::StDecayMode() {}
StDecayMode::~StDecayMode() { mInstance=0; }

/*
11  K+ -> Mu Nu        5 4  (mu+)        |9
    K+ -> Pi+ Pi0      8 7               |15
    K+ -> Pi+ Pi+ Pi-  8 8 9             |25
    K+ -> E Nu Pi0     2 4 7 (positron)  |13
    K+ -> Mu Nu Pi0    5 4 7             |16 
    K+ -> Pi+ Pi0 Pi0  8 7 7             |22 

12  K- -> Mu Nu        6 4   (mu-)       |10
    K- -> Pi- Pi0      9 7               |16
    K- -> Pi+ Pi- Pi-  8 9 9             |26
    K- -> E Nu Pi0     3 4 7             |14
    K- -> Mu Nu Pi0    6 4 7             |17
    K- -> Pi- Pi0 Pi0  9 7 7             |23 

16  K0 -> Pi+ Pi-    8  9                |17 
    K0 -> Pi0 Pi0    7  7                |14

18  Lambda -> P Pi-  14 9                |23  
    Lambda -> N Pi0  13 7                |20

26  AntiLambda -> AntiP Pi+  15 8        |23
    AntiLambda -> AntiN Pi0  25 7        |32



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
  if (!mcVertex->numberOfDaughters()) return kWrongDecay;
  const StMcTrack* parent = mcVertex->parent();	

  if (parent) {
    Int_t parentId = parent->geantId();
    StSPtrVecMcTrack& Daughters = mcVertex->daughters();

    for (StMcTrackIterator DTrackIt = Daughters.begin();
                           DTrackIt != Daughters.end(); DTrackIt++) {
      Int_t daughterId = (*DTrackIt)->geantId();
      ID  += daughterId;
      ID2 *= daughterId;
    }
    
    switch (parentId) {
      case (11) : return KPlusProcess(ID);
      case (12) : return KMinusProcess(ID);
      case (16) : return KShortProcess(ID);
      case (18) : return LambdaProcess(ID);
      case (26) : return AntiLambdaProcess(ID);
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
    case (9)  : return kKPlus2MuNu;
    case (15) : return kKPlus2PiPlusPiZero;
    case (25) : return kKPlus2PiPlusPiPlusPiMinus;
    case (13) : return kKPlus2ENuPiZero;
    case (16) : return kKPlus2MuNuPiZero;
    case (22) : return kKPlus2PiPlusPiZeroPiZero;
    default   : return kWrongDecay;
  }
}

//_____________________________________________________________________
Int_t StDecayMode::KMinusProcess(Int_t ID)  
{
  switch (ID) {
    case (10) : return kKMinus2MuNu;
    case (16) : return kKMinus2PiMinusPiZero;
    case (26) : return kKMinus2PiPlusPiMinusPiMinus;
    case (14) : return kKMinus2ENuPiZero;
    case (17) : return kKMinus2MuNuPiZero;
    case (23) : return kKMinus2PiMinusPiZeroPiZero;
    default   : return kWrongDecay;
  }
}

//____________________________________________________________________
Int_t StDecayMode::KShortProcess(Int_t ID)
{
  //  Int_t test =  kWrongDecay;
  switch (ID) {
    case (17) : return kKShort2PiPlusPiMinus;
    case (14) : return kKShort2PiZeroPiZero;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::LambdaProcess(Int_t ID) 
{
  switch (ID) {
    case (23) : return kLambda2ProtonPiMinus;
    case (20) : return kLambda2NeutronPiZero;
    default   : return kWrongDecay;
  }
}
//____________________________________________________________________
Int_t StDecayMode::AntiLambdaProcess(Int_t ID) 
{
  switch (ID) {
    case (23) : return kAntiLambda2AntiProtonPiPlus;
    case (32) : return kAntiLambda2AntiNeutronPiZero;
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
