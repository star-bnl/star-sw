/***********************************************************************
 *
 * $Id: StDecayMode.cc,v 2.0 2000/06/05 05:19:37 genevb Exp $
 * $Log: StDecayMode.cc,v $
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

  if (parent)
   {
    Int_t parentId = parent->geantId();
    StSPtrVecMcTrack& Daughters = mcVertex->daughters();
    StMcTrackIterator DTrackIt; 

    for (DTrackIt = Daughters.begin(); DTrackIt != Daughters.end(); DTrackIt++)
      { ID+=(*DTrackIt)->geantId(); ID2*=(*DTrackIt)->geantId();}
	

    if (parentId==11) { return KPlusProcess(ID);}
    if (parentId==12) { return KMinusProcess(ID);}
    if (parentId==16) { return KShortProcess(ID);}
    if (parentId==18) { return LambdaProcess(ID);}
    if (parentId==26) { return AntiLambdaProcess(ID);}

    if (parentId==23) { return XiProcess(ID);}
    if (parentId==31) { return AntiXiProcess(ID);}
    if (parentId==24) { return OmegaProcess(ID2);}
    if (parentId==32) { return AntiOmegaProcess(ID2);}
    
   }	
  return kWrongDecay;
}
//_____________________________________________________________________  
Int_t StDecayMode::KPlusProcess(Int_t ID) 
{
  if (ID == 9) return kKPlus2MuNu;
  if (ID == 15) return kKPlus2PiPlusPiZero;
  if (ID == 25) return kKPlus2PiPlusPiPlusPiMinus;
  if (ID == 13) return kKPlus2ENuPiZero;
  if (ID == 16) return kKPlus2MuNuPiZero;
  if (ID == 22) return kKPlus2PiPlusPiZeroPiZero;
  return kWrongDecay;
}

//_____________________________________________________________________
Int_t StDecayMode::KMinusProcess(Int_t ID)  
{
  if (ID == 10) return kKMinus2MuNu;
  if (ID == 16) return kKMinus2PiMinusPiZero;
  if (ID == 26) return kKMinus2PiPlusPiMinusPiMinus;
  if (ID == 14) return kKMinus2ENuPiZero;
  if (ID == 17) return kKMinus2MuNuPiZero;
  if (ID == 23) return kKMinus2PiMinusPiZeroPiZero;
  return kWrongDecay;
}

//____________________________________________________________________
Int_t StDecayMode::KShortProcess(Int_t ID)
{
  //  Int_t test =  kWrongDecay;
  if (ID == 17)  return  kKShort2PiPlusPiMinus;
  if (ID == 14) return kKShort2PiZeroPiZero;
  return kWrongDecay;
}

//____________________________________________________________________
Int_t StDecayMode::LambdaProcess(Int_t ID) 
{

  if (ID == 23) return kLambda2ProtonPiMinus;
  if (ID == 20) return kLambda2NeutronPiZero;
  return kWrongDecay;

}
//____________________________________________________________________
Int_t StDecayMode::AntiLambdaProcess(Int_t ID) 
{
  if (ID == 23) return kAntiLambda2AntiProtonPiPlus;
  if (ID == 32) return kAntiLambda2AntiNeutronPiZero;
  return kWrongDecay;

}

//____________________________________________________________________
Int_t StDecayMode::XiProcess(Int_t ID) 
{
  if (ID == 27) return kCascade2LambdaPiMinus;
  return kWrongDecay;

}
//____________________________________________________________________
Int_t StDecayMode::AntiXiProcess(Int_t ID) 
{
  if (ID == 34) return kAntiCascade2AntiLambdaPiPlus;
  return kWrongDecay;

}
//____________________________________________________________________
Int_t StDecayMode::OmegaProcess(Int_t ID) 
{
  if (ID == 216) return kOmega2LambdaKMinus;
  if (ID == 198) return kOmega2CascadePiMinus;
  if (ID == 161) return kOmega2CascadePiZero;
   
  return kWrongDecay;
}
//____________________________________________________________________
Int_t StDecayMode::AntiOmegaProcess(Int_t ID) 
{
  if (ID == 286) return kAntiOmega2AntiLambdaKPlus;
  if (ID == 240) return kAntiOmega2AntiCascadePiPlus;
  if (ID == 217) return kAntiOmega2AntiCascadePiZero;

  return kWrongDecay;
}
