// $Id: strangeFormulas.C,v 1.1 2000/04/07 16:14:53 genevb Exp $
// $Log: strangeFormulas.C,v $
// Revision 1.1  2000/04/07 16:14:53  genevb
// Introduction of strangeFormulas.C
//
//
//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  creates formulas for use with strangeness micro DST
// usage: First parameter (char string 'doing') should include
//          one or more of the strings "v0", "xi", and/or "kink"
//          to indicate which formulas should be loaded in.
//          Capitalization and delimeters don't matter - it's simply
//          a search to match the substring.
//        Second parameter is either a pointer to a strangeness
//          micro DST tree, a pointer to a TFile containing such
//          a tree (tree name assumed to be "StrangeMuDst"), or
//          the name of such a file. In the latter two cases, a
//          pointer to the strangeness TTree is returned.
// examples:
//        .x strangeFormulas.C("V0,xi","myfile.root");
//          Opens a file named "myfile.root", loads the formulas needed
//          for V0's and Xi's, and returns a pointer to the TTree.
//        .x strangeFormulas.C("Xi",myTreePtr);
//          Loads only the formulas needed for Xi's, and returns the
//          number of new formulas loaded into Root.
//        .x strangeFormulas.C("kinkV0",myFilePtr);
//          Loads only the formulas needed for V0's and Kinks, and
//          returns a pointer to the tree named "StrangeMuDst" in the
//          file pointed to by myFilePtr.
//======================================================
//
// Should be careful to optimize formulas for number of calls to
// other formulas. TTreeFormula/TFormula is not smart enough to save
// values to repeat them, and calculates the full thing twice, but
// has a limit of 50 values one can calculate.
//
// For example, "a()+a()+a()" repeats everything necessary to find
// "a()" three times. If "a()" requires 20 values to calculate, then
// "a()+a()+a()" will fail, needing 60 values. Much better to write
// something like "3*a()".
//

TTree* strangeFormulas(const char* doing="", const char* fname=0);
TTree* strangeFormulas(const char* doing="", TFile* fptr=0);
Int_t strangeFormulas(const char* doing="", TTree* tree);

TTree* strangeFormulas(const char* doing, const char* fname) {
  TFile *a1;
  if (!fname) {
    a1 = new TFile("evMuDst.root");
  } else {
    a1 = new TFile(fname);
  }
  TTree* m1 = strangeFormulas(doing,a1);
  return m1;
}

TTree* strangeFormulas(const char* doing, TFile* fptr) {
  if (!fptr) return 0;
  TTree* m1 = (TTree*) fptr->Get("StrangeMuDst");
  strangeFormulas(doing,m1);
  return m1;
}

Int_t strangeFormulas(const char* doing, TTree* tree) {
  if (!(gROOT->GetClass("TTreeFormula"))) {
    gSystem->Load("libProof");
    gSystem->Load("libTreePlayer");
  }
  if (((!tree) || (!doing)) || (!(*doing))) return;
  size_t len = strlen(doing);
  char* str = new char[len+1];
  str[len] = 0;
  for (size_t i=0; i<len; i++) {
    str[i] = tolower(doing[i]);
  }

  TFormula *f0=0;
  TTreeFormula *f1=0;
  Int_t initialFormulas = gROOT->GetListOfFunctions()->GetSize();
  
  // Mass formulas
  f0 = new TFormula("mLambda","1.11563");
  f0 = new TFormula("mAntiLambda","1.11563");
  f0 = new TFormula("mK0Short","0.497671");
  f0 = new TFormula("mProton","0.938272");
  f0 = new TFormula("mAntiProton","0.938272");
  f0 = new TFormula("mPiPlus","0.139568");
  f0 = new TFormula("mPiMinus","0.139568");
  f0 = new TFormula("mKaonMinus","0.493646");
  
  // Event
  f1 = new TTreeFormula("Event.run()",
    "Event.mRun",tree);
  gROOT->GetListOfFunctions()->Add(f1);
  f1 = new TTreeFormula("Event.event()",
    "Event.mEvent",tree);
  gROOT->GetListOfFunctions()->Add(f1);
  f1 = new TTreeFormula("Event.primaryVertexX()",
    "Event.mPrimaryVertexX",tree);
  gROOT->GetListOfFunctions()->Add(f1);
  f1 = new TTreeFormula("Event.primaryVertexY()",
    "Event.mPrimaryVertexY",tree);
  gROOT->GetListOfFunctions()->Add(f1);
  f1 = new TTreeFormula("Event.primaryVertexZ()",
    "Event.mPrimaryVertexZ",tree);
  gROOT->GetListOfFunctions()->Add(f1);
  
  // V0
  if (strstr(str,"v0")) {
  
    f1 = new TTreeFormula("V0.decayLengthV0()",
      "sqrt(sq(V0.mDecayVertexV0X-Event.mPrimaryVertexX)+sq(V0.mDecayVertexV0Y-Event.mPrimaryVertexY)+sq(V0.mDecayVertexV0Z-Event.mPrimaryVertexZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.decayVertexV0X()",
      "V0.mDecayVertexV0X",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.decayVertexV0Y()",
      "V0.mDecayVertexV0Y",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.decayVertexV0Z()",
      "V0.mDecayVertexV0Z",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.dcaV0Daughters()",
      "V0.mDcaV0Daughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.dcaV0Daughters()",
      "V0.mDcaV0Daughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.dcaV0Daughters()",
      "V0.mDcaV0Daughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.dcaV0ToPrimVertex()",
      "V0.mDcaV0ToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.dcaPosToPrimVertex()",
      "V0.mDcaPosToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.dcaNegToPrimVertex()",
      "V0.mDcaNegToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momPosX()",
      "V0.mMomPosX",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momPosY()",
      "V0.mMomPosY",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momPosZ()",
      "V0.mMomPosZ",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momNegX()",
      "V0.mMomNegX",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momNegY()",
      "V0.mMomNegY",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momNegZ()",
      "V0.mMomNegZ",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.Ptot2Pos()",
      "(sq(V0.mMomPosX)+sq(V0.mMomPosY)+sq(V0.mMomPosZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.Ptot2Neg()",
      "(sq(V0.mMomNegX)+sq(V0.mMomNegY)+sq(V0.mMomNegZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momV0X()",
      "(V0.mMomPosX+V0.mMomNegX)",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momV0Y()",
      "(V0.mMomPosY+V0.mMomNegY)",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.momV0Z()",
      "(V0.mMomPosZ+V0.mMomNegZ)",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.Pt2V0()",
      "(sq(V0.momV0X())+sq(V0.momV0Y()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.Ptot2V0()",
      "(V0.Pt2V0()+sq(V0.momV0Z()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following momentum component formulas use 15 values:
    f1 = new TTreeFormula("V0.MomPosAlongV0()",
      "(((V0.mMomPosX*V0.momV0X())+(V0.mMomPosY*V0.momV0Y())+(V0.mMomPosZ*V0.momV0Z()))/sqrt(V0.Ptot2V0()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.MomNegAlongV0()",
      "(((V0.mMomNegX*V0.momV0X())+(V0.mMomNegY*V0.momV0Y())+(V0.mMomNegZ*V0.momV0Z()))/sqrt(V0.Ptot2V0()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    f1 = new TTreeFormula("V0.alphaV0()",
    //  "((V0.MomPosAlongV0()-V0.MomNegAlongV0())/(V0.MomPosAlongV0()+V0.MomNegAlongV0()))",tree);
    // The above fails for exceeding 50 value limit. The following uses 30:
      "1.-(2./(1.+(V0.MomPosAlongV0()/V0.MomNegAlongV0())))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    // The following formula uses 18 values:
    f1 = new TTreeFormula("V0.ptArmV0()",
      "sqrt(V0.Ptot2Pos()-sq(V0.MomPosAlongV0()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following energy formulas use 6 values:
    f1 = new TTreeFormula("V0.eLambda()",
      "sqrt(V0.Ptot2V0()+sq(mLambda))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.eK0Short()",
      "sqrt(V0.Ptot2V0()+sq(mK0Short))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ePosProton()",
      "sqrt(V0.Ptot2Pos()+sq(mProton))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.eNegAntiProton()",
      "sqrt(V0.Ptot2Neg()+sq(mAntiProton))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ePosPion()",
      "sqrt(V0.Ptot2Pos()+sq(mPiPlus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.eNegPion()",
      "sqrt(V0.Ptot2Neg()+sq(mPiMinus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following mass formulas use 12 values:
    f1 = new TTreeFormula("V0.massLambda()",
      "sqrt(sq(V0.ePosProton()+V0.eNegPion())-V0.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.massAntiLambda()",
      "sqrt(sq(V0.eNegAntiProton()+V0.ePosPion())-V0.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.massK0Short()",
      "sqrt(sq(V0.ePosPion()+V0.eNegPion())-V0.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    
    // The following rapidity formulas use 8 values:
    f1 = new TTreeFormula("V0.rapLambda()",
    //  "0.5*log((V0.eLambda()+V0.momV0Z())/(V0.eLambda()-V0.momV0Z()))",tree);
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((V0.eLambda()/V0.momV0Z())-1.)))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.rapK0Short()",
    //  "0.5*log((V0.eK0Short()+V0.momV0Z())/(V0.eK0Short()-V0.momV0Z()))",tree);
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((V0.eK0Short()/V0.momV0Z())-1.)))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following cTau formulas use 24 values:
    f1 = new TTreeFormula("V0.cTauLambda()",
      "V0.massLambda()*V0.decayLengthV0()/sqrt(V0.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.cTauK0Short()",
      "V0.massK0Short()*V0.decayLengthV0()/sqrt(V0.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    
    f1 = new TTreeFormula("V0.ptPos()",
      "sqrt(sq(V0.mMomPosX)+sq(V0.mMomPosY))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ptotPos()",
      "sqrt(V0.Ptot2Pos())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ptNeg()",
      "sqrt(sq(V0.mMomNegX)+sq(V0.mMomNegY))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ptotNeg()",
      "sqrt(V0.Ptot2Neg())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ptV0()",
      "sqrt(V0.Pt2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("V0.ptotV0()",
      "sqrt(V0.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
  }
  
  // Xi
  if (strstr(str,"xi")) {
  
    f0 = new TFormula("mXiMinus","1.32133");
    f0 = new TFormula("mOmegaMinus","1.67243");

    // First, the Xi's get all the same functions that the V0's get...
    f1 = new TTreeFormula("Xi.decayLengthV0()",
      "sqrt(sq(Xi.mDecayVertexV0X-Event.mPrimaryVertexX)+sq(Xi.mDecayVertexV0Y-Event.mPrimaryVertexY)+sq(Xi.mDecayVertexV0Z-Event.mPrimaryVertexZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayVertexV0X()",
      "Xi.mDecayVertexV0X",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayVertexV0Y()",
      "Xi.mDecayVertexV0Y",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayVertexV0Z()",
      "Xi.mDecayVertexV0Z",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaV0Daughters()",
      "Xi.mDcaV0Daughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaV0Daughters()",
      "Xi.mDcaV0Daughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaV0Daughters()",
      "Xi.mDcaV0Daughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaV0ToPrimVertex()",
      "Xi.mDcaV0ToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaPosToPrimVertex()",
      "Xi.mDcaPosToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaNegToPrimVertex()",
      "Xi.mDcaNegToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momPosX()",
      "Xi.mMomPosX",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momPosY()",
      "Xi.mMomPosY",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momPosZ()",
      "Xi.mMomPosZ",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momNegX()",
      "Xi.mMomNegX",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momNegY()",
      "Xi.mMomNegY",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momNegZ()",
      "Xi.mMomNegZ",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Ptot2Pos()",
      "(sq(Xi.mMomPosX)+sq(Xi.mMomPosY)+sq(Xi.mMomPosZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Ptot2Neg()",
      "(sq(Xi.mMomNegX)+sq(Xi.mMomNegY)+sq(Xi.mMomNegZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momV0X()",
      "(Xi.mMomPosX+Xi.mMomNegX)",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momV0Y()",
      "(Xi.mMomPosY+Xi.mMomNegY)",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momV0Z()",
      "(Xi.mMomPosZ+Xi.mMomNegZ)",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Pt2V0()",
      "(sq(Xi.momV0X())+sq(Xi.momV0Y()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Ptot2V0()",
      "(Xi.Pt2V0()+sq(Xi.momV0Z()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following momentum component formulas use 15 values:
    f1 = new TTreeFormula("Xi.MomPosAlongV0()",
      "(((Xi.mMomPosX*Xi.momV0X())+(Xi.mMomPosY*Xi.momV0Y())+(Xi.mMomPosZ*Xi.momV0Z()))/sqrt(Xi.Ptot2V0()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.MomNegAlongV0()",
      "(((Xi.mMomNegX*Xi.momV0X())+(Xi.mMomNegY*Xi.momV0Y())+(Xi.mMomNegZ*Xi.momV0Z()))/sqrt(Xi.Ptot2V0()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    f1 = new TTreeFormula("Xi.alphaV0()",
    //  "((Xi.MomPosAlongV0()-Xi.MomNegAlongV0())/(Xi.MomPosAlongV0()+Xi.MomNegAlongV0()))",tree);
    // The above fails for exceeding 50 value limit. The following uses 30:
      "1.-(2./(1.+(Xi.MomPosAlongV0()/Xi.MomNegAlongV0())))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    // The following formula uses 18 values:
    f1 = new TTreeFormula("Xi.ptArmV0()",
      "sqrt(Xi.Ptot2Pos()-sq(Xi.MomPosAlongV0()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following energy formulas use 6 values:
    f1 = new TTreeFormula("Xi.eLambda()",
      "sqrt(Xi.Ptot2V0()+sq(mLambda))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.eK0Short()",
      "sqrt(Xi.Ptot2V0()+sq(mK0Short))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ePosProton()",
      "sqrt(Xi.Ptot2Pos()+sq(mProton))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.eNegAntiProton()",
      "sqrt(Xi.Ptot2Neg()+sq(mAntiProton))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ePosPion()",
      "sqrt(Xi.Ptot2Pos()+sq(mPiPlus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.eNegPion()",
      "sqrt(Xi.Ptot2Neg()+sq(mPiMinus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following mass formulas use 12 values:
    f1 = new TTreeFormula("Xi.massLambda()",
      "sqrt(sq(Xi.ePosProton()+Xi.eNegPion())-Xi.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.massAntiLambda()",
      "sqrt(sq(Xi.eNegAntiProton()+Xi.ePosPion())-Xi.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.massK0Short()",
      "sqrt(sq(Xi.ePosPion()+Xi.eNegPion())-Xi.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    
    // The following rapidity formulas use 8 values:
    f1 = new TTreeFormula("Xi.rapLambda()",
    //  "0.5*log((Xi.eLambda()+Xi.momV0Z())/(Xi.eLambda()-Xi.momV0Z()))",tree);
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eLambda()/Xi.momV0Z())-1.)))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.rapK0Short()",
    //  "0.5*log((Xi.eK0Short()+Xi.momV0Z())/(Xi.eK0Short()-Xi.momV0Z()))",tree);
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eK0Short()/Xi.momV0Z())-1.)))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following cTau formulas use 24 values:
    f1 = new TTreeFormula("Xi.cTauLambda()",
      "Xi.massLambda()*Xi.decayLengthV0()/sqrt(Xi.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.cTauK0Short()",
      "Xi.massK0Short()*Xi.decayLengthV0()/sqrt(Xi.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    
    f1 = new TTreeFormula("Xi.ptPos()",
      "sqrt(sq(Xi.mMomPosX)+sq(Xi.mMomPosY))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptotPos()",
      "sqrt(Xi.Ptot2Pos())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptNeg()",
      "sqrt(sq(Xi.mMomNegX)+sq(Xi.mMomNegY))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptotNeg()",
      "sqrt(Xi.Ptot2Neg())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptV0()",
      "sqrt(Xi.Pt2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptotV0()",
      "sqrt(Xi.Ptot2V0())",tree);
    gROOT->GetListOfFunctions()->Add(f1);


    // Now, finally get to the unique Xi formulas:
    f1 = new TTreeFormula("Xi.charge()",
      "Xi.mCharge",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayVertexXiX()",
      "Xi.mDecayVertexXiX",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayVertexXiY()",
      "Xi.mDecayVertexXiY",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayVertexXiZ()",
      "Xi.mDecayVertexXiZ",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaXiDaughters()",
      "Xi.mDcaXiDaughters",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaXiToPrimVertex()",
      "Xi.mDcaXiToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.dcaBachelorToPrimVertex()",
      "Xi.mDcaBachelorToPrimVertex",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momBachelorX()",
      "Xi.mMomBachelorX",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momBachelorY()",
      "Xi.mMomBachelorY",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momBachelorZ()",
      "Xi.mMomBachelorZ",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momXiX()",
      "Xi.mMomBachelorX+Xi.momV0X()",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momXiY()",
      "Xi.mMomBachelorY+Xi.momV0Y()",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.momXiZ()",
      "Xi.mMomBachelorZ+Xi.momV0Z()",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.keyBachelor()",
      "Xi.mKeyBachelor",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.decayLengthXi()",
      "sqrt(sq(Xi.mDecayVertexXiX-Event.mPrimaryVertexX)+sq(Xi.mDecayVertexXiY-Event.mPrimaryVertexY)+sq(Xi.mDecayVertexXiZ-Event.mPrimaryVertexZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Ptot2Bachelor()",
      "(sq(Xi.mMomBachelorX)+sq(Xi.mMomBachelorY)+sq(Xi.mMomBachelorZ))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Pt2Xi()",
      "(sq(Xi.momXiX())+sq(Xi.momXiY()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.Ptot2Xi()",
      "(Xi.Pt2Xi()+sq(Xi.momXiZ()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following momentum component formulas use 21 and 24 values:
    f1 = new TTreeFormula("Xi.MomBachelorAlongXi()",
       "(((Xi.mMomBachelorX*Xi.momXiX())+(Xi.mMomBachelorY*Xi.momXiY())+(Xi.mMomBachelorZ*Xi.momXiZ()))/sqrt(Xi.Ptot2Xi()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.MomV0AlongXi()",
       "(((Xi.momV0X()*Xi.momXiX())+(Xi.momV0Y()*Xi.momXiY())+(Xi.momV0Z()*Xi.momXiZ()))/sqrt(Xi.Ptot2Xi()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    f1 = new TTreeFormula("Xi.alphaXi()",
    //  "(Xi.mCharge*(Xi.MomBachelorAlongXi()-Xi.MomV0AlongXi())/(Xi.MomBachelorAlongXi()+Xi.MomV0AlongXi()))",tree);
    // The above fails for exceeding 50 value limit. The following uses 46:
      "Xi.mCharge*(1.-(2./(1.+(Xi.MomBachelorAlongXi()/Xi.MomV0AlongXi()))))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    // The following formula uses 30 values:
    f1 = new TTreeFormula("Xi.ptArmXi()",
      "sqrt(Xi.Ptot2V0()-sq(Xi.MomV0AlongXi()))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following energy formulas use 9 values:
    f1 = new TTreeFormula("Xi.eXi()",
      "sqrt(Xi.Ptot2Xi()+sq(mXiMinus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.eOmega()",
      "sqrt(Xi.Ptot2Xi()+sq(mOmegaMinus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.eBachelorPion()",
      "sqrt(Xi.Ptot2Bachelor()+sq(mPiMinus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.eBachelorKaon()",
      "sqrt(Xi.Ptot2Bachelor()+sq(mKaonMinus))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following mass formulas use 18 values:
    f1 = new TTreeFormula("Xi.massXi()",
      "sqrt(sq(Xi.eLambda()+Xi.eBachelorPion())-Xi.Ptot2Xi())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.massOmega()",
      "sqrt(sq(Xi.eLambda()+Xi.eBachelorKaon())-Xi.Ptot2Xi())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    
    // The following rapidity formulas use 12 values:
    f1 = new TTreeFormula("Xi.rapXi()",
    //  "0.5*log((Xi.eXi()+Xi.momXiZ())/(Xi.eXi()-Xi.momXiZ()))",tree);
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eXi()/Xi.momXiZ())-1.)))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.rapOmega()",
    //  "0.5*log((Xi.eOmega()+Xi.momXiZ())/(Xi.eOmega()-Xi.momXiZ()))",tree);
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eOmega()/Xi.momXiZ())-1.)))",tree);
    gROOT->GetListOfFunctions()->Add(f1);

    // The following cTau formulas use 33 values:
    f1 = new TTreeFormula("Xi.cTauXi()",
      "Xi.massXi()*Xi.decayLengthXi()/sqrt(Xi.Ptot2Xi())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.cTauOmega()",
      "Xi.massOmega()*Xi.decayLengthXi()/sqrt(Xi.Ptot2Xi())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    
    f1 = new TTreeFormula("Xi.ptBachelor()",
      "sqrt(sq(Xi.mMomBachelorX)+sq(Xi.mMomBachelorY))",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptotBachelor()",
      "sqrt(Xi.Ptot2Bachelor())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptXi()",
      "sqrt(Xi.Pt2Xi())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
    f1 = new TTreeFormula("Xi.ptotXi()",
      "sqrt(Xi.Ptot2Xi())",tree);
    gROOT->GetListOfFunctions()->Add(f1);
  }

  // Kink
  if (strstr(str,"kink")) {
  }
  
  delete str;
  Int_t finalFormulas = gROOT->GetListOfFunctions()->GetSize();
  return (finalFormulas-initialFormulas);
}
