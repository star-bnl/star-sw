// $Id: strangeFormulas.C,v 1.4 2000/04/12 21:13:27 genevb Exp $
// $Log: strangeFormulas.C,v $
// Revision 1.4  2000/04/12 21:13:27  genevb
// Added track topology map functions
//
// Revision 1.3  2000/04/11 17:55:39  genevb
// Removed first parameter
//
// Revision 1.2  2000/04/07 16:22:48  genevb
// Remove ambiguity on supplying just one parameter
//
// Revision 1.1  2000/04/07 16:14:53  genevb
// Introduction of strangeFormulas.C
//
//
//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  creates formulas for use with strangeness micro DST
// usage: The parameter is either a pointer to a strangeness
//          micro DST tree, a pointer to a TFile containing such
//          a tree (tree name assumed to be "StrangeMuDst"), or
//          the name of such a file. In the latter two cases, a
//          pointer to the strangeness TTree is returned.
// examples:
//        .x strangeFormulas.C("myfile.root");
//          Opens a file named "myfile.root", loads the formulas needed,
//          and returns a pointer to the TTree.
//        .x strangeFormulas.C;
//          Opens a file named "evMuDst.root", loads the formulas needed,
//          and returns a pointer to the TTree (this is the default file).
//        .x strangeFormulas.C(myTreePtr);
//          Returns the number of new formulas loaded into Root.
//        .x strangeFormulas.C(myFilePtr);
//          Returns a pointer to the tree named "StrangeMuDst" in the
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

TTree* strangeFormulas(const char* fname=0);
TTree* strangeFormulas(TFile* fptr);
Int_t strangeFormulas(TTree* tree);

TTree* strangeFormulas(const char* fname) {
  TFile *a1;
  if (!fname) {
    a1 = new TFile("evMuDst.root");    // Default filename for root of TTree
  } else {
    a1 = new TFile(fname);
  }
  TTree* m1 = strangeFormulas(a1);
  return m1;
}

TTree* strangeFormulas(TFile* fptr) {
  if (!fptr) return 0;
  TTree* m1 = (TTree*) fptr->Get("StrangeMuDst");
  strangeFormulas(m1);
  return m1;
}

Int_t strangeFormulas(TTree* tree) {
  if (!tree) return 0;
  if (!(gROOT->GetClass("TTreeFormula"))) {
    gSystem->Load("libProof");
    gSystem->Load("libTreePlayer");
  }

  char name[256];
  char expr[2048];
  char track[32];
  char temp[256];
  char ftpc[256];
  TString tstr;

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
  if (tree->GetBranch("V0")) {
  
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
    
    // Track topology maps, with function names like
    // "V0.topologyMapNeg.*()" and "V0.topologyMapPos.*()"
    if (!(tree->GetBranch("V0.mTopologyMapPos.mMap0"))) continue;
    for (int k=0; k<2; k++) {
      if (k) sprintf(track,"Neg\0");
      else sprintf(track,"Pos\0");


      // bit(i), i=0,63, uses only 1 value.
      for (int l=0; l<2; l++) {
        for (int j=0; j<32; j++) {
          int m = l*32 + j;
          sprintf(name,"V0.topologyMap%s.bit(%d)\0",track,m);
          sprintf(expr,"(V0.mTopologyMap%s.mMap%d/(2^%d))&1\0",track,l,j);
          f1 = new TTreeFormula(name,expr,tree);
          gROOT->GetListOfFunctions()->Add(f1);
        }

        // data(i), i=0,1, uses only 1 value.
        sprintf(name,"V0.topologyMap%s.data(%d)",track,l);
	sprintf(expr,"V0.mTopologyMap%s.mMap%d",track,l);
        f1 = new TTreeFormula(name,expr,tree);
        gROOT->GetListOfFunctions()->Add(f1);
      }

      // ftpcFormat()
      sprintf(ftpc,"V0.topologyMap%s.ftpcFormat()",track);
      sprintf(expr,"V0.topologyMap%s.bit(63)",track);
      f1 = new TTreeFormula(ftpc,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // primaryVertexUsed()
      sprintf(name,"V0.topologyMap%s.primaryVertexUsed()",track);
      sprintf(expr,"V0.topologyMap%s.bit(0)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // turnAroundFlag()
      sprintf(name,"V0.topologyMap%s.turnAroundFlag()",track);
      sprintf(expr,"V0.topologyMap%s.bit(62)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInSvtLayer(i), i=1,6
      for (int j=1; j<7; j++) {
        sprintf(name,"V0.topologyMap%s.hasHitInSvtLayer(%d)",track,j);
        sprintf(temp,"V0.topologyMap%s.bit(%d)\0",track,j);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        f1 = new TTreeFormula(name,expr,tree);
        gROOT->GetListOfFunctions()->Add(f1);
	
        tstr += temp;
        if (j<6) tstr += "+";
      }

      // numOfSvtHits(), uses 6 values
      sprintf(name,"V0.topologyMap%s.numOfSvtHits()\0",track);
      sprintf(expr,"(!%s)*(%s)\0",ftpc,tstr.Data());
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);
      tstr = "";
      
      // hasHitInSsd()
      sprintf(name,"V0.topologyMap%s.hasHitInSsd()",track);
      sprintf(expr,"V0.topologyMap%s.bit(7)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      sprintf(temp,"(!%s)*(\0",ftpc);
      tstr = temp;

      // hasHitInTpcRow(i), i=1,45
      for (int j=1; j<45; j++) {
        sprintf(name,"V0.topologyMap%s.hasHitInTpcRow(%d)",track,j);
	int m = j+8;
        sprintf(temp,"V0.topologyMap%s.bit(%d)\0",track,m);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        f1 = new TTreeFormula(name,expr,tree);
        gROOT->GetListOfFunctions()->Add(f1);
	
        tstr += temp;
        if (j<44) tstr += "+";
      }

      // numOfTpcHits(), uses 45 values.
      sprintf(name,"V0.topologyMap%s.numOfTpcHits()\0",track);
      tstr += ")";
      f1 = new TTreeFormula(name,tstr.Data(),tree);
      gROOT->GetListOfFunctions()->Add(f1);
      tstr = "";

      // hasHitInMwpc()
      sprintf(name,"V0.topologyMap%s.hasHitInMwpc()",track);
      sprintf(expr,"V0.topologyMap%s.bit(53)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInCtb()
      sprintf(name,"V0.topologyMap%s.hasHitInCtb()",track);
      sprintf(expr,"V0.topologyMap%s.bit(54)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInTofPatch()
      sprintf(name,"V0.topologyMap%s.hasHitInTofPatch()",track);
      sprintf(expr,"V0.topologyMap%s.bit(55)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInRich()
      sprintf(name,"V0.topologyMap%s.hasHitInRich()",track);
      sprintf(expr,"V0.topologyMap%s.bit(56)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInBarrelEmc()
      sprintf(name,"V0.topologyMap%s.hasHitInBarrelEmc()",track);
      sprintf(expr,"V0.topologyMap%s.bit(57)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInEndcapEmc()
      sprintf(name,"V0.topologyMap%s.hasHitInEndcapEmc()",track);
      sprintf(expr,"V0.topologyMap%s.bit(58)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);
    }

  }  // End of V0
  
  // Xi
  if (tree->GetBranch("Xi")) {
  
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

    // Track topology maps, with function names like
    // "Xi.topologyMapNeg.*()", Xi.topologyMapPos.*(),
    // and "Xi.topologyMapBachelor.*()"
    if (!(tree->GetBranch("Xi.mTopologyMapBachelor.mMap0"))) continue;
    for (int k=0; k<3; k++) {
      if (k==2) sprintf(track,"Bachelor\0");
      else if (k==1) sprintf(track,"Neg\0");
      else sprintf(track,"Pos\0");


      // bit(i), i=0,63, uses only 1 value.
      for (int l=0; l<2; l++) {
        for (int j=0; j<32; j++) {
          int m = l*32 + j;
          sprintf(name,"Xi.topologyMap%s.bit(%d)\0",track,m);
          sprintf(expr,"(Xi.mTopologyMap%s.mMap%d/(2^%d))&1\0",track,l,j);
          f1 = new TTreeFormula(name,expr,tree);
          gROOT->GetListOfFunctions()->Add(f1);
        }

        // data(i), i=0,1, uses only 1 value.
        sprintf(name,"Xi.topologyMap%s.data(%d)",track,l);
	sprintf(expr,"Xi.mTopologyMap%s.mMap%d",track,l);
        f1 = new TTreeFormula(name,expr,tree);
        gROOT->GetListOfFunctions()->Add(f1);
      }

      // ftpcFormat()
      sprintf(ftpc,"Xi.topologyMap%s.ftpcFormat()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(63)",track);
      f1 = new TTreeFormula(ftpc,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // primaryVertexUsed()
      sprintf(name,"Xi.topologyMap%s.primaryVertexUsed()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(0)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // turnAroundFlag()
      sprintf(name,"Xi.topologyMap%s.turnAroundFlag()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(62)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInSvtLayer(i), i=1,6
      for (int j=1; j<7; j++) {
        sprintf(name,"Xi.topologyMap%s.hasHitInSvtLayer(%d)",track,j);
        sprintf(temp,"Xi.topologyMap%s.bit(%d)\0",track,j);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        f1 = new TTreeFormula(name,expr,tree);
        gROOT->GetListOfFunctions()->Add(f1);
	
        tstr += temp;
        if (j<6) tstr += "+";
      }

      // numOfSvtHits(), uses 6 values
      sprintf(name,"Xi.topologyMap%s.numOfSvtHits()\0",track);
      sprintf(expr,"(!%s)*(%s)\0",ftpc,tstr.Data());
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);
      tstr = "";
      
      // hasHitInSSD()
      sprintf(name,"Xi.topologyMap%s.hasHitInSSD()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(7)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      sprintf(temp,"(!%s)*(\0",ftpc);
      tstr = temp;

      // hasHitInTpcRow(i), i=1,45
      for (int j=1; j<45; j++) {
        sprintf(name,"Xi.topologyMap%s.hasHitInTpcRow(%d)",track,j);
	int m = j+8;
        sprintf(temp,"Xi.topologyMap%s.bit(%d)\0",track,m);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        f1 = new TTreeFormula(name,expr,tree);
        gROOT->GetListOfFunctions()->Add(f1);
	
        tstr += temp;
        if (j<44) tstr += "+";
      }

      // numOfTpcHits(), uses 45 values.
      sprintf(name,"Xi.topologyMap%s.numOfTpcHits()\0",track);
      tstr += ")";
      f1 = new TTreeFormula(name,tstr.Data(),tree);
      gROOT->GetListOfFunctions()->Add(f1);
      tstr = "";

      // hasHitInMwpc()
      sprintf(name,"Xi.topologyMap%s.hasHitInMwpc()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(53)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInCtb()
      sprintf(name,"Xi.topologyMap%s.hasHitInCtb()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(54)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInTofPatch()
      sprintf(name,"Xi.topologyMap%s.hasHitInTofPatch()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(55)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInRich()
      sprintf(name,"Xi.topologyMap%s.hasHitInRich()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(56)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInBarrelEmc()
      sprintf(name,"Xi.topologyMap%s.hasHitInBarrelEmc()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(57)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);

      // hasHitInEndcapEmc()
      sprintf(name,"Xi.topologyMap%s.hasHitInEndcapEmc()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(58)",track);
      f1 = new TTreeFormula(name,expr,tree);
      gROOT->GetListOfFunctions()->Add(f1);
    }

  }  // End of Xi

  // Kink
  if (tree->GetBranch("Kink")) {
  }  // End of Kink
  
  Int_t finalFormulas = gROOT->GetListOfFunctions()->GetSize();
  return (finalFormulas-initialFormulas);
}
