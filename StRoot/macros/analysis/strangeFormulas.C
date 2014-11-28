//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  creates formulas for use with strangeness micro DST
// usage: The parameter is either a pointer to a strangeness
//          micro DST tree, a pointer to a TFile containing such
//          a tree (tree name assumed to be either "StrangeMuDst"
//          or "MuDst" (in the case of a common micro DST)), or
//          the name of such a file or files (one can use wildcards
//          to examine the data in multiple files). In the latter
//          two cases, a pointer to the strangeness TTree is returned.
//          See documentation for more information.
//
//          After initial execution of the macro, the user may utilize
//          the global pointers TTree* strangeTree or TChain* strangeChain.
//          Additonal files can be added to the chain by calling
//          strangeFormulas(filename) again.
//
// examples:
//        .x strangeFormulas.C("myfile.root");
//          Opens a file named "myfile.root", loads the formulas needed,
//          and returns a pointer to the TTree.
//        .x strangeFormulas.C("/path/to/otherfiles*.root");
//          Opens all the files matching the pattern using a TChain.
//          The returned pointer is the TChain cast to a TTree pointer.
//        .x strangeFormulas.C("listfile.lis");
//          Opens all the files specified in the file listfile.lis,
//          whose format may simply be one filename per line, or the
//          output of a call to: get_file_list.pl -keys path,filename ...
//          The returned pointer is the TChain cast to a TTree pointer.
//        .x strangeFormulas.C;
//          Opens a file named "evMuDst.root", loads the formulas needed,
//          and returns a pointer to the TTree (this is the default file).
//        .x strangeFormulas.C(myTreePtr);
//          Returns the number of new formulas loaded into Root.
//        .x strangeFormulas.C(myFilePtr);
//          Returns a pointer to the tree named either "StrangeMuDst"
//          or "MuDst" in the file pointed to by myFilePtr.
//======================================================
//
// Should be careful to optimize formulas for number of calls to
// other formulas. TTreeFormula/TFormula is not smart enough to save
// values to repeat them, and calculates the full thing twice, but
// has a limit of 100 values one can calculate (only 50 values for
// Root 2.24/04 and earlier).
//
// For example, "a()+a()+a()" repeats everything necessary to find
// "a()" three times. If "a()" requires 20 values to calculate, then
// "a()+a()+a()" will fail for Root 2.24/04, needing 60 values. Much
// better to write something like "3*a()".
//


// Public interface:
//    TTree* strangeFormulas(const char* fname=0, const char* tname=0);
//    TTree* strangeFormulas(TFile* fptr);
//     Int_t strangeFormulas(TTree* tree);
//      void findFormulas(const char* c0, const char* c1=0, const char* c2=0);
// TFormula* findFormula(const char* c0);       // Requires exact match
//      void findDefinition(const char* c0);    // Requires exact match


// Internal members and functions:
// void formulate(const char* name, const char* formula);
TTree* strangeTree=0;
TChain* strangeChain=0;
TCollection* ListofFuncs=0;
Int_t max_codes=0;
static const char* defaultFile = "evMuDst.root";
static const char* defaultTree = "StrangeMuDst";
static const char* altTree = "MuDst";

//-----------------------------------------------------

void prep() {
  if (!(gROOT->GetClass("TTreePlayer"))) {
    gSystem->Load("libTreePlayer");
    gSystem->Load("libProof");
  }
  if (!ListofFuncs) ListofFuncs=gROOT->GetListOfFunctions();
}

//-----------------------------------------------------

void findFormulas(const char* c0, const char* c1=0, const char* c2=0) {
  for (Int_t i=0; i<ListofFuncs->GetSize(); i++) {
    TString ostr = ListofFuncs->At(i)->GetName();
    if (ostr.Contains(c0,TString::kIgnoreCase)) {
      if ((!c1) || (ostr.Contains(c1,TString::kIgnoreCase))) {
        if ((!c2) || (ostr.Contains(c2,TString::kIgnoreCase))) {
          cout << ostr.Data() << endl;
        }
      }
    }
  }
}

//-----------------------------------------------------

TFormula* findFormula(const char* c0) {
  return (TFormula*) ListofFuncs->FindObject(c0);
}

//-----------------------------------------------------
    
void findDefinition(const char* c0) {
  TFormula* f1 = findFormula(c0);
  if (f1) {
    cout << "Name       : " << f1->GetName() << endl;
    if (f1->IsA() == TTreeFormula::Class())
      cout << "# of Codes : " << ((TTreeFormula*) f1)->GetNcodes() << endl;
    cout << "Definition : " << f1->GetTitle() << endl;
  }
}

//-----------------------------------------------------

void formulate(const char* name, const char* formula) {
  TTreeFormula* f1 = (TTreeFormula*) findFormula(name);
  if (f1) {
    f1->SetTree(strangeTree);
  } else {
    f1 = new TTreeFormula(name,formula,strangeTree);
    ListofFuncs->Add(f1);
    if (f1->GetNcodes() > max_codes) {
      cout << "\nWARNING!!! The following formula uses " << f1->GetNcodes();
      cout << " values,\n too many for this version of ROOT: \n";
      cout << f1->GetName() << " = " << f1->GetTitle() << endl;
    }
  }
}

//-----------------------------------------------------

TTree* strangeFormulas(const char* fname=0, const char* tname=0) {
  if (!fname) {
    fname = defaultFile;
  } else {
    
    // Check for list files...
    TString fnamestr = fname;
    if (!(fnamestr.EndsWith(".root"))) {
      ifstream inlist(fname);
      if (!inlist) {
        cout << "\nERROR!!! Cannot find list file: " << fname << endl;
	return strangeTree;
      }
      char iname[256];
      TString inamestr;
      inlist >> iname;
      while (! inlist.eof()) {
	inamestr = iname;
	inamestr.ReplaceAll("::","/");
	inamestr.ReplaceAll("//","/");
	strangeFormulas(inamestr.Data(),tname);
	if (!strangeTree) {
	  cout << "\nERROR!!! Trouble with first file: "
	       << inamestr.Data() << endl;
          return 0;
	}
        inlist >> iname;
      }
      return strangeTree;
    }
  }

  // If we already have a chain and are just adding files...
  if (strangeChain) {
    strangeChain->Add(fname);
    return strangeTree;
  }

  // Start chain with the first filename...
  prep();
  if (!tname) {
    tname = defaultTree;
  }
  printf("Looking for tree with name %s\n",tname);
  strangeChain = new TChain(tname);
  strangeChain->Add(fname);
  if (strangeChain->LoadTree(0)) {
    delete strangeChain;
    strangeChain = 0;
    // Try again with altTree
    if (!(strcmp(tname,defaultTree))) return strangeFormulas(fname,altTree);
    return 0;
  }
  strangeTree = (TTree*) strangeChain;
  strangeFormulas(strangeTree);
  return strangeTree;
}

//-----------------------------------------------------

TTree* strangeFormulas(TFile* fptr) {
  prep();
  if (!fptr) return 0;
  strangeTree = (TTree*) fptr->Get(defaultTree);
  if (!strangeTree) strangeTree = (TTree*) fptr->Get(altTree);
  strangeFormulas(strangeTree);
  return strangeTree;
}

//-----------------------------------------------------

Int_t strangeFormulas(TTree* tree) {
  prep();
  if (!tree) return 0;
  if (gROOT->GetVersionInt() < 22405) {
    max_codes = 50;
  } else {
    max_codes = 100;
  }

  // These branches seem to require classes. No thanks.
  if (tree->GetBranch("EmcCollection")) {
    tree->SetBranchStatus("EmcCollection.*",0);
  }
  if (tree->GetBranch("PmdCollection")) {
    tree->SetBranchStatus("PmdCollection.*",0);
  }

  strangeTree = tree;
  char name[256];
  char expr[2048];
  char track[32];
  char temp[256];
  char ftpc[256];
  TString tstr;

  TFormula *f0=0;
  Int_t initialFormulas = ListofFuncs->GetSize();
  
  // Mass formulas
  f0 = new TFormula("mLambda", "1.11563");
  f0 = new TFormula("mAntiLambda", "1.11563");
  f0 = new TFormula("mK0Short", "0.497671");
  f0 = new TFormula("mProton", "0.938272");
  f0 = new TFormula("mAntiProton", "0.938272");
  f0 = new TFormula("mPiPlus", "0.139568");
  f0 = new TFormula("mPiMinus", "0.139568");
  f0 = new TFormula("mKaonMinus", "0.493646");
  

  // Event
  printf("Loading event formulas...\n");
  formulate("Event.run()", "Event.mRun");
  formulate("Event.event()", "abs(Event.mEvent)");
  formulate("Event.globalTracks()", "Event.mGlobalTracks");
  formulate("Event.primaryTracks()", "Event.mPrimaryTracks");
  formulate("Event.primaryNegTracks()", "Event.mPrimaryNegTracks");
  formulate("Event.primaryVertexX()", "Event.mPrimaryVertexX");
  formulate("Event.primaryVertexY()", "Event.mPrimaryVertexY");
  formulate("Event.primaryVertexZ()", "Event.mPrimaryVertexZ");
  formulate("Event.magneticField()",  "Event.mMagneticField");
  formulate("Event.l0TriggerWord()", "Event.mL0TriggerWord");
  formulate("Event.unbiasedTrigger()", "(Event.mEvent>0)");

  
  // McEvent
  if (tree->GetBranch("McEvent")) {
    printf("Loading MC event formulas...\n");

    formulate("McEvent.run()", "McEvent.mRun");
    formulate("McEvent.event()", "McEvent.mEvent");
    formulate("McEvent.globalTracks()", "McEvent.mGlobalTracks");
    formulate("McEvent.primaryTracks()", "McEvent.mPrimaryTracks");
    formulate("McEvent.primaryNegTracks()", "McEvent.mPrimaryNegTracks");
    formulate("McEvent.primaryVertexX()", "McEvent.mPrimaryVertexX");
    formulate("McEvent.primaryVertexY()", "McEvent.mPrimaryVertexY");
    formulate("McEvent.primaryVertexZ()", "McEvent.mPrimaryVertexZ");

  }  // End of McEvent


  // V0
  if (tree->GetBranch("V0")) {
    printf("Loading V0 formulas...\n");
  
    // The following formula uses 6 values:
    formulate("V0.decayLengthV0()",
      "sqrt(sq(V0.mDecayVertexV0X-Event.mPrimaryVertexX[0])+sq(V0.mDecayVertexV0Y-Event.mPrimaryVertexY[0])+sq(V0.mDecayVertexV0Z-Event.mPrimaryVertexZ[0]))");
  
    // The following formulas use 1 value:
    formulate("V0.decayVertexV0X()", "V0.mDecayVertexV0X");
    formulate("V0.decayVertexV0Y()", "V0.mDecayVertexV0Y");
    formulate("V0.decayVertexV0Z()", "V0.mDecayVertexV0Z");
    formulate("V0.dcaV0Daughters()", "V0.mDcaV0Daughters");
    formulate("V0.dcaV0ToPrimVertex()", "V0.mDcaV0ToPrimVertex");
    formulate("V0.dcaPosToPrimVertex()", "V0.mDcaPosToPrimVertex");
    formulate("V0.dcaNegToPrimVertex()", "V0.mDcaNegToPrimVertex");
    formulate("V0.momPosX()", "V0.mMomPosX");
    formulate("V0.momPosY()", "V0.mMomPosY");
    formulate("V0.momPosZ()", "V0.mMomPosZ");
    formulate("V0.momNegX()", "V0.mMomNegX");
    formulate("V0.momNegY()", "V0.mMomNegY");
    formulate("V0.momNegZ()", "V0.mMomNegZ");
    
    formulate("V0.radt2V0()", "sq(V0.mDecayVertexV0X)+sq(V0.mDecayVertexV0Y)");
    formulate("V0.radtV0()", "sqrt(V0.radt2V0())");
    formulate("V0.radV0()", "sqrt(V0.radt2V0()+sq(V0.mDecayVertexV0Z))");
    formulate("V0.phiV0()",
      "atan2((V0.mDecayVertexV0Y-Event.mPrimaryVertexY[0]),(V0.mDecayVertexV0X-Event.mPrimaryVertexX[0]))");
    formulate("V0.phi90V0()",
      "atan2(-(V0.mDecayVertexV0X-Event.mPrimaryVertexX[0]),(V0.mDecayVertexV0Y-Event.mPrimaryVertexY[0]))");

    formulate("V0.Ptot2Pos()",
      "(sq(V0.mMomPosX)+sq(V0.mMomPosY)+sq(V0.mMomPosZ))");
    formulate("V0.Ptot2Neg()",
      "(sq(V0.mMomNegX)+sq(V0.mMomNegY)+sq(V0.mMomNegZ))");
    formulate("V0.momV0X()", "(V0.mMomPosX+V0.mMomNegX)");
    formulate("V0.momV0Y()", "(V0.mMomPosY+V0.mMomNegY)");
    formulate("V0.momV0Z()", "(V0.mMomPosZ+V0.mMomNegZ)");
    formulate("V0.Pt2V0()", "(sq(V0.momV0X())+sq(V0.momV0Y()))");
    formulate("V0.Ptot2V0()", "(V0.Pt2V0()+sq(V0.momV0Z()))");

    // The following momentum component formulas use 15 values:
    formulate("V0.MomPosAlongV0()",
      "(((V0.mMomPosX*V0.momV0X())+(V0.mMomPosY*V0.momV0Y())+(V0.mMomPosZ*V0.momV0Z()))/sqrt(V0.Ptot2V0()))");
    formulate("V0.MomNegAlongV0()",
      "(((V0.mMomNegX*V0.momV0X())+(V0.mMomNegY*V0.momV0Y())+(V0.mMomNegZ*V0.momV0Z()))/sqrt(V0.Ptot2V0()))");

    formulate("V0.alphaV0()",
    //  "((V0.MomPosAlongV0()-V0.MomNegAlongV0())/(V0.MomPosAlongV0()+V0.MomNegAlongV0()))");
    // The above is cumbersome, using 50 values. The following uses 30:
      "1.-(2./(1.+(V0.MomPosAlongV0()/V0.MomNegAlongV0())))");
    // The following formula uses 18 values:
    formulate("V0.ptArmV0()", "sqrt(V0.Ptot2Pos()-sq(V0.MomPosAlongV0()))");

    // The following energy formulas use 6 values:
    formulate("V0.eLambda()", "sqrt(V0.Ptot2V0()+sq(mLambda))");
    formulate("V0.eK0Short()", "sqrt(V0.Ptot2V0()+sq(mK0Short))");
    formulate("V0.ePosProton()", "sqrt(V0.Ptot2Pos()+sq(mProton))");
    formulate("V0.eNegAntiProton()", "sqrt(V0.Ptot2Neg()+sq(mAntiProton))");
    formulate("V0.ePosPion()", "sqrt(V0.Ptot2Pos()+sq(mPiPlus))");
    formulate("V0.eNegPion()", "sqrt(V0.Ptot2Neg()+sq(mPiMinus))");

    // The following mass formulas use 12 values:
    formulate("V0.massLambda()",
      "sqrt(sq(V0.ePosProton()+V0.eNegPion())-V0.Ptot2V0())");
    formulate("V0.massAntiLambda()",
      "sqrt(sq(V0.eNegAntiProton()+V0.ePosPion())-V0.Ptot2V0())");
    formulate("V0.massK0Short()",
      "sqrt(sq(V0.ePosPion()+V0.eNegPion())-V0.Ptot2V0())");
    
    // The following rapidity formulas use 8 values:
    formulate("V0.rapLambda()",
    //  "0.5*log((V0.eLambda()+V0.momV0Z())/(V0.eLambda()-V0.momV0Z()))");
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((V0.eLambda()/V0.momV0Z())-1.)))");
    formulate("V0.rapK0Short()",
    //  "0.5*log((V0.eK0Short()+V0.momV0Z())/(V0.eK0Short()-V0.momV0Z()))");
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((V0.eK0Short()/V0.momV0Z())-1.)))");

    // The following cTau formulas use 24 values:
    formulate("V0.cTauLambda()",
      "V0.massLambda()*V0.decayLengthV0()/sqrt(V0.Ptot2V0())");
    formulate("V0.cTauK0Short()",
      "V0.massK0Short()*V0.decayLengthV0()/sqrt(V0.Ptot2V0())");
    
    formulate("V0.ptPos()", "sqrt(sq(V0.mMomPosX)+sq(V0.mMomPosY))");
    formulate("V0.ptotPos()", "sqrt(V0.Ptot2Pos())");
    formulate("V0.ptNeg()", "sqrt(sq(V0.mMomNegX)+sq(V0.mMomNegY))");
    formulate("V0.ptotNeg()", "sqrt(V0.Ptot2Neg())");
    formulate("V0.ptV0()", "sqrt(V0.Pt2V0())");
    formulate("V0.ptotV0()", "sqrt(V0.Ptot2V0())");
    formulate("V0.thetaV0()", "acos(V0.momV0Z()/V0.ptotV0())");
    formulate("V0.pseudoRapV0()", "-log(tan(V0.thetaV0()/2.))");
    formulate("V0.psiV0()", "atan2(V0.momV0Y(),V0.momV0X())");
    formulate("V0.psi90V0()", "atan2(-V0.momV0X(),V0.momV0Y())");

    // Track topology maps, with function names like
    // "V0.topologyMapNeg.*()" and "V0.topologyMapPos.*()"
    if (tree->GetBranch("V0.mTopologyMapPos.mMap0")) {
    for (int k=0; k<2; k++) {
      if (k) sprintf(track,"Neg\0");
      else sprintf(track,"Pos\0");


      // bit(i), i=0,63, uses only 1 value.
      for (int l=0; l<2; l++) {
        for (int j=0; j<32; j++) {
          int m = l*32 + j;
          sprintf(name,"V0.topologyMap%s.bit(%d)\0",track,m);
          sprintf(expr,"(V0.mTopologyMap%s.mMap%d/(2^%d))&1\0",track,l,j);
          formulate(name,expr);
        }

        // data(i), i=0,1, uses only 1 value.
        sprintf(name,"V0.topologyMap%s.data(%d)",track,l);
	sprintf(expr,"V0.mTopologyMap%s.mMap%d",track,l);
        formulate(name,expr);
      }

      // ftpcFormat()
      sprintf(ftpc,"V0.topologyMap%s.ftpcFormat()",track);
      sprintf(expr,"V0.topologyMap%s.bit(63)",track);
      formulate(ftpc,expr);

      // primaryVertexUsed()
      sprintf(name,"V0.topologyMap%s.primaryVertexUsed()",track);
      sprintf(expr,"V0.topologyMap%s.bit(0)",track);
      formulate(name,expr);

      // turnAroundFlag()
      sprintf(name,"V0.topologyMap%s.turnAroundFlag()",track);
      sprintf(expr,"V0.topologyMap%s.bit(62)",track);
      formulate(name,expr);

      // hasHitInSvtLayer(i), i=1,6
      for (int j=1; j<7; j++) {
        sprintf(name,"V0.topologyMap%s.hasHitInSvtLayer(%d)",track,j);
        sprintf(temp,"V0.topologyMap%s.bit(%d)\0",track,j);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        formulate(name,expr);
	
        tstr += temp;
        if (j<6) tstr += "+";
      }

      // numOfSvtHits(), uses 6 values
      sprintf(name,"V0.topologyMap%s.numOfSvtHits()\0",track);
      sprintf(expr,"(!%s)*(%s)\0",ftpc,tstr.Data());
      formulate(name,expr);
      tstr = "";
      
      // hasHitInSsd()
      sprintf(name,"V0.topologyMap%s.hasHitInSsd()",track);
      sprintf(expr,"V0.topologyMap%s.bit(7)",track);
      formulate(name,expr);

      sprintf(temp,"(!%s)*(\0",ftpc);
      tstr = temp;

      // hasHitInTpcRow(i), i=0,44
      for (int j=0; j<45; j++) {
        sprintf(name,"V0.topologyMap%s.hasHitInTpcRow(%d)",track,j);
	int m = j+8;
        sprintf(temp,"V0.topologyMap%s.bit(%d)\0",track,m);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        formulate(name,expr);
	
        tstr += temp;
        if (j<44) tstr += "+";
      }

      // numOfTpcHits(), uses 45 values.
      sprintf(name,"V0.topologyMap%s.numOfTpcHits()\0",track);
      tstr += ")";
      formulate(name,tstr.Data());
      tstr = "";

      // hasHitInMwpc()
      sprintf(name,"V0.topologyMap%s.hasHitInMwpc()",track);
      sprintf(expr,"V0.topologyMap%s.bit(53)",track);
      formulate(name,expr);

      // hasHitInCtb()
      sprintf(name,"V0.topologyMap%s.hasHitInCtb()",track);
      sprintf(expr,"V0.topologyMap%s.bit(54)",track);
      formulate(name,expr);

      // hasHitInTofPatch()
      sprintf(name,"V0.topologyMap%s.hasHitInTofPatch()",track);
      sprintf(expr,"V0.topologyMap%s.bit(55)",track);
      formulate(name,expr);

      // hasHitInRich()
      sprintf(name,"V0.topologyMap%s.hasHitInRich()",track);
      sprintf(expr,"V0.topologyMap%s.bit(56)",track);
      formulate(name,expr);

      // hasHitInBarrelEmc()
      sprintf(name,"V0.topologyMap%s.hasHitInBarrelEmc()",track);
      sprintf(expr,"V0.topologyMap%s.bit(57)",track);
      formulate(name,expr);

      // hasHitInEndcapEmc()
      sprintf(name,"V0.topologyMap%s.hasHitInEndcapEmc()",track);
      sprintf(expr,"V0.topologyMap%s.bit(58)",track);
      formulate(name,expr);
    } } // End topology maps

    // The following formulas use 1 value:
    formulate("V0.chi2V0()", "V0.mChi2V0");
    formulate("V0.clV0()", "V0.mClV0");
    formulate("V0.chi2Pos()", "V0.mChi2Pos");
    formulate("V0.clPos()", "V0.mClPos");
    formulate("V0.dedxPos()", "V0.mDedxPos");
    formulate("V0.numDedxPos()", "V0.mNumDedxPos");
    formulate("V0.chi2Neg()", "V0.mChi2Neg");
    formulate("V0.clNeg()", "V0.mClNeg");
    formulate("V0.dedxNeg()", "V0.mDedxNeg");
    formulate("V0.numDedxNeg()", "V0.mNumDedxNeg");

  }  // End of V0

  // V0Mc
  if (tree->GetBranch("V0Mc")) {
    printf("Loading V0Mc formulas...\n");

    // The following formula uses 6 values:
    formulate("V0Mc.decayLengthV0()",
      "sqrt(sq(V0Mc.mPositionX-McEvent.mPrimaryVertexX[0])+sq(V0Mc.mPositionY-McEvent.mPrimaryVertexY[0])+sq(V0Mc.mPositionZ-McEvent.mPrimaryVertexZ[0]))");

    // The following formulas use 1 value:
    formulate("V0Mc.decayMode()", "V0Mc.mDecayMode");
    formulate("V0Mc.positiveCommonTpcHits()", "V0Mc.mPositiveCommonTpcHits");
    formulate("V0Mc.positiveSimTpcHits()", "V0Mc.mPositiveSimTpcHits");
    formulate("V0Mc.negativeCommonTpcHits()", "V0Mc.mNegativeCommonTpcHits");
    formulate("V0Mc.negativeSimTpcHits()", "V0Mc.mNegativeSimTpcHits");
    formulate("V0Mc.geantIdParent()", "V0Mc.mParentGeantId");
    formulate("V0Mc.geantIdPositive()", "V0Mc.mPositiveGeantId");
    formulate("V0Mc.geantIdNegative()", "V0Mc.mNegativeGeantId");
    formulate("V0Mc.parentMomentumX()", "V0Mc.mParentMomentumX");
    formulate("V0Mc.parentMomentumY()", "V0Mc.mParentMomentumY");
    formulate("V0Mc.parentMomentumZ()", "V0Mc.mParentMomentumZ");
    formulate("V0Mc.positiveMomentumX()", "V0Mc.mPositiveMomentumX");
    formulate("V0Mc.positiveMomentumY()", "V0Mc.mPositiveMomentumY");
    formulate("V0Mc.positiveMomentumZ()", "V0Mc.mPositiveMomentumZ");
    formulate("V0Mc.negativeMomentumX()", "V0Mc.mNegativeMomentumX");
    formulate("V0Mc.negativeMomentumY()", "V0Mc.mNegativeMomentumY");
    formulate("V0Mc.negativeMomentumZ()", "V0Mc.mNegativeMomentumZ");
    formulate("V0Mc.positionX()", "V0Mc.mPositionX");
    formulate("V0Mc.positionY()", "V0Mc.mPositionY");
    formulate("V0Mc.positionZ()", "V0Mc.mPositionZ");

  }  // End of V0Mc
  

  // Xi
  if (tree->GetBranch("Xi")) {
    printf("Loading Xi formulas...\n");

    f0 = new TFormula("mXiMinus", "1.32133");
    f0 = new TFormula("mOmegaMinus", "1.67243");

    // First, the Xi's get all the same functions that the V0's get...
    // The following formula uses 6 values:
    formulate("Xi.decayLengthV0()",
      "sqrt(sq(Xi.mDecayVertexV0X-Xi.mDecayVertexXiX)+sq(Xi.mDecayVertexV0Y-Xi.mDecayVertexXiY)+sq(Xi.mDecayVertexV0Z-Xi.mDecayVertexXiZ))");
//      "sqrt(sq(Xi.mDecayVertexV0X-Event.mPrimaryVertexX[0])+sq(Xi.mDecayVertexV0Y-Event.mPrimaryVertexY[0])+sq(Xi.mDecayVertexV0Z-Event.mPrimaryVertexZ[0]))");

    // The following formulas use 1 value:
    formulate("Xi.decayVertexV0X()", "Xi.mDecayVertexV0X");
    formulate("Xi.decayVertexV0Y()", "Xi.mDecayVertexV0Y");
    formulate("Xi.decayVertexV0Z()", "Xi.mDecayVertexV0Z");
    formulate("Xi.dcaV0Daughters()", "Xi.mDcaV0Daughters");
    formulate("Xi.dcaV0ToPrimVertex()", "Xi.mDcaV0ToPrimVertex");
    formulate("Xi.dcaPosToPrimVertex()", "Xi.mDcaPosToPrimVertex");
    formulate("Xi.dcaNegToPrimVertex()", "Xi.mDcaNegToPrimVertex");
    formulate("Xi.momPosX()", "Xi.mMomPosX");
    formulate("Xi.momPosY()", "Xi.mMomPosY");
    formulate("Xi.momPosZ()", "Xi.mMomPosZ");
    formulate("Xi.momNegX()", "Xi.mMomNegX");
    formulate("Xi.momNegY()", "Xi.mMomNegY");
    formulate("Xi.momNegZ()", "Xi.mMomNegZ");

    formulate("Xi.radt2V0()", "sq(Xi.mDecayVertexV0X)+sq(Xi.mDecayVertexV0Y)");
    formulate("Xi.radtV0()", "sqrt(Xi.radt2V0())");
    formulate("Xi.radV0()", "sqrt(Xi.radt2V0()+sq(Xi.mDecayVertexV0Z))");
    formulate("Xi.phiV0()",
      "atan2((Xi.mDecayVertexV0Y-Xi.mDecayVertexXiY),(Xi.mDecayVertexV0X-Xi.mDecayVertexXiX))");
//      "atan2((Xi.mDecayVertexV0Y-Event.mPrimaryVertexY[0]),(Xi.mDecayVertexV0X-Event.mPrimaryVertexX[0]))");

    formulate("Xi.Ptot2Pos()",
      "(sq(Xi.mMomPosX)+sq(Xi.mMomPosY)+sq(Xi.mMomPosZ))");
    formulate("Xi.Ptot2Neg()",
      "(sq(Xi.mMomNegX)+sq(Xi.mMomNegY)+sq(Xi.mMomNegZ))");
    formulate("Xi.momV0X()", "(Xi.mMomPosX+Xi.mMomNegX)");
    formulate("Xi.momV0Y()", "(Xi.mMomPosY+Xi.mMomNegY)");
    formulate("Xi.momV0Z()", "(Xi.mMomPosZ+Xi.mMomNegZ)");
    formulate("Xi.Pt2V0()", "(sq(Xi.momV0X())+sq(Xi.momV0Y()))");
    formulate("Xi.Ptot2V0()", "(Xi.Pt2V0()+sq(Xi.momV0Z()))");

    // The following momentum component formulas use 15 values:
    formulate("Xi.MomPosAlongV0()",
      "(((Xi.mMomPosX*Xi.momV0X())+(Xi.mMomPosY*Xi.momV0Y())+(Xi.mMomPosZ*Xi.momV0Z()))/sqrt(Xi.Ptot2V0()))");
    formulate("Xi.MomNegAlongV0()",
      "(((Xi.mMomNegX*Xi.momV0X())+(Xi.mMomNegY*Xi.momV0Y())+(Xi.mMomNegZ*Xi.momV0Z()))/sqrt(Xi.Ptot2V0()))");

    formulate("Xi.alphaV0()",
    //  "((Xi.MomPosAlongV0()-Xi.MomNegAlongV0())/(Xi.MomPosAlongV0()+Xi.MomNegAlongV0()))");
    // The above is cumbersome, using 50 values. The following uses 30:
      "1.-(2./(1.+(Xi.MomPosAlongV0()/Xi.MomNegAlongV0())))");
    // The following formula uses 18 values:
    formulate("Xi.ptArmV0()",
      "sqrt(Xi.Ptot2Pos()-sq(Xi.MomPosAlongV0()))");

    // The following energy formulas use 6 values:
    formulate("Xi.eLambda()", "sqrt(Xi.Ptot2V0()+sq(mLambda))");
    formulate("Xi.eK0Short()", "sqrt(Xi.Ptot2V0()+sq(mK0Short))");
    formulate("Xi.ePosProton()", "sqrt(Xi.Ptot2Pos()+sq(mProton))");
    formulate("Xi.eNegAntiProton()", "sqrt(Xi.Ptot2Neg()+sq(mAntiProton))");
    formulate("Xi.ePosPion()", "sqrt(Xi.Ptot2Pos()+sq(mPiPlus))");
    formulate("Xi.eNegPion()", "sqrt(Xi.Ptot2Neg()+sq(mPiMinus))");

    // The following mass formulas use 12 values:
    formulate("Xi.massLambda()",
      "sqrt(sq(Xi.ePosProton()+Xi.eNegPion())-Xi.Ptot2V0())");
    formulate("Xi.massAntiLambda()",
      "sqrt(sq(Xi.eNegAntiProton()+Xi.ePosPion())-Xi.Ptot2V0())");
    formulate("Xi.massK0Short()",
      "sqrt(sq(Xi.ePosPion()+Xi.eNegPion())-Xi.Ptot2V0())");
    
    // The following rapidity formulas use 8 values:
    formulate("Xi.rapLambda()",
    //  "0.5*log((Xi.eLambda()+Xi.momV0Z())/(Xi.eLambda()-Xi.momV0Z()))");
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eLambda()/Xi.momV0Z())-1.)))");
    formulate("Xi.rapK0Short()",
    //  "0.5*log((Xi.eK0Short()+Xi.momV0Z())/(Xi.eK0Short()-Xi.momV0Z()))");
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eK0Short()/Xi.momV0Z())-1.)))");

    // The following cTau formulas use 24 values:
    formulate("Xi.cTauLambda()",
      "Xi.massLambda()*Xi.decayLengthV0()/sqrt(Xi.Ptot2V0())");
    formulate("Xi.cTauK0Short()",
      "Xi.massK0Short()*Xi.decayLengthV0()/sqrt(Xi.Ptot2V0())");
    
    formulate("Xi.ptPos()", "sqrt(sq(Xi.mMomPosX)+sq(Xi.mMomPosY))");
    formulate("Xi.ptotPos()", "sqrt(Xi.Ptot2Pos())");
    formulate("Xi.ptNeg()", "sqrt(sq(Xi.mMomNegX)+sq(Xi.mMomNegY))");
    formulate("Xi.ptotNeg()", "sqrt(Xi.Ptot2Neg())");
    formulate("Xi.ptV0()", "sqrt(Xi.Pt2V0())");
    formulate("Xi.ptotV0()", "sqrt(Xi.Ptot2V0())");
    formulate("Xi.thetaV0()", "acos(Xi.momV0Z()/Xi.ptotV0())");
    formulate("Xi.pseudoRapV0()", "-log(tan(Xi.thetaV0()/2.))");
    formulate("Xi.psiV0()", "atan2(Xi.momV0Y(),Xi.momV0X())");

    // The following formulas use 1 value:
    formulate("Xi.chi2V0()", "Xi.mChi2V0");
    formulate("Xi.clV0()", "Xi.mClV0");
    formulate("Xi.chi2Pos()", "Xi.mChi2Pos");
    formulate("Xi.dedxPos()", "Xi.mDedxPos");
    formulate("Xi.numDedxPos()", "Xi.mNumDedxPos");
    formulate("Xi.clPos()", "Xi.mClPos");
    formulate("Xi.chi2Neg()", "Xi.mChi2Neg");
    formulate("Xi.clNeg()", "Xi.mClNeg");
    formulate("Xi.dedxNeg()", "Xi.mDedxNeg");
    formulate("Xi.numDedxNeg()", "Xi.mNumDedxNeg");


    // Now, finally get to the unique Xi formulas:
    // The following formula uses 6 values:
    // A straight line approximation to the path...
    formulate("Xi.decayLengthXi()",
      "sqrt(sq(Xi.mDecayVertexXiX-Event.mPrimaryVertexX[0])+sq(Xi.mDecayVertexXiY-Event.mPrimaryVertexY[0])+sq(Xi.mDecayVertexXiZ-Event.mPrimaryVertexZ[0]))");

    // The following formulas use 1 value:
    formulate("Xi.charge()", "Xi.mCharge");
    formulate("Xi.decayVertexXiX()", "Xi.mDecayVertexXiX");
    formulate("Xi.decayVertexXiY()", "Xi.mDecayVertexXiY");
    formulate("Xi.decayVertexXiZ()", "Xi.mDecayVertexXiZ");
    formulate("Xi.dcaXiDaughters()", "Xi.mDcaXiDaughters");
    formulate("Xi.dcaXiToPrimVertex()", "Xi.mDcaXiToPrimVertex");
    formulate("Xi.dcaBachelorToPrimVertex()", "Xi.mDcaBachelorToPrimVertex");
    formulate("Xi.momBachelorX()", "Xi.mMomBachelorX");
    formulate("Xi.momBachelorY()", "Xi.mMomBachelorY");
    formulate("Xi.momBachelorZ()", "Xi.mMomBachelorZ");
    formulate("Xi.momXiX()", "Xi.mMomBachelorX+Xi.momV0X()");
    formulate("Xi.momXiY()", "Xi.mMomBachelorY+Xi.momV0Y()");
    formulate("Xi.momXiZ()", "Xi.mMomBachelorZ+Xi.momV0Z()");
    formulate("Xi.keyBachelor()", "Xi.mKeyBachelor");

    formulate("Xi.radt2Xi()", "sq(Xi.mDecayVertexXiX)+sq(Xi.mDecayVertexXiY)");
    formulate("Xi.radtXi()", "sqrt(Xi.radt2Xi())");
    formulate("Xi.radXi()", "sqrt(Xi.radt2Xi()+sq(Xi.mDecayVertexXiZ))");
    formulate("Xi.phiXi()",
      "atan2((Xi.mDecayVertexXiY-Event.mPrimaryVertexY[0]),(Xi.mDecayVertexXiX-Event.mPrimaryVertexX[0]))");

    formulate("Xi.Ptot2Bachelor()",
      "(sq(Xi.mMomBachelorX)+sq(Xi.mMomBachelorY)+sq(Xi.mMomBachelorZ))");
    formulate("Xi.Pt2Xi()", "(sq(Xi.momXiX())+sq(Xi.momXiY()))");
    formulate("Xi.Ptot2Xi()", "(Xi.Pt2Xi()+sq(Xi.momXiZ()))");

    // The following momentum component formulas use 21 and 24 values:
    formulate("Xi.MomBachelorAlongXi()",
       "(((Xi.mMomBachelorX*Xi.momXiX())+(Xi.mMomBachelorY*Xi.momXiY())+(Xi.mMomBachelorZ*Xi.momXiZ()))/sqrt(Xi.Ptot2Xi()))");
    formulate("Xi.MomV0AlongXi()",
       "(((Xi.momV0X()*Xi.momXiX())+(Xi.momV0Y()*Xi.momXiY())+(Xi.momV0Z()*Xi.momXiZ()))/sqrt(Xi.Ptot2Xi()))");

    formulate("Xi.alphaXi()",
    //  "(Xi.mCharge*(Xi.MomBachelorAlongXi()-Xi.MomV0AlongXi())/(Xi.MomBachelorAlongXi()+Xi.MomV0AlongXi()))");
    // The above is cumbersome, using 50 values. The following uses 30:
      "Xi.mCharge*(1.-(2./(1.+(Xi.MomBachelorAlongXi()/Xi.MomV0AlongXi()))))");
    // The following formula uses 30 values:
    formulate("Xi.ptArmXi()",
      "sqrt(Xi.Ptot2V0()-sq(Xi.MomV0AlongXi()))");

    // The following energy formulas use 9 values:
    formulate("Xi.eXi()", "sqrt(Xi.Ptot2Xi()+sq(mXiMinus))");
    formulate("Xi.eOmega()", "sqrt(Xi.Ptot2Xi()+sq(mOmegaMinus))");
    formulate("Xi.eBachelorPion()", "sqrt(Xi.Ptot2Bachelor()+sq(mPiMinus))");
    formulate("Xi.eBachelorKaon()", "sqrt(Xi.Ptot2Bachelor()+sq(mKaonMinus))");

    // The following mass formulas use 18 values:
    formulate("Xi.massXi()",
      "sqrt(sq(Xi.eLambda()+Xi.eBachelorPion())-Xi.Ptot2Xi())");
    formulate("Xi.massOmega()",
      "sqrt(sq(Xi.eLambda()+Xi.eBachelorKaon())-Xi.Ptot2Xi())");
    
    // The following rapidity formulas use 12 values:
    formulate("Xi.rapXi()",
    //  "0.5*log((Xi.eXi()+Xi.momXiZ())/(Xi.eXi()-Xi.momXiZ()))");
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eXi()/Xi.momXiZ())-1.)))");
    formulate("Xi.rapOmega()",
    //  "0.5*log((Xi.eOmega()+Xi.momXiZ())/(Xi.eOmega()-Xi.momXiZ()))");
    // Can optimize for TTreeFormula:
      "0.5*log(1.+(2./((Xi.eOmega()/Xi.momXiZ())-1.)))");

    // The following cTau formulas use 33 values:
    formulate("Xi.cTauXi()",
      "Xi.massXi()*Xi.decayLengthXi()/sqrt(Xi.Ptot2Xi())");
    formulate("Xi.cTauOmega()",
      "Xi.massOmega()*Xi.decayLengthXi()/sqrt(Xi.Ptot2Xi())");
    
    formulate("Xi.ptBachelor()",
      "sqrt(sq(Xi.mMomBachelorX)+sq(Xi.mMomBachelorY))");
    formulate("Xi.ptotBachelor()", "sqrt(Xi.Ptot2Bachelor())");
    formulate("Xi.ptXi()", "sqrt(Xi.Pt2Xi())");
    formulate("Xi.ptotXi()", "sqrt(Xi.Ptot2Xi())");
    formulate("Xi.thetaXi()", "acos(Xi.momXiZ()/Xi.ptotXi())");
    formulate("Xi.pseudoRapXi()", "-log(tan(Xi.thetaXi()/2.))");
    formulate("Xi.psiXi()", "atan2(Xi.momXiY(),Xi.momXiX())");

    // Track topology maps, with function names like
    // "Xi.topologyMapNeg.*()", Xi.topologyMapPos.*(),
    // and "Xi.topologyMapBachelor.*()"
    if (tree->GetBranch("Xi.mTopologyMapBachelor.mMap0")) {
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
          formulate(name,expr);
        }

        // data(i), i=0,1, uses only 1 value.
        sprintf(name,"Xi.topologyMap%s.data(%d)",track,l);
	sprintf(expr,"Xi.mTopologyMap%s.mMap%d",track,l);
        formulate(name,expr);
      }

      // ftpcFormat()
      sprintf(ftpc,"Xi.topologyMap%s.ftpcFormat()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(63)",track);
      formulate(ftpc,expr);

      // primaryVertexUsed()
      sprintf(name,"Xi.topologyMap%s.primaryVertexUsed()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(0)",track);
      formulate(name,expr);

      // turnAroundFlag()
      sprintf(name,"Xi.topologyMap%s.turnAroundFlag()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(62)",track);
      formulate(name,expr);

      // hasHitInSvtLayer(i), i=1,6
      for (int j=1; j<7; j++) {
        sprintf(name,"Xi.topologyMap%s.hasHitInSvtLayer(%d)",track,j);
        sprintf(temp,"Xi.topologyMap%s.bit(%d)\0",track,j);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        formulate(name,expr);
	
        tstr += temp;
        if (j<6) tstr += "+";
      }

      // numOfSvtHits(), uses 6 values
      sprintf(name,"Xi.topologyMap%s.numOfSvtHits()\0",track);
      sprintf(expr,"(!%s)*(%s)\0",ftpc,tstr.Data());
      formulate(name,expr);
      tstr = "";
      
      // hasHitInSSD()
      sprintf(name,"Xi.topologyMap%s.hasHitInSSD()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(7)",track);
      formulate(name,expr);

      sprintf(temp,"(!%s)*(\0",ftpc);
      tstr = temp;

      // hasHitInTpcRow(i), i=0,44
      for (int j=0; j<45; j++) {
        sprintf(name,"Xi.topologyMap%s.hasHitInTpcRow(%d)",track,j);
	int m = j+8;
        sprintf(temp,"Xi.topologyMap%s.bit(%d)\0",track,m);
        sprintf(expr,"(!%s)&&(%s)\0",ftpc,temp);
        formulate(name,expr);
	
        tstr += temp;
        if (j<44) tstr += "+";
      }

      // numOfTpcHits(), uses 45 values.
      sprintf(name,"Xi.topologyMap%s.numOfTpcHits()\0",track);
      tstr += ")";
      formulate(name,tstr.Data());
      tstr = "";

      // hasHitInMwpc()
      sprintf(name,"Xi.topologyMap%s.hasHitInMwpc()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(53)",track);
      formulate(name,expr);

      // hasHitInCtb()
      sprintf(name,"Xi.topologyMap%s.hasHitInCtb()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(54)",track);
      formulate(name,expr);

      // hasHitInTofPatch()
      sprintf(name,"Xi.topologyMap%s.hasHitInTofPatch()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(55)",track);
      formulate(name,expr);

      // hasHitInRich()
      sprintf(name,"Xi.topologyMap%s.hasHitInRich()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(56)",track);
      formulate(name,expr);

      // hasHitInBarrelEmc()
      sprintf(name,"Xi.topologyMap%s.hasHitInBarrelEmc()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(57)",track);
      formulate(name,expr);

      // hasHitInEndcapEmc()
      sprintf(name,"Xi.topologyMap%s.hasHitInEndcapEmc()",track);
      sprintf(expr,"Xi.topologyMap%s.bit(58)",track);
      formulate(name,expr);

    } } // End topology maps

    // The following formulas use 1 value:
    formulate("Xi.chi2Xi()", "Xi.mChi2Xi");
    formulate("Xi.clXi()", "Xi.mClXi");
    formulate("Xi.chi2Bachelor()", "Xi.mChi2Bachelor");
    formulate("Xi.clBachelor()", "Xi.mClBachelor");
    formulate("Xi.dedxBachelor()", "Xi.mDedxBachelor");
    formulate("Xi.numDedxBachelor()", "Xi.mNumDedxBachelor");

  }  // End of Xi

  // XiMc
  if (tree->GetBranch("XiMc")) {
    printf("Loading XiMc formulas...\n");

    // The following formulas use 6 values:
    formulate("XiMc.decayLengthXi()",
      "sqrt(sq(XiMc.mPositionX-McEvent.mPrimaryVertexX[0])+sq(XiMc.mPositionY-McEvent.mPrimaryVertexY[0])+sq(XiMc.mPositionZ-McEvent.mPrimaryVertexZ[0]))");

    // The following formulas use 1 value:
    formulate("XiMc.decayMode()", "XiMc.mDecayMode");
    formulate("XiMc.commonTpcHits()", "XiMc.mCommonTpcHits");
    formulate("XiMc.simTpcHits()", "XiMc.mSimTpcHits");
    formulate("XiMc.geantIdParent()", "XiMc.mParentGeantId");
    formulate("XiMc.geantIdDaughter()", "XiMc.mDaughterGeantId");
    formulate("XiMc.parentMomentumX()", "XiMc.mParentMomentumX");
    formulate("XiMc.parentMomentumY()", "XiMc.mParentMomentumY");
    formulate("XiMc.parentMomentumZ()", "XiMc.mParentMomentumZ");
    formulate("XiMc.daughterMomentumX()", "XiMc.mDaughterMomentumX");
    formulate("XiMc.daughterMomentumY()", "XiMc.mDaughterMomentumY");
    formulate("XiMc.daughterMomentumZ()", "XiMc.mDaughterMomentumZ");
    formulate("XiMc.positionX()", "XiMc.mPositionX");
    formulate("XiMc.positionY()", "XiMc.mPositionY");
    formulate("XiMc.positionZ()", "XiMc.mPositionZ");

  }  // End of XiMc


  // Kink
  if (tree->GetBranch("Kink")) {
    printf("Loading Kink formulas...\n");

  }  // End of Kink

  // KinkMc
  if (tree->GetBranch("KinkMc")) {
    printf("Loading KinkMc formulas...\n");

    // The following formulas use 1 value:
    formulate("KinkMc.decayMode()", "KinkMc.mDecayMode");
    formulate("KinkMc.commonTpcHits()", "KinkMc.mCommonTpcHits");
    formulate("KinkMc.simTpcHits()", "KinkMc.mSimTpcHits");
    formulate("KinkMc.geantIdParent()", "KinkMc.mParentGeantId");
    formulate("KinkMc.geantIdDaughter()", "KinkMc.mDaughterGeantId");
    formulate("KinkMc.parentMomentumX()", "KinkMc.mParentMomentumX");
    formulate("KinkMc.parentMomentumY()", "KinkMc.mParentMomentumY");
    formulate("KinkMc.parentMomentumZ()", "KinkMc.mParentMomentumZ");
    formulate("KinkMc.daughterMomentumX()", "KinkMc.mDaughterMomentumX");
    formulate("KinkMc.daughterMomentumY()", "KinkMc.mDaughterMomentumY");
    formulate("KinkMc.daughterMomentumZ()", "KinkMc.mDaughterMomentumZ");
    formulate("KinkMc.positionX()", "KinkMc.mPositionX");
    formulate("KinkMc.positionY()", "KinkMc.mPositionY");
    formulate("KinkMc.positionZ()", "KinkMc.mPositionZ");

  }  // End of KinkMc
  
  Int_t finalFormulas = ListofFuncs->GetSize();
  return (finalFormulas-initialFormulas);

}
//______________________________________________________________________
// $Id: strangeFormulas.C,v 3.12 2008/03/05 15:47:25 genevb Exp $
// $Log: strangeFormulas.C,v $
// Revision 3.12  2008/03/05 15:47:25  genevb
// updated ROOT class for ListOfFunctions()
//
// Revision 3.11  2004/10/20 19:22:10  genevb
// Compatibility with small ROOT and CINT changes
//
// Revision 3.10  2004/04/12 19:50:06  genevb
// Allow use of list files
//
// Revision 3.9  2004/04/06 21:27:51  genevb
// Avoid PmdCollection branch
//
// Revision 3.8  2004/01/06 05:21:22  genevb
// Restore decay length functionality (was broken)
//
// Revision 3.7  2003/01/22 05:15:16  genevb
// Use TChain for multiple files
//
// Revision 3.6  2002/05/17 14:07:55  genevb
// Added some new event functions
//
// Revision 3.5  2002/04/30 01:29:18  genevb
// Updated macros for common micro DST
//
// Revision 3.4  2001/05/04 21:17:57  genevb
// Catch ROOT definition of TTreeFormula class, add McEvent branch
//
// Revision 3.3  2000/08/28 16:24:33  genevb
// Introduce findFormula(), handle executing on second tree
//
// Revision 3.2  2000/08/11 17:09:05  genevb
// Introduce findFormulas(), findDefinition(),  and some new formulas.
//
// Revision 3.1  2000/08/10 01:21:10  genevb
// Added number of dedx points
//
// Revision 3.0  2000/07/17 22:08:11  genevb
// Updated for rev. 3, fixed bug in number of tpc hits
//
// Revision 2.0  2000/06/09 22:13:45  genevb
// Updated for version 2 of Strangeness mico DST package
//
// Revision 1.7  2000/04/19 15:11:15  genevb
// Streamlined adding TTreeFormulas
//
// Revision 1.6  2000/04/13 19:33:44  genevb
// Removed redundant formulas
//
// Revision 1.5  2000/04/13 18:30:26  genevb
// Better handling of backward compatibility
//
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
