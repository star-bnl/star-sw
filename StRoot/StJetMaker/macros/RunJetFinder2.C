
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
StChain *chain;
int total=0;

void RunJetFinder2(int nevents=100,
		   //const char* file="/star/data24/reco/productionPP/ReversedFullField/P04ik/2004/118/st_physics_adc_5118006_raw_2080001.MuDst.root",
		   const char* file="/dante3/starprod/reco/ppLong-1/FullField/P03ih/2003/137/st_physics_4137015_raw_0030012.MuDst.root",
		   //const char* file="/star/data45/reco/productionPP/ReversedFullField/P04ik/2004/117/st_physics_adc_5117052_raw_2060003.MuDst.root",
		   //const char* file="/dante/starprod/reco/productionPP/ReversedFullField/P04ik/2004/118/st_physics_5118056_raw_2020003.MuDst.root",
		   const char* outfile="blah.root",
		   const char* dir = "",
		   const char *filter = "")
{
    TString histfile(outfile);
    histfile.ReplaceAll(".root",".jethist.root");

    cout <<"Read file:\t"<<file<<endl;
    cout <<"Write file:\t"<<outfile<<endl;
    cout <<"jet hists:\t"<<histfile<<endl;
    
    if (gClassTable->GetID("TTable") < 0) {
	gSystem->Load("libStar");
	gSystem->Load("libPhysics");
    }
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    //gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StEEmcDbMaker");
    gSystem->Load("StEEmcUtil");// needed by EEMC-Db
    gSystem->Load("StJetFinder");
    gSystem->Load("StJetMaker");
    
    double pi = atan(1.0)*4.0;
    cout << " loading done " << endl;
    
    chain= new StChain("StChain"); 
    chain->SetDebug(1);
    
    //Instantiate the MuDstReader
    StMuDebug::setLevel(1);
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");
    
    //StMuDbReader...
    StMuDbReader* db = StMuDbReader::instance();
    
    //StMuDst2StEventMaker
    //StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");
    
    //Database
    St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb");

    //EmcDb
    StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");

    //EmcAdc2EMaker
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();

    //test Mike's new 4p maker:
    //here we also tag whether or not to do the swap:
    bool doTowerSwapFix = true;
    bool use2003TowerCuts = false;
    StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker, doTowerSwapFix);
    bet4pMaker->setUse2003Cuts(use2003TowerCuts);
    
    /*
    //test Mike's new 4p maker with Endcap (defualts to noEndcap)
    StBET4pMaker* bet4pMaker2 = new StBET4pMaker("BET4pMaker",muDstMaker,adc);
    bet4pMaker2->setUseEndcap(true);
    */
    
    //Instantiate the JetMaker
    StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker", muDstMaker, outfile);
    
    //Instantiate Jet Histogram Maker
    StJetHistMaker* jetHistMaker = new StJetHistMaker(muDstMaker, histfile.Data() );
    
    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(20); //track->nHitsFit()>20
    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
    anapars->setJetPtMin(5.0);
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);
  
    //Setup the kt finder for measured particles (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(0.7);
    ktpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ktpars, bet4pMaker, "KtJet");
    
    chain->PrintInfo();
    chain->Init();
    
    cout <<"\tLoop on branches"<<endl;
    
    TChain* muchain = muDstMaker->chain();
    assert(muchain);
    /*
      TObjArray* branches = muchain->GetListOfBranches();
      assert(branches);
      TObjArray branchesToKill;
      branchesToKill.Add( new TObjString("KingAssoc") );
      branchesToKill.Add( new TObjString("McKink") );
      branchesToKill.Add( new TObjString("Xi") );
      branchesToKill.Add( new TObjString("L3AlgoAccept") );
      branchesToKill.Add( new TObjString("McV0") );
      branchesToKill.Add( new TObjString("RichSpectra") );
      branchesToKill.Add( new TObjString("XiAssoc") );
      branchesToKill.Add( new TObjString("L3AlgoReject") );
      branchesToKill.Add( new TObjString("McXi") );
      branchesToKill.Add( new TObjString("StrangeCuts") );
      branchesToKill.Add( new TObjString("L3Tracks") );
      branchesToKill.Add( new TObjString("V0") );
      branchesToKill.Add( new TObjString("Kink") );
      branchesToKill.Add( new TObjString("McEvent") );
      branchesToKill.Add( new TObjString("OtherTracks") );
      branchesToKill.Add( new TObjString("V0Assoc") );
      for (int ib=0; ib<branches->GetLast()+1; ++ib) {
      TBranch* branch = dynamic_cast<TBranch*>((*branches)[ib]);
      if (!branch) {cout <<"\tNull branch"<<endl; abort();}
      const char* bname = branch->GetName();
      TString bnameString(bname);
      cout <<"\t--- Found branch:\t"<<bnameString<<endl;
      for (int jb=0; jb<branchesToKill.GetLast()+1; ++jb) {
      TObjString* tos = static_cast<TObjString*>( branchesToKill[jb] );
      TString btk = tos->GetString();
      //cout <<"\t\tcompare to:\t"<<btk<<endl;
      if (bnameString.Contains(btk)) {
      cout <<"\tdeactivating branch:\t"<<bname<<endl;
      muchain->SetBranchStatus(bname, 0);
	    }
	    }
	    }
    */
    
    for (Int_t iev=0; iev<nevents; iev++) {
	cout << "****************************************** " << endl;
	cout << "Working on eventNumber " << iev << endl;
	cout << "*************************1***************** " << endl;
	chain->Clear();
	TFile* currentFile = muchain->GetCurrentFile();
	if (currentFile) {
	    cout <<"analyzing file:\t"<<currentFile->GetName()<<endl;
	}
	int iret = chain->Make(iev); 
	total++;
	if (iret) {
	    cout << "Bad return code!" << endl;
	    break;
	}
    } 
    chain->Finish(); 
    cout << "****************************************** " << endl;
    cout << "total number of events  " << total << endl;
    cout << "****************************************** " << endl;      
}







    /*
    //set the analysis cuts: (see StJetMaker/StppJetAnalyzer.h -> class StppAnaPars )
    StppAnaPars* anapars = new StppAnaPars();
    anapars->setFlagMin(0); //track->flag() > 0
    anapars->setNhits(15); //track->nHitsFit()>15
    anapars->setCutPtMin(0.2); //track->pt() > 0.2
    anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
    anapars->setJetPtMin(5.0); //MLM, remember to change this back to 5!
    anapars->setJetEtaMax(100.0);
    anapars->setJetEtaMin(0);
    anapars->setJetNmin(0);
  
    //Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
    StConePars* cpars = new StConePars();
    cpars->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
    cpars->setConeRadius(0.7);
    cpars->setSeedEtMin(0.5);
    cpars->setAssocEtMin(0.1);
    cpars->setSplitFraction(0.5);
    cpars->setPerformMinimization(true);
    cpars->setAddMidpoints(true);
    cpars->setRequireStableMidpoints(true);
    cpars->setDoSplitMerge(true);
    cpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, cpars, emcFourPMaker, "MkConeJetsPt02R07");

    //Setup the cone finder (See StJetFinder/StCdfChargedConeJetFinder.h -> class StCdfChargedConePars)
    StCdfChargedConePars* ccdfpars = new StCdfChargedConePars();
    ccdfpars->setGridSpacing(56, -1.6, 1.6, 120, -pi, pi);
    ccdfpars->setConeRadius(0.7);
    ccdfpars->setSeedEtMin(1.0);
    ccdfpars->setAssocEtMin(0.1);
    ccdfpars->setDebug(false);
    emcJetMaker->addAnalyzer(anapars, ccdfpars, emcFourPMaker, "MkCdfChargedJetsPt02R07");
    

    //Setup the kt=finder (See StJetFinder/StKtCluFinder.h -> class StKtCluPars)
    StKtCluPars* ktpars = new StKtCluPars();
    ktpars->setR(0.7);
    ktpars->setDebug(false);
    
    //emcJetMaker->addAnalyzer(anapars, ktpars, emcFourPMaker, "MkKtJet");
    emcJetMaker->addAnalyzer(anapars, ktpars, bet4pMaker, "4pKtJet");
    //emcJetMaker->addAnalyzer(anapars, ktpars, bet4pMaker2, "EndcapKtJet");
    */
