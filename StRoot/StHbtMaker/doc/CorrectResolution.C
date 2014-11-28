
#define USE_MICRODST


void CorrectResolution(Int_t nevents=100)
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");  // new addition 22jul99
    gSystem->Load("StAnalysisUtilities");  // needed by V0dstMaker
    gSystem->Load("StMagF");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker");
    gSystem->Load("StHbtMaker");

    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();


#ifndef USE_MICRODST
    
    StIOMaker* ioMaker = new StIOMaker("IO","r","/star/rcf/data07/reco/P00hg/2000/07/st_physics_*dst.root","bfcTree");
    ioMaker->SetDebug();

    ioMaker->SetIOMode("r");
    ioMaker->SetDebug();
    ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
    ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch


    StEventMaker* eventMaker = new StEventMaker("events","title");
    cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;
#endif



    // Now we add Makers to the chain...

    StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
    cout << "StHbtMaker instantiated"<<endl;


    /* -------------- set up of hbt stuff ----- */
    cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;

    StHbtManager* TheManager = hbtMaker->HbtManager();

    // here, we instantiate the appropriate StHbtEventReader




#ifdef USE_MICRODST
    StHbtBinaryReader* Reader = new StHbtBinaryReader("-","microDst.lis");
#else
    StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
    Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from
#endif


    // here would be the palce to plug in any "front-loaded" Event or Particle Cuts...
    TheManager->SetEventReader(Reader);


    StHbtVertexAnalysis* anal;
    mikesEventCut* FrontLoadedEvcut;
    mikesStarStandardEventCut* evcut;
    mikesTrackCut* trkcut;
    qualityPairCut* qpc;
    ManyPairCuts* MPC;
    EntranceSepPairCut* espc;
    StHbtCoulomb* cc;

    QinvCorrFctn* QinvCF;
    BPLCMSFrame3DCorrFctn* BpLcmsCF;
    char* QinvCF_name;
    char* BP_name;
    int MultLo,MultHi;
    float pTLo,pTHi;
    int charge;

    // SPEED IT UP -- FRONT-LOADED EVENT CUT and TRACK CUT

    FrontLoadedEvcut = new mikesEventCut;  
    FrontLoadedEvcut->SetEventMult(30,100000);
    FrontLoadedEvcut->SetVertZPos(-75.0,75.0);
    Reader->SetEventCut(FrontLoadedEvcut);

    // Removed Front-loaded Track cut

    cout << "READER SET UP.... " << endl;




    /* MOST CENTRAL PIMINUS hi-pT (5) */

    QinvCF_name = "Qinvmm3hi";
    BP_name = "BPLCMSmm3hi";
    MultLo = 174;
    MultHi = 500;
    pTLo = 0.325;
    pTHi = 0.45;
    charge = -1;

    anal = new StHbtVertexAnalysis(10,-75.0,75.0);
    // 1) set the Event cuts for the analysis
    evcut = new mikesStarStandardEventCut;
    evcut->SetEventMult(MultLo,MultHi);      
    evcut->SetVertZPos(-75.0,75.0);   
    anal->SetEventCut(evcut);         
    // 2) set the Track (particle) cuts for the analysis
    trkcut = new mikesTrackCut;  
    trkcut->SetNSigmaPion(-3.0,3.0); 
    trkcut->SetNSigmaKaon(-1000.0,1000.0); 
    trkcut->SetNSigmaProton(-1000.0,1000.0);
    trkcut->SetNHits(5,50);            
    trkcut->SetPt(pTLo,pTHi);
    trkcut->SetRapidity(-0.5,0.5);     
    trkcut->SetDCA(0.0,3.0);           
    trkcut->SetCharge(charge);             
    trkcut->SetMass(0.139);            
    anal->SetFirstParticleCut(trkcut); 
    anal->SetSecondParticleCut(trkcut);
    // 3) set the Pair cuts for the analysis - this one uses ManyPairCuts
    MPC = new ManyPairCuts;
    qpc = new qualityPairCut;
    qpc->SetQualityCut(-0.5,0.6);
    MPC->AddPairCut(qpc);
    espc = new EntranceSepPairCut;
    espc->SetEntranceSepRange(2.5,2000.0);
    MPC->AddPairCut(espc);
    anal->SetPairCut(MPC);         
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(10);        
    // 5) now set up the correlation functions that this analysis will make
    //    QinvCF = new QinvCorrFctn(QinvCF_name,40,0.0,0.20);
    //    anal->AddCorrFctn(QinvCF);
    BpLcmsCF = new BPLCMSFrame3DCorrFctn(BP_name,40,0.0,0.2);
    cc = new StHbtCoulomb;
    cc->SetRadius(5.0);
    BpLcmsCF->SetCoulombCorrection(cc);
    //
    // now set up momentumResolutionCorrection stuff
    //
    StHbtSmearPair* smear = new StHbtSmearPair;
    smear->SetFractionalPtRes(0.02);
    smear->SetPhiRes_a(0.00112);
    smear->SetPhiRes_b(0.000563);
    smear->SetPhiRes_alpha(-1.51);
    smear->SetThetaRes_a(0.00136);
    smear->SetThetaRes_b(0.000776);
    smear->SetThetaRes_alpha(-1.53);
    BpLcmsCF->SetSmearPair(smear);
    //
    BpLcmsCF->SetLambda(0.65);
    BpLcmsCF->SetRout(4.52);
    BpLcmsCF->SetRside(5.00);
    BpLcmsCF->SetRlong(5.43);


    anal->AddCorrFctn(BpLcmsCF);
    // 7) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);



    /* ------------------ end of setting up hbt stuff ------------------ */


  // now execute the chain member functions
  
  if (chain->Init()){ // This should call the Init() method in ALL makers
    cout << "Initialization failed \n";
    goto TheEnd;
  }
  chain->PrintInfo();


  // Event loop
  int istat=0,iev=1;
 EventLoop: if (iev <= nevents && !istat) {
   cout << "StHbtExample -- Working on eventNumber " << iev << " of " << nevents << endl;
   chain->Clear();
   istat = chain->Make(iev);
   if (istat) {cout << "Last event processed. Status = " << istat << endl;}
   iev++; goto EventLoop;
 }


  cout << "StHbtExample -- Done with event loop" << endl;

  chain->Finish(); // This should call the Finish() method in ALL makers

 TheEnd:

  TFile histoOutput("ThreeDHBT.root","recreate");

  for (int ia=0; ia<1; ia++){
    StHbtVertexAnalysis* Vanal = (StHbtVertexAnalysis*)(hbtMaker->HbtManager()->Analysis(ia));
//     QinvCorrFctn* qcf = (QinvCorrFctn*)(Vanal->CorrFctn(0));
//     qcf->Numerator()->Write();
//     qcf->Denominator()->Write();
//     qcf->Ratio()->Write();
    BPLCMSFrame3DCorrFctn* bpcf = (BPLCMSFrame3DCorrFctn*)(Vanal->CorrFctn(0));
    bpcf->Numerator()->Write();
    bpcf->Denominator()->Write();
    bpcf->Ratio()->Write();
    //
    bpcf->mIDNumHisto->Write();
    bpcf->mIDDenHisto->Write();
    bpcf->mIDRatHisto->Write();
    bpcf->mSMNumHisto->Write();
    bpcf->mSMDenHisto->Write();
    bpcf->mSMRatHisto->Write();
    bpcf->mCorrectionHisto->Write();
    bpcf->mCorrCFHisto->Write();
  }

  histoOutput.Close();


}



