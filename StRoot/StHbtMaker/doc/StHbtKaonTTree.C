// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    

// keep pointers to Analysis global, so you can have access to them ...
class StHbtVertexAnalysis;
StHbtVertexAnalysis* kaonAnal;

// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.
Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath; 
TString  theFileName;
TString  originalPath;
//class StChain;
//StChain *chain=0;

const char *venusFile ="set*geant.root";
const char *venusPath ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfs_4/";
const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic.bnl.gov/star/data/samples/psc0054_07_40evts_dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *geantFile ="/disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0041_01_53evts.geant.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void wait(int n=1) {
  for ( int i=0; i<n*1e6; i++) { /*no-op*/ }
}
 

//==========================================================================================
void StHbtKaonTTree(const Int_t nevents=9999,
		    const Char_t *path="/star/data02/scratch/laue/dataSomeEyesOnly/",
		    const Char_t *fileName="",
		    const Char_t *extention=".hbtTTreeMuDst",
		    const Char_t *filter=".",
		    const int maxFiles=10) {

  gStyle->SetTextFont(41);
  gStyle->SetStatH(.3);
  gStyle->SetStatW(.3);
  
  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  gSystem->Load("St_emc_Maker");
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  //  gSystem->Load("StFlowMaker");
  //  gSystem->Load("StFlowTagMaker");
  //  gSystem->Load("StFlowAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StHbtMaker");   
  //  gSystem->Load("global_Tables");   
  
  // cout << " loading done " << endl;
  chain = new StChain("StChain"); 
  chain->SetDebug(1);
  
  
 // *********
 // Hbt Maker
 // *********
 
  StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
  StHbtManager* TheManager = hbtMaker->HbtManager();

  StHbtTTreeReader* Reader = new StHbtTTreeReader(0,0,path,fileName,extention,filter,maxFiles);
  TheManager->SetEventReader(Reader);
  
  // define example particle cut and cut monitors to use in the analyses
  // example particle cut
  franksTrackCut* aTrackCut = new franksTrackCut;  // use "frank's" particle cut object
  aTrackCut->SetPidProbKaon(0.8,10);   
  aTrackCut->SetNHits(23,50);             // range on number of TPC hits on the track
  aTrackCut->SetP(0.1,1.);              // range in P
  aTrackCut->SetPt(0.1,1.);             // range in Pt
  aTrackCut->SetEta(-1.1,+1.1);          // range in rapidity
  aTrackCut->SetRapidity(-10.,10.);          // range in rapidity
  aTrackCut->SetDCA(0.0,3.);             // range in Distance of Closest Approach to primary vertex
  aTrackCut->SetCharge(+1);              // want positive kaons
  aTrackCut->SetMass(0.494);             // kaon mass
  // define example track cut monitor
  trackCutMonitor_P_vs_Dedx* aDedxMoniPos = new trackCutMonitor_P_vs_Dedx(+1,"P_vs_Dedx+","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,2.,100,0.,1e-5);
  trackCutMonitor_P_vs_Dedx* aDedxMoniNeg = new trackCutMonitor_P_vs_Dedx(-1,"P_vs_Dedx-","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,2.,100,0.,1e-5);
  
  // now, we define another analysis that runs simultaneously with the previous one.
  // this one looks at K+K- correlations (so NONidentical particles) in invariant mass
  
  // ****************************************** // 
  // * franks phiLikeSign analysis - by Frank Laue, OSU //
  // ****************************************** // 
    // ****************************************** // 
  // * franks kaon analysis - by Frank Laue, OSU //
  // ****************************************** // 
  // 0) now define an analysis...
  //  StHbtVertexAnalysis* kaonAnal = new StHbtVertexAnalysis();
  kaonAnal = new StHbtVertexAnalysis(20,-50.,50.);
  //  kaonAnal->SetDebug(10);
  // 1) set the Event cuts for the analysis
  rotationEventCut* kaonEvcut = new rotationEventCut();  // use "mike's" event cut object
  kaonEvcut->SetEventRefMult(124,1000);      // selected multiplicity range
  kaonEvcut->SetVertZPos(-50.0,+50.0);    // selected range of vertex z-position
  kaonEvcut->RotationOff();   // turn of rotation
  kaonEvcut->SetSmear(0);    // selected range of vertex z-position
  kaonEvcut->RotationOff();    // selected range of vertex z-position
  eventCutMonitor_Mult* multMoniPass = new eventCutMonitor_Mult("mult","multiplicity",100,0.,5000);
  eventCutMonitor_Mult* multMoniFail = new eventCutMonitor_Mult("mult","multiplicity",100,0.,5000);
  kaonEvcut->AddCutMonitor(multMoniPass, multMoniFail);
  kaonAnal->SetEventCut(kaonEvcut);          // this is the event cut object for this analsys
  // 2) set the Track (particle) cuts for the analysis
  franksTrackCut* kaonTrackCut = new franksTrackCut(*aTrackCut);  // copy from example
  // new particle cut moni
  trackCutMonitor_P_vs_Dedx* dedxMoniNegPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
  trackCutMonitor_P_vs_Dedx* dedxMoniNegFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
  kaonTrackCut->AddCutMonitor( dedxMoniNegPass, dedxMoniNegFail);
  kaonAnal->SetFirstParticleCut(kaonTrackCut);  // this is the track cut for the "first" particle
  // copy second particle cut from first particle cut
  kaonAnal->SetSecondParticleCut(kaonTrackCut);  // this is the track cut for the "first" particle
  // 3) set the Pair cuts for the analysis
  franksPairCut* kaonPairCut = new franksPairCut;  // use "mike's" pair cut object
  kaonPairCut->SetQuality(-1,+0.65);         // this is the pair cut for this analysis
  kaonPairCut->SetEntranceSeparation(2.,1000.);         // this is the pair cut for this analysis
  kaonAnal->SetPairCut(kaonPairCut);         // this is the pair cut for this analysis
  // 4) set the number of events to mix (per event)
  kaonAnal->SetNumEventsToMix(5); 
  // ********************************************************************
  // 5) now set up the correlation functions that this analysis will make
  // ********************************************************************
  // define correlation function
  QinvCorrFctnPidProbWeight* Qinv = new QinvCorrFctnPidProbWeight("Qinv5MeV","Qinv5MeV",40,0.,+0.2); 
  kaonAnal->AddCorrFctn(Qinv);          // adds the just-defined correlation function to the analysis
  TheManager->AddAnalysis(kaonAnal);

  cout << " kaonAnal - setup done " << endl;
//   // ------------------ end of setting up hbt stuff ------------------ //
  
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
  
  // exit(); 
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "StHbtExample -- Working on eventNumber " << iev << endl;
    chain->Clear();
//  cout << "before make" << endl;
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
//    cout << "after make" << endl;
    if (iret) {
      // cout << "Bad return code!" << endl;
      break;
    }
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers
  
  TFile file("KaonPidProbWeight.root","RECREATE");
  StHbtHistoCollector* collector = StHbtHistoCollector::Instance();
  collector->Write();
  file.Close();  
}





