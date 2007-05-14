
// this macro is not really for HBT, it is a tool requested by the
// reco group to estimate the track splittin in the TPC from the data
// it makes an upper and lower limit on its estimate of the splitting
// malisa 15aug2000 (updated 05nov2001)
// an example of an analysis done with this is at:
//http://sol.star.bnl.gov/star/starlib/doc/www/protected/hbt/lisa/SplittingEstimate/

class StChain;
StChain *chain=0;    

// keep pointers to Analysis global, so you can have access to them ...
class StHbtSplitEvalAnalysis;
StHbtSplitEvalAnalysis* anal;


// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.
Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath; 
TString  theFileName;
TString  originalPath;

 

//==========================================================================================
void StHbtSplittingEstimate(const Int_t nevents=9999,
			    const Char_t *path="/star/data01/pwg/hbt/RandTheMan/uDSTs/P01hi/minbias/08/",
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
  aTrackCut->SetPidProbPion(0.3,10);      // make it loose
  aTrackCut->SetNHits(10,50);             // range on number of TPC hits on the track
  aTrackCut->SetP(0.1,4.);              // range in P
  aTrackCut->SetPt(0.1,2.);             // range in Pt
  aTrackCut->SetEta(-1.1,+1.1);          // range in rapidity
  //  aTrackCut->SetRapidity(-10.,10.);          // range in rapidity
  aTrackCut->SetDCA(0.0,3.);             // range in Distance of Closest Approach to primary vertex
  aTrackCut->SetCharge(-1);              // want piminux
  aTrackCut->SetMass(0.138);             // pion mass
  // define example track cut monitor
  //  trackCutMonitor_P_vs_Dedx* aDedxMoniPos = new trackCutMonitor_P_vs_Dedx(+1,"P_vs_Dedx+","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,2.,100,0.,1e-5);
  //  trackCutMonitor_P_vs_Dedx* aDedxMoniNeg = new trackCutMonitor_P_vs_Dedx(-1,"P_vs_Dedx-","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,2.,100,0.,1e-5);
  
  // ****************************************** // 
  // 0) now define an analysis...
  anal = new StHbtSplitEvalAnalysis;
  anal->SetQinvCut(0.03);
  // 1) set the Event cuts for the analysis
  mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
  evcut->SetEventMult(30,10000);      // selected multiplicity range
  evcut->SetVertZPos(-70.0,70.0);    // selected range of vertex z-position
  anal->SetEventCut(evcut);          // this is the event cut object for this analsys
  // 2) set the Track (particle) cuts for the analysis
  anal->SetFirstParticleCut(aTrackCut);
  anal->SetSecondParticleCut(aTrackCut);  // identical-particle analysis...

  // 3) set the Pair cuts for the analysis
  qualityPairCut* qpc = new qualityPairCut;
  qpc->SetQualityCut(-0.5,0.65);
  anal->SetPairCut(qpc);

  // 4) set the number of events to mix (per event)
  anal->SetNumEventsToMix(1);        
  // 5) now set up the correlation functions that this analysis will make
  // THIS ANALYSIS WILL MAKE NO CORRELATION FUNCTIONS!!

  TheManager->AddAnalysis(anal);

  cout << " StHbt Analysis - setup done " << endl;
//   // ------------------ end of setting up hbt stuff ------------------ //
  
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
  
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "StHbtExample -- Working on eventNumber " << iev << endl;
    chain->Clear();
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
    if (iret) {
      // cout << "Bad return code!" << endl;
      break;
    }
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers
  
  TFile file("SplittingHistos.root","RECREATE");
  anal->mRealSplits->Write();
  anal->mRealAll->Write();
  anal->mMixedSplits->Write();
  anal->mMixedAll->Write();
  anal->mSplitFractionUpperLimit->Write();
  anal->mSplitFractionLowerLimit->Write();
  file.Close();  


  // plotting:
  TCanvas can;
  can->Divide(2,3);
  can->cd(1);
  anal->mRealAll->Draw();
  can->cd(2);
  anal->mRealSplits->Draw();
  can->cd(3);
  anal->mMixedAll->Draw();
  can->cd(4);
  anal->mMixedSplits->Draw();
  can->cd(5);
  anal->mSplitFractionUpperLimit->Draw();
  can->cd(6);
  anal->mSplitFractionLowerLimit->Draw();



}





