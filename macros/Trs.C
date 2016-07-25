void Trs(const Int_t nevents=100,
		 //		 const Char_t *fileIn = "/star/simu/simu/gstardata/rcf1197_78_5865evts.fzd"
		 const Char_t *fileIn ="/star/rcf/test/rcf0191_01_380evts.fzd",
		 const Char_t* rootFile="rcf0191_01_380evts.root"
		 ) {
  gROOT->LoadMacro("bfc.C"); 
  TString Chain("");
  if (! fileIn) Chain += "gstar,Y2003X";
  else          Chain += "fzin";
  Chain += ",trs,tpc,Cdst,Kalman,Event,-EventQA";//,evout,GeantOut";
  bfc(-1,Chain.Data(),fileIn,0,rootFile);
  
  StMcParameterDB* parameterDB = StMcParameterDB::instance();  
  // TPC
  parameterDB->setXCutTpc(.5); // 5 mm
  parameterDB->setYCutTpc(.5); // 5 mm
  parameterDB->setZCutTpc(.5); // 5 mm
  parameterDB->setReqCommonHitsTpc(3); // Require 3 hits in common for tracks to be associated
  St_geant_Maker *geant = (  St_geant_Maker * ) chain->Maker("geant");
  if (geant && ! fileIn)      geant->Do("gkine 50 14 1. 1. -4. 4. 0 6.28  0. 0.;");
  //  if (geant && ! fileIn)      geant->Do("gkine 1 6 1. 1. -.2 .2 0. 6.28  0. 0.;");
  StMaker *trs = chain->Maker("Trs");
  if (trs) trs->SetDebug(1);
  else     return;
  // now execute the chain member functions

  //  chain->PrintInfo();
  if (nevents >= 0)   {
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    if (initStat) chain->Fatal(initStat, "during Init()");
  }
  int istat=0,iev=1;
 EventLoop: if (iev<=nevents && istat!=2) {
   chain->Clear();
   cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
   if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
   iev++; goto EventLoop;
 } // Event Loop
#if 0
  StMcAnalysisMaker*  examples      = (StMcAnalysisMaker* ) chain->Maker("StMcAnalysisMaker");
  examples->mAssociationCanvas = new TCanvas("mAssociationCanvas", "Histograms",200,10,600,600);
  TCanvas* myCanvas = examples->mAssociationCanvas;
  myCanvas->Divide(2,2);
  
  myCanvas->cd(1);
  gPad->SetLogy(0);
  examples->mTrackNtuple->Draw("(p-prec)/p:commTpcHits","prec!=0");

  TList* dList = chain->GetMaker("StMcAnalysisMaker")->Histograms();
  TH2F* hitRes = dList->At(0);
  TH1F* momRes = dList->At(1);
  TH2F* coordRc = dList->At(2);
  TH2F* coordMc = dList->At(3);
    
  myCanvas->cd(2);
  gPad->SetLogy(0);
  hitRes->Draw("box");
  
  myCanvas->cd(3);
  gPad->SetLogy(0);
  momRes->Draw();
  
  myCanvas->cd(4);
  gPad->SetLogy(0);
  coordRc->SetMarkerStyle(20);
  coordRc->Draw();
    
  myCanvas->cd(4);
  gPad->SetLogy(0);
  coordMc->SetMarkerColor(2);
  coordMc->SetMarkerStyle(20);
  coordMc->Draw("same");
  
  if(iev>200) chain->Finish(); // This should call the Finish() method in ALL makers,
  // comment it out if you want to keep the objects
  // available at the command line after running
  // the macro.

  // To look at the ntuple after the macro has executed:
  // f1 = new TFile("TrackMapNtuple.root");  //This opens the file, and loads the Ntuple
  // TrackNtuple->Draw("px:pxrec")  //Once loaded, the Ntuple is available by name.
  // To look at the Histograms once the Macro has executed:
  // TList* dList = chain->GetMaker("McAnalysis")->Histograms();
  // TH2F* hitRes = dList->At(0);  //or whatever index from 0 to 3
#endif
}

