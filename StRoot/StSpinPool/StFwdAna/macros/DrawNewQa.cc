/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  To draw the histograms generated from #StMuFcsRun22QaMaker and #StMuEpdRun22QaMaker

  DESCRIPTION
  Read the genereated root file and extract histograms then draw them. The drawing functions have been written in the maker. Some draw functions generate multiple pages and should be called with *pdf* save names.

  LOG
  @[June 5, 2024] > First instance where you provide the file name, and it loads libraries, creates the maker and then calls the draw functions defined in the maker.

  @[July 30, 2024] > Extended to draw the new EPD vertex QA histograms.

  @[August 29, 2024] > Changes to accommodate the spltting of #StFcsRun22QaMaker in to #StMuFcsRun22QaMaker and #StMuEpdRun22QaMaker. Also changes to plot new histograms and the new draw function calls.

  @[September 16, 2024] > Added drawing just the trigger histogram with the trigger names as bin labels

  @[September 26, 2024] > Loads StFcsTreeManager since it is needed by StFcsPi0Ana

  @[November 27, 2024] > Draws spin info related histograms from StMuFcsRun22QaMaker

  @[March 21, 2025] > Added Draw function without ZDC since not using it for this analysis
  @[August 11, 2025] > Turned off BestMass in Run22QaMaker
  @[May 14, 2026] > Added StMuFcsAnaEpdFcsMixedEvent to the list of data makers and added its related painting methods. Fixed the code so that if a certain analysis maker doesn't exist it doesn't paint it
  @[June 5, 2026] > Added code related to #StMuFcsAnaEpdMatchQa
  @[June 10, 2026] > Added code for new #StMuFcsAnaEpdMakePairsQa. Fixed some checks of null pointers from unused analysis modules so those plot methods don't get called
  @[July 2, 2026] > Got rid of dependency to my 'Rtools' by moving AddHists() to #StFwdAnaData
 */


void DrawNewQa(const char* filename="/gpfs/mnt/gpfs01/star/pwg/dkap7827/FcsRun22Qa/TEST/condor/StFcsPi0Maker_23022033_0000002.root")
{
  gROOT->Macro("Load.C");
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StEventMaker");
  gSystem->Load("StFcsDbMaker");
  //gSystem->Load("StFcsRawHitMaker");
  //gSystem->Load("StFcsClusterMaker");
  gSystem->Load("StEpdUtil");
  gSystem->Load("libMinuit");
  gSystem->Load("StFcsPointMaker");

  gSystem->Load("StFwdData");
  gSystem->Load("StFwdAna");
  StFwdAnaData* fwdanadata = new StFwdAnaData();
  fwdanadata->setTreeOnBit(0);
  fwdanadata->setRandomSeed(time(0));
  fwdanadata->setEpdNmipCut(0.7);
  //fwdanadata->setIgnoreTrig();
  
    
  HistManager* treehists = new HistManager();
    
  StFwdAnaDataMaker* fwddatamkr = new StFwdAnaDataMaker();
  fwddatamkr->setAnaData(fwdanadata);
  fwddatamkr->setPolDataFilename("Run22PolForJobs.txt");
  fwddatamkr->setFcsTrigFilename("FcsSortedTrig.txt");
  //fwddatamkr->setOutFilename(filenametree.Data();   //Only do this if an external hist manager was not declared and the file was not initialized like above
  fwddatamkr->setHistManager(treehists);

  StFwdAnaPolarization* polana = new StFwdAnaPolarization();
  StFwdAnaSpin*spinana =  new StFwdAnaSpin();
  
  fwddatamkr->addAna(polana);
  fwddatamkr->addAna(spinana);
  StFwdAnaFcsRun22Qa* fcsqa = new StFwdAnaFcsRun22Qa();
  fcsqa->setFcsAdcTbOn(false);
  fcsqa->setEpdAdcQaOn(false);
  fcsqa->setEpdTacQaOn(false);
  //fcsqa->setEpdTacAdcOn(false);
  fcsqa->setBestMassOn(false);
  fwddatamkr->addAna(fcsqa);
  
  StFwdAnaEpdQaAndVert* epdqa = new StFwdAnaEpdQaAndVert();
  epdqa->setEpdTacAdcOn(false);
  fwddatamkr->addAna(epdqa);
  StFwdAnaVertex* vertexana = new StFwdAnaVertex();
  StFwdAnaCheckTrig* trigana = new StFwdAnaCheckTrig();
  StFwdAnaFillEcalClusPoint* fillcluspointana = new StFwdAnaFillEcalClusPoint();
  StFwdAnaEpdMatch* epdmatchana = new StFwdAnaEpdMatch();
  StFwdAnaEpdMatchQa* epdmatchqa = new StFwdAnaEpdMatchQa();
  StFwdAnaEpdFcsMixedEvent* epdfcsmix = new StFwdAnaEpdFcsMixedEvent();
  StFwdAnaMakeEcalPairs* makepairsana = new StFwdAnaMakeEcalPairs();
  StFwdAnaEcalPairQa* pairqa = new StFwdAnaEcalPairQa();
  StFwdAnaEcalPi0Tssa* pi0tssaana = new StFwdAnaEcalPi0Tssa();

  fwddatamkr->addAna(vertexana);
  fwddatamkr->addAna(trigana);
  fwddatamkr->addAna(fillcluspointana);
  fwddatamkr->addAna(epdmatchana);
  fwddatamkr->addAna(epdmatchqa);
  fwddatamkr->addAna(epdfcsmix);
  fwddatamkr->addAna(makepairsana);
  fwddatamkr->addAna(pairqa);
  fwddatamkr->addAna(pi0tssaana);
  
  TFile* infile = treehists->InitFile(filename,"READ");
  UInt_t histsloaded = fwddatamkr->LoadDataFromFile(infile);
  std::cout << "|TotalHistsLoaded:"<<histsloaded << std::endl;

  gStyle->SetOptStat(111111);
  TCanvas* canvas = new TCanvas("canvas","",1920,1080);

  polana->PaintPolarization(canvas,"FcsAna_Polarization.png");
  fcsqa->DrawTrigger(canvas,"FcsAna_QaTrigger.png");

  //canvas->Print("TestAdcVTb.pdf[");
  //fcsqamkr->DrawAdcVTb(canvas,"TestAdcVTb.pdf");
  //canvas->Print("TestAdcVTb.pdf]");
  /*
  canvas->Print("TestFcsHitQa.pdf[");
  fcsqamkr->DrawFcsHitQa(canvas,"TestFcsHitQa.pdf");
  canvas->Print("TestFcsHitQa.pdf]");

  canvas->Print("TestEpdHitQa.pdf[");
  fcsqamkr->DrawEpdHitQa(canvas,"TestEpdHitQa.pdf");
  canvas->Print("TestEpdHitQa.pdf]");

  canvas->Print("TestFcsClusterQa.pdf[");
  fcsqamkr->DrawFcsClusterQa(canvas,"TestFcsClusterQa.pdf");
  canvas->Print("TestFcsClusterQa.pdf]");

  fcsqamkr->DrawFcsClusterPi0(canvas,"TestFcsClusterPi0.pdf");

  canvas->Print("TestFcsPointQa.pdf[");
  fcsqamkr->DrawFcsPointQa(canvas,"TestFcsPointQa.pdf");
  canvas->Print("TestFcsPointQa.pdf]");

  fcsqamkr->DrawFcsPointPi0(canvas,"TestFcsPointPi0.pdf");
  */
  if( fcsqa!=0 ){
    fcsqa->DrawBxId(canvas,"FcsAna_QaBxId.png");
    fcsqa->DrawBx7Bx48Ana(canvas,"FcsAna_QaBxAna.png");
    fcsqa->DrawSpinInfo(canvas,"FcsAna_QaSpinInfo.png");
    fcsqa->DrawFcsHitSingle(canvas,0,"FcsAna_QaHitDet0.png");
    fcsqa->DrawFcsHitSingle(canvas,1,"FcsAna_QaHitDet1.png");
    fcsqa->DrawFcsHitSingle(canvas,2,"FcsAna_QaHitDet2.png");
    fcsqa->DrawFcsHitSingle(canvas,3,"FcsAna_QaHitDet3.png");
    fcsqa->DrawFcsHitSingle(canvas,4,"FcsAna_QaHitDet4.png");
    fcsqa->DrawFcsHitSingle(canvas,5,"FcsAna_QaHitDet5.png");
    fcsqa->DrawFcsTotalE(canvas,"FcsAna_QaEcalTotalE.png");  
    fcsqa->DrawFcsClusterSingle(canvas,0,"FcsAna_QaClusterDet0.png");
    fcsqa->DrawFcsClusterSingle(canvas,1,"FcsAna_QaClusterDet1.png");
    fcsqa->DrawFcsClusterSingle(canvas,2,"FcsAna_QaClusterDet2.png");
    fcsqa->DrawFcsClusterSingle(canvas,3,"FcsAna_QaClusterDet3.png");
    fcsqa->DrawFcsPointSingle(canvas,0,"FcsAna_QaPointDet0.png");
    fcsqa->DrawFcsPointSingle(canvas,1,"FcsAna_QaPointDet1.png"); 
    fcsqa->DrawFcsClusterPi0(canvas,"FcsAna_QaClusterPi0.pdf");
    fcsqa->DrawFcsPointPi0(canvas,"FcsAna_QaPointPi0.pdf");
  }

  if( epdqa!=0 ){
    //epdqa->DrawVertex(canvas,"FcsAna_EpdVertex.png");
    //epdqa->DrawEpdAllQa(canvas,"FcsAna_EpdAllQa.png");
    epdqa->DrawEpdHitQa(canvas,"FcsAna_EpdHitQa.png");
    epdqa->DrawEpdTacCutQa(canvas,"FcsAna_EpdTacCutQa.png");
    //epdqa->DrawVertexNoZdc(canvas,"FcsAna_EpdVertexNoZdc.png");
  }
  
  if( vertexana!=0 ){
    vertexana->DrawVertex(canvas,"FcsAna_Vertex.png");
    vertexana->DrawVertexCorrelation(canvas,"FcsAna_VertexCorrelations.png");
    vertexana->DrawVertexCorrelationNoZdc(canvas,"FcsAna_VertexCorrelationsNoZdc.png");
  }
  
  if( trigana!=0 ){ trigana->PaintMatchTriggers(canvas,"FcsAna_MatchTriggers.png"); }

  if( fillcluspointana!=0 ){
    fillcluspointana->PaintHeatMap(canvas,"FcsAna_PointHeatMap.png");
    fillcluspointana->PaintClusPointQa(canvas,"FcsAna_ClusPointQa.png");
    fillcluspointana->PaintEnergyZoom(canvas,"FcsAna_EnergyZoom.png");
  }
  
  if( epdmatchana!=0 ){ epdmatchana->PaintEpdProjections(canvas,"FcsAna_EpdProjections.png"); }

  if( epdmatchqa!=0 ){
    epdmatchqa->PaintProjEpdAdjQa(canvas,"FcsAna_ProjEpdAdjQa.png");
    epdmatchqa->PaintBadProjections(canvas, "FcsAna_EpdBadProjQa.png");
    epdmatchqa->PaintFoundChargeId(canvas, "FcsAna_EpdFoundChargeId.png");
  }

  if( epdfcsmix!=0 ){
    epdfcsmix->PaintEpdAllDistQa(canvas,"FcsAna_EpdAllDistQa.png");
    epdfcsmix->PaintEpdAllDistQaLowMult(canvas,"FcsAna_EpdAllDistQaLowMult.png" );
    epdfcsmix->PaintEpdTileDistQa(canvas,"FcsAna_EpdTileDistQa.png");
    epdfcsmix->PaintEpdDistAnaQa(canvas,"FcsAna_EpdDistAnaQa.png");
  }
  
  if( pairqa!=0 ){
    pairqa->PaintPairEnergy(canvas,"FcsAna_PairEnergy.png");
    //canvas->Clear();
    //canvas->Print("FcsAna_EpdNmipCuts.pdf[");
    //makepairsana->PaintEpdNmipCuts(canvas,"FcsAna_EpdNmipCuts.pdf");
    //canvas->Print("FcsAna_EpdNmipCuts.pdf]");
  }

  if( pi0tssaana!=0 ){
    pi0tssaana->PaintAllPi0(canvas,"FcsAna_AllPi0.png");
    pi0tssaana->PaintNoEpdCut(canvas,"FcsAna_Pi0NoEpdCut.png");
    pi0tssaana->PaintEpdPhPi0(canvas,"FcsAna_EpdPhPi0.png");
    pi0tssaana->PaintEpdChPi0(canvas,"FcsAna_EpdChPi0.png");
    pi0tssaana->PaintEpdSinglePh(canvas,"FcsAna_SinglePh.png");
    pi0tssaana->PaintEpdSingleCh(canvas,"FcsAna_SingleCh.png");
    pi0tssaana->PaintPi0Overlap(canvas,"FcsAna_Pi0Overlap.png");
    pi0tssaana->PaintInvMassEpdQa(canvas,"FcsAna_Pi0InvMassEpdQa.png");
    pi0tssaana->PaintEpdQa(canvas,"FcsAna_Pi0EpdQa.png");
    pi0tssaana->PaintPi0Cuts(canvas,"FcsAna_Pi0Cuts.png");
    pi0tssaana->PaintInvMassCuts(canvas,"FcsAna_Pi0InvMassCuts.png");
    pi0tssaana->PaintNpi0Inc(canvas,"FcsAna_Npi0Inc.png");
    pi0tssaana->PaintNpi0Bg1(canvas,"FcsAna_Npi0Bg1.png");
    pi0tssaana->PaintNpi0Bg2(canvas,"FcsAna_Npi0Bg2.png");
  
    pi0tssaana->PaintAllHistOneTrigger(canvas,0,"FcsAna_testAllHistEmAllTrig.png");
    pi0tssaana->PaintAllHistOneTrigger(canvas,1,"FcsAna_testAllHistEm0Trig.png");
    pi0tssaana->PaintAllHistOneTrigger(canvas,2,"FcsAna_testAllHistEm1Trig.png");
    pi0tssaana->PaintAllHistOneTrigger(canvas,3,"FcsAna_testAllHistEm2Trig.png");
    pi0tssaana->PaintAllHistOneTrigger(canvas,4,"FcsAna_testAllHistEm3Trig.png");

    pi0tssaana->PaintAllTrigInvMass(canvas);
    pi0tssaana->PaintAllTrigPi0Mult(canvas);
    pi0tssaana->PaintAllTrigxF(canvas);
    pi0tssaana->PaintAllTrigxFZoom(canvas);
    pi0tssaana->PaintAllTrigZgg(canvas);
    pi0tssaana->PaintAllTrigDgg(canvas);
    pi0tssaana->PaintAllTrigPi0En(canvas);
    pi0tssaana->PaintAllTrigPi0massVen(canvas);
    pi0tssaana->PaintAllTrigPi0xfVen(canvas);
    pi0tssaana->PaintAllTrigPi0ptVeta(canvas);
    pi0tssaana->PaintAllTrigPi0etaVphi(canvas);
    pi0tssaana->PaintAllTrigPi0yVx(canvas);
  
    canvas->Clear();
    canvas->cd()->SetLogy();
  }
  TH1* badproj = treehists->Find("H1F_NBadEpdProj");
  TH1* badprojvcut = treehists->Find("H1F_NBadEpdProjVcut");
  if( badproj!=0 && badprojvcut!=0 ){
    TLegend* leg = new TLegend(0.7,0.5,0.99,0.95,"","nbNDC");
    badproj->SetLineColor(kBlack);
    badprojvcut->SetLineColor(kBlue);
    badproj->Draw("hist e");
    badprojvcut->Draw("hist e same");
    StFwdAnaData::AddHistStats(leg,badproj,"No Vertex Cut");
    StFwdAnaData::AddHistStats(leg,badprojvcut,"|vertex|<150");
    leg->Draw();
    canvas->SaveAs("FcsAna_BadEpdProjHits.png");
  }

  canvas->Clear();
  //pi0tssaana->PaintPhotonQaForDefense(canvas,"testphoton_defense.png");
  //pi0tssaana->PaintPi0QaForDefense(canvas,"testpi0cuts_defense.png");
  
  delete canvas;
  infile->Close();
  //@[August 20, 2024] > Need to delete in reverse creation order to not seg fault. This may be because of how StMaker is intialized and destructed.
  delete fwddatamkr; //Will delete StFwdAnaData and all analysis modules
  delete treehists; //Deletes file pointer

}
