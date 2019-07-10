
//______________________________________________________________________
//  Draw QA histograms
//    - Output filename are automatically determined by 
//      StEmbeddingQAMaker
//    - You don't need to put year, production and particlename. Those 
//      are obtained from the embedding output filename
//    - 'outputDirectory' is the directory where the output figures 
//      are printed.
//    - You can check the embedding output only by settting isEmbeddingOnly = kTRUE
void drawEmbeddingQA(
    const TString outputDirectory = "./",
    const TString embeddingFile = "qa_embedding_2005_P07ie.root",
    const TString realDataFile  = "qa_real_2005_P07ie.root",
    const Int_t geantid = 8,
    const Float_t ptMaxCut = 10.0, 
    const Bool_t isEmbeddingOnly = kFALSE,
    const Int_t parentGeantId = 0,
    const Float_t vzCut = 30.0, 
    const Int_t refMultMinCut = 0,
    const Int_t refMultMaxCut = 1000,
    const Float_t ptMinCut = 0.1,
    const Float_t etaMaxCut = 1.5,
    const Float_t yMaxCut = 10.0
){
  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingUtilities");

  // Set relevant cut parameters here used in the base QA
  // The default parameters can be found in the constructor of StRoot/StEmbeddingUtilities/StEmbeddingQAUtilities.cxx
  // You should have consistent parameters in both doEmbeddingQAMaker.C and drawEmbeddingQA.C
  // Below are the examples how to change the parameters
  // All values used here are default ones
  //
  // NOTE: These are just used to print them out in the pdf, not really affect the real QA process
  const StEmbeddingQAUtilities* utility = StEmbeddingQAUtilities::instance() ;
  utility->setPtMinCut(ptMinCut);
  utility->setPtMaxCut(ptMaxCut);
  utility->setEtaCut(etaMaxCut);
  //  utility->setNHitCut(10);
  //  utility->setNHitToNPossCut(0.51);
  //  utility->setDcaCut(3.0);
  //  utility->setNSigmaCut(2.0);
  utility->setRapidityCut(yMaxCut);
  utility->setRefMultMinCut(refMultMinCut);
  utility->setRefMultMaxCut(refMultMaxCut);
  utility->setZVertexCut(vzCut);

  // FIXME: the trigger ID selections has to be hard-coded below!!!
  // Default is no trigger cut, you can add multiple trigger id's like
  //  utility->addTriggerIdCut(290001);
  //  utility->addTriggerIdCut(290004);
  // FIXME: using BTof PID in selecting real data primary tracks (turn this on ONLY for checking the dE/dx match between data and MC)
  //  utility->setBTofPid(kTRUE);

  StEmbeddingQADraw* maker = new StEmbeddingQADraw(embeddingFile, realDataFile, geantid, isEmbeddingOnly);
  maker->setParentGeantId(parentGeantId) ;
  maker->init();
  maker->setOutputDirectory(outputDirectory);


  // Flag for output figures (default is false)
//  maker->setPNGOn() ; // Print png file
//  maker->setGIFOn() ; // Print gif file
//  maker->setJPGOn() ; // Print jpg file
//  maker->setEPSOn() ; // Print eps file
//  maker->setPSOn() ;  // Print ps file

  // Set maximum pt to be drawn
  maker->setPtMax(ptMaxCut+0.5) ;

  // Draw all QA plots
  maker->draw();

  // or draw each QA
  // Event-wise QA
//  maker->drawEvent();

  // MC tracks
//  maker->drawMcTrack();
 
  // Reconstructed track compared with real data
  // Either
//  maker->drawTrack();
  // or
//  maker->drawGeantId();
//  maker->drawRapidity();
//  maker->drawMomentum();
//  maker->drawPt();
//  maker->drawdEdx();
//  maker->drawDca();
//  maker->drawNHit();

  maker->finish() ;
}

//______________________________________________________________________
//    You need to put year, production and particlename
//    if you determine the output filename by hand in StEmbeddingQAMaker.
//    Please put the correct year, production and particleName since 
//    those are printed in the legend for each QA, and are used 
//    for the output figure name. 
void drawEmbeddingQA(
    const TString outputDirectory,
    const TString embeddingFile,
    const TString realDataFile,
    const Int_t year,
    const TString production,
    const Int_t geantid,
    const Double_t ptmax,
    const Bool_t isEmbeddingOnly = kFALSE,
    const Int_t parentgeantId = 0
){
  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingUtilities");

  StEmbeddingQADraw* maker = new StEmbeddingQADraw(embeddingFile, realDataFile, year, production, geantid, isEmbeddingOnly);
//  maker->setParentGeantId(parentGeantId) ;
  maker->init();
  maker->setOutputDirectory(outputDirectory);

  // Flag for output figures (default is false)
//  maker->setPNGOn() ; // Print png file
//  maker->setGIFOn() ; // Print gif file
//  maker->setJPGOn() ; // Print jpg file
//  maker->setEPSOn() ; // Print eps file
//  maker->setPSOn() ;  // Print ps file

  // Set maximum pt to be drawn
  maker->setPtMax(ptmax) ;

  // Draw all QA plots
  maker->draw();

  // or draw each QA
  // Event-wise QA
//  maker->drawEvent();

  // MC tracks
//  maker->drawMcTrack();
 
  // Reconstructed track compared with real data
  // Either
//  maker->drawTrack();
  // or
//  maker->drawGeantId();
//  maker->drawRapidity();
//  maker->drawMomentum();
//  maker->drawPt();
//  maker->drawdEdx();
//  maker->drawDca();
//  maker->drawNHit();

  maker->finish() ;
}


