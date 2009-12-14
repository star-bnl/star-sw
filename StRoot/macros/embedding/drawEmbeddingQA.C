
//______________________________________________________________________
//  Draw QA histograms
//    - Output filename are automatically determined by 
//      StEmbeddingQAMaker
//    - You don't need to put year, production and particlename. Those 
//      are obtained from the embedding output filename
//    - 'outputDirectory' is the directory where the output figures 
//      are printed.
void drawEmbeddingQA(
    const TString outputDirectory = "./",
    const TString embeddingFile = "qa_embedding_2005_P07ie.root",
    const TString realDataFile  = "qa_real_2005_P07ie.root",
    const Int_t geantid = 8
){
  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingQAMaker");

  StEmbeddingQADraw* maker = new StEmbeddingQADraw(embeddingFile, realDataFile, geantid);
  maker->setOutputDirectory(outputDirectory);

  // Flag for output figures (default is false, only png file will be printed)
//  maker->setGIFOn() ; // Print gif file
//  maker->setJPGOn() ; // Print jpg file
//  maker->setEPSOn() ; // Print eps file
//  maker->setPSOn() ;  // Print ps file

  // Draw all QA plots
//  maker->draw();

  // or draw each QA
  // Event-wise QA
  maker->drawEvent();

  // MC tracks
  maker->drawMcTrack();
 
  // Reconstructed track compared with real data
  // Either
  maker->drawTrack();
  // or
//  maker->drawGeantId();
//  maker->drawRapidity();
//  maker->drawMomentum();
//  maker->drawPt();
//  maker->drawdEdx();
//  maker->drawDca();
//  maker->drawNHit();
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
    const Int_t geantid
){
  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingQAMaker");

  StEmbeddingQADraw* maker = new StEmbeddingQADraw(embeddingFile, realDataFile, year, production, geantid);
  maker->setOutputDirectory(outputDirectory);

  // Draw all QA plots
//  maker->draw();

  // or draw each QA
  // Event-wise QA
  maker->drawEvent();

  // MC tracks
  maker->drawMcTrack();
 
  // Reconstructed track compared with real data
  // Either
  maker->drawTrack();
  // or
//  maker->drawGeantId();
//  maker->drawRapidity();
//  maker->drawMomentum();
//  maker->drawPt();
//  maker->drawdEdx();
//  maker->drawDca();
//  maker->drawNHit();
}


