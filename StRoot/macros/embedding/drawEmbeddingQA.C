
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
    const TString embeddingFile = "qa_embedding_2005_P07ie_D0.root",
    const TString realDataFile  = "qa_real_2005_P07ie_D0.root"
){
  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingQAMaker");

  StEmbeddingQADraw* maker = new StEmbeddingQADraw(embeddingFile, realDataFile);
  maker->SetOutputDirectory(outputDirectory);

  // Draw all QA plots
  maker->draw();

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
    const TString particleName
){
  gROOT->Macro("${STAR}/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMiniMcEvent");
  gSystem->Load("StEmbeddingQAMaker");

  StEmbeddingQADraw* maker = new StEmbeddingQADraw(embeddingFile, realDataFile, year, production, particleName);
  maker->SetOutputDirectory(outputDirectory);

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


