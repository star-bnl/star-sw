void doEStructGevsim(int numEvents, const char* outputDir, const char* cutFile, const char* jobName=0, int cutBinMode=0, int analysisMode = 0){

  // libraries required and helper macros in $STAR/StRoot/StEStructPool/macros
  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  gROOT->LoadMacro("getOutFileName.C");
  gROOT->LoadMacro("support.C");

  const char* evtCutFile=cutFile;
  const char* trackCutFile=evtCutFile;
  const char* pairCutFile =evtCutFile;

  // For documentation on how to use gevsim, visit:
  // http://www.star.bnl.gov/protected/estruct/msd/gevsim.html

  // *** gevsim *** 
  gSystem->Load("libEG.so");
  gSystem->Load("libPhysics.so");
  gSystem->Load("StEStructPoolGevsim.so");
  gSystem->Load("StEStructPoolEventGenerators.so");

  // add particles
  TGeVSim *gener = new TGeVSim("GeVSim", 0);
  TGeVSimParticle *pip = new TGeVSimParticle(211,  TGeVSim::kLevy, 500, 0.2, 1, 0.0457);
  TGeVSimParticle *pim = new TGeVSimParticle(-211, TGeVSim::kLevy, 500, 0.2, 1, 0.0457);

  pip->SetEllipticSimple(0.05);
  pim->SetEllipticSimple(0.05);

  gener->AddParticleType(pip);
  gener->AddParticleType(pim);

  // event-by-event fluctuations
  //TF1 *mult = new TF1("gevsimMultRndm", "gaus(0)", 0,2);
  //mult->SetParameters(1, 1, 0.1);

  TF1 *temp = new TF1("gevsimTempRndm", "gaus(0)", 0,2);
  temp->SetParameters(1, 1, 0.015);

  TF1 *psi = new TF1("gevsimPsiRndm", "1", 0,360);

  gener->SetVerbose(kFALSE);
  gener->Print();
  // *************

  StEStructCentrality* cent=StEStructCentrality::Instance();
  //const double temp[2]={0,2000}; //=1 centrality
  //const double temp[2]={1,20}; //=1 centrality
  //cent->setCentralities(temp,2);
  //const double temp[13]={1,14,30,56,94,146,217,312,431,510,660,810,1500}; 
  //cent->setCentralities(temp,13);
  //int nset=cent->numCentralities()-1;
  int nset=1;

  char** datadirs=getDirNames("data",nset);
  char** cutdirs=getDirNames("cuts",nset);

  // choose the mode for the binning
  StEStructCutBin* cb=StEStructCutBin::Instance();
  cb->setMode(cutBinMode);

  // create the low-level reader (here for MuDst)
  //StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",5000);   

  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");

  // Set up the EStruct data Readers and Writer codes
  char** outputFile                    = new char*[nset];
  StEStructEventCuts** ecuts           = new StEStructEventCuts*[nset];
  StEStructTrackCuts** tcuts           = new StEStructTrackCuts*[nset];
  StEStruct2ptCorrelations**  analysis = new StEStruct2ptCorrelations*[nset];
  //StEStructMuDstReader** readers       = new StEStructMuDstReader*[nset];
  StEStructGevsim** readers            = new StEStructGevsim*[nset];
  

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool* skipMake=new bool[nset];
  skipMake[0]=false;
  for(int i=1;i<nset;i++)skipMake[i]=true;

  //  build the NSET readers & NSET analysis objects
  //  analysis = analysis interface & contains pair-cut object (needs file)
  //  reader = reader interface + pointer to StMuDstMaker + cut classes
  for(int i=0;i<nset;i++){

     outputFile[i]=getOutFileName(outputDir,jobName,datadirs[i]);
     analysis[i]=new StEStruct2ptCorrelations(analysisMode);
     analysis[i]->setCutFile(pairCutFile);
     analysis[i]->setOutputFileName(outputFile[i]);

     /* here's the new way to set the numTracks cut */
      ecuts[i]=new StEStructEventCuts(evtCutFile);
      //int min=floor(cent->minCentrality(i));
      //int max=floor(cent->maxCentrality(i));
      //char** tmp=getNumTracksStrings(min,max);  // find in support.C
      //ecuts[i]->loadBaseCuts("numTracks",tmp,2);
      //ecuts[i]->loadBaseCuts("centrality",tmp,2);
 
     tcuts[i]=new StEStructTrackCuts(trackCutFile);
     //readers[i]=new StEStructMuDstReader(mk,ecuts[i],tcuts[i],skipMake[i]);
     readers[i]=new StEStructGevsim(numEvents,gener,ecuts[i],tcuts[i]);
     estructMaker->SetReaderAnalysisPair(readers[i],analysis[i]);
  }
 
  // --- now do the work ---
  doTheWork(estructMaker,numEvents);

  //--- now write out stats and cuts ---
  char* statsFileName=getOutFileName(outputDir,jobName,"stats");
  ofstream ofs(statsFileName);

  ofs<<endl;
  estructMaker->logAnalysisTime(ofs);
  estructMaker->logInputEvents(ofs);
  estructMaker->logOutputEvents(ofs);
  estructMaker->logOutputRate(ofs);
  estructMaker->logAnalysisStats(ofs);

  for(int i=0;i<nset;i++){
    ofs<<"  *************** ";
    ofs<<" Cut Stats for Analysis Number = "<<i;
    ofs<<"  *************** "<<endl;
     ecuts[i]->printCuts(ofs);
     tcuts[i]->printCuts(ofs);

     StEStructPairCuts* pcuts=NULL;
     if(analysis){
        pcuts=&analysis[i]->getPairCuts();
        pcuts->printCuts(ofs);
     }

     // --> root cut file 
     char* rootCutFile=getOutFileName(outputDir,jobName,cutdirs[i]);
     TFile* tf=new TFile(rootCutFile,"RECREATE");
     ecuts[i]->writeCutHists(tf);
     tcuts[i]->writeCutHists(tf);
     if(pcuts)pcuts->writeCutHists(tf);
     tf->Close();

  }
  ofs.close();

  // --- write out the data 
  estructMaker->Finish();
}



























































