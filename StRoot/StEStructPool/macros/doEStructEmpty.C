void doEStructEmpty(const char* fileListFile, const char* outputDir, const char* cutFile, const char* jobName=0, int maxNumEvents=0){

  // libraries required and helper macros in $STAR/StRoot/StEStructPool/macros
  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  gROOT->LoadMacro("getOutFileName.C");
  gROOT->LoadMacro("support.C");

  // in this example, all cuts (event, track, pair) are in same file
  // cutFile can also refer to an entry in the cut DB 
  //    (see StEStructPool/AnalysisMaker/StEStructCuts.cxx for pre-compiled cuts)
  const char* evtCutFile=cutFile;
  const char* trackCutFile=evtCutFile;
  const char* pairCutFile =evtCutFile;

  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double temp[2]={0,2000}; //=1 centrality
  cent->setCentralities(temp,2);
  int nset = 1;  // no centrality bins 

  char** datadirs=getDirNames("data",nset);
  char** cutdirs=getDirNames("cuts",nset);

  // choose the mode for the binning
  StEStructCutBin* cb=StEStructCutBin::Instance();
  cb->setMode(0);

  // create the low-level reader (here for MuDst)
  StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",5000);   

  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");
  
  // Set up the EStruct data Readers and Writer codes
  char** outputFile                    = new char*[nset];
  StEStructEventCuts** ecuts           = new StEStructEventCuts*[nset];
  StEStructTrackCuts** tcuts           = new StEStructTrackCuts*[nset];
  //StEStruct2ptCorrelations**  analysis = new StEStruct2ptCorrelations*[nset];
  StEStructEmptyAnalysis**  analysis   = new StEStructEmptyAnalysis*[nset];
  StEStructMuDstReader** readers       = new StEStructMuDstReader*[nset];

  // only 1 reader actually reads the event, the others just pull from mem.
  // this 'skipMake' ensures this to be the case
  bool* skipMake=new bool[nset];
  skipMake[0]=false;
  for(int i=1;i<nset;i++)skipMake[i]=true;
  
  //  build the NSET readers & NSET analysis objects
  //  analysis = analysis interface & contains pair-cut object (needs file)
  //  reader = reader interface + pointer to StMuDstMaker + cut classes
  for(int i=0;i<nset;i++){
    
    //outputFile[i]=getOutFileName(outputDir,jobName,datadirs[i]);
    outputFile[i]=getOutFileName(outputDir,jobName,"data","");
    //analysis[i]=new StEStruct2ptCorrelations(mode);
    analysis[i]=new StEStructEmptyAnalysis();
    //analysis[i]->setCutFile(pairCutFile);  // not making pairs here
    analysis[i]->setOutputFileName(outputFile[i]); 
    
    /* here's the new way to set the centrality cut */
    
     ecuts[i]=new StEStructEventCuts(evtCutFile);
     /*
     int min=floor(cent->minCentrality(i));
     int max=floor(cent->maxCentrality(i));
     char** tmp=getNumTracksStrings(min,max);  // find in support.C, turns int's into array of strings
     // IMPORTANT: centrality cuts on refmult, which is what you need for STAR centrality bins
     // numTracks is the number of tracks passing cuts in an event, and has nothing to do with centrality
     //ecuts[i]->loadBaseCuts("centrality",tmp,2); 
     ecuts[i]->loadBaseCuts("numTracks",tmp,2); 
     */
     tcuts[i]=new StEStructTrackCuts(trackCutFile);
     readers[i]=new StEStructMuDstReader(mk,ecuts[i],tcuts[i],skipMake[i]);
     estructMaker->SetReaderAnalysisPair(readers[i],analysis[i]);
  }
 
  // --- now do the work ---
  doTheWork(estructMaker,maxNumEvents);

  //--- now write out stats and cuts ---
  //char* statsFileName=getOutFileName(outputDir,jobName,"stats");
  char* statsFileName=getOutFileName(outputDir,jobName,"stats","");
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

     ofs<<"  Not evaluating pairs in empty analysis"<<endl;
     /* StEStructPairCuts* pcuts=NULL;
     if(analysis){
        pcuts=&analysis[i]->getPairCuts();
        pcuts->printCuts(ofs);
	}*/

     // --> root cut file 
     //char* rootCutFile=getOutFileName(outputDir,jobName,cutdirs[i]);
     char* rootCutFile=getOutFileName(outputDir,jobName,"cuts","");
     TFile* tf=new TFile(rootCutFile,"RECREATE");
     ecuts[i]->writeCutHists(tf);
     tcuts[i]->writeCutHists(tf);
     //if(pcuts)pcuts->writeCutHists(tf);
     tf->Close();

  }
  ofs.close();

  // --- write out the data 
  estructMaker->Finish();
}





