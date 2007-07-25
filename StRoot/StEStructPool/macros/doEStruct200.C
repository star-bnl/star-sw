void doEStruct200(const char* fileListFile, const char* outputDir, const char* cutFile, const char* jobName, int cutBinMode=0, int maxNumEvents=0, int analysisMode=0){

  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();
  gROOT->LoadMacro("getOutFileName.C"); // for laying out file names

  // define centrality
  StEStructCentrality* cent=StEStructCentrality::Instance();

  //int numCent = 2;
  //const double temp[2]={0,2000}; //=1 centrality

  //int numCent = 12;
  //const double temp[12]={2,15,35,68,117,187,281,401,551,739,852,2000};  // new 11 bins, not too different

  int numCent = 23;
  const double temp[23]={2,15,35,68,117,152,187,234,281,341,401,451,501,551,614,676,739,796,852,902,952,1002,2000};  //small slices
  //                    {2,15,35,68,117,    187,    281,    401,        551,        739,    852,             2000} 

  cent->setCentralities(temp,numCent);
  cent->Print();
  int numberOfAnalyses=numCent-1;

  // choose the mode for the binning
  StEStructCutBin* cb=StEStructCutBin::Instance();
  cb->setMode(cutBinMode);
  int mbNCutBins = cb->getNumBins();
 
  // create the low-level reader (here for MuDst)
  //  reader = reader interface + pointer to Data Maker + cut classes
  StMuDstMaker* mk = new StMuDstMaker(0,0,"",fileListFile,".",5000);

  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");

  // Set up the reader with event/track cuts
  StEStructEventCuts*    ecuts = new StEStructEventCuts(cutFile);
  StEStructTrackCuts*    tcuts = new StEStructTrackCuts(cutFile);
  StEStructMuDstReader* reader = new StEStructMuDstReader(mk,ecuts,tcuts);
  estructMaker->SetEventReader(reader);

  // create the QAHist object (must come after centrality and cutbin objects)
  int EventType = 0; // real data
  StEStructQAHists* qaHists = new StEStructQAHists(EventType);
  qaHists->initHistograms();
  estructMaker->SetQAHists(qaHists);

  StEStruct2ptCorrelations** analysis = new StEStruct2ptCorrelations*[numberOfAnalyses];
  StEStructPairCuts* pcuts = new StEStructPairCuts(cutFile);

  for(int i=0;i<numberOfAnalyses;i++){
   int ic=i;
   if(numberOfAnalyses==1)ic=-1;
   analysis[i]=new StEStruct2ptCorrelations(pcuts,analysisMode);
   analysis[i]->setOutputFileName(getOutFileName(outputDir,jobName,"data",ic));
   analysis[i]->setQAHists(qaHists); // only 1 QA Object in this case
   analysis[i]->setZBuffLimits(ecuts); // common to all
   analysis[i]->setAnalysisIndex(i);
  }
  estructMaker->SetAnalyses(analysis,numberOfAnalyses);

  // --- now do the work ---
    estructMaker->Init();
    estructMaker->startTimer();
    int counter=0, istat=0, i=0;

    while (istat!=2) {
      istat=estructMaker->Make(); // now includes filling qa histograms
      i++; counter++;
      if (counter==200) {
          cout<<"doing event ="<<i<<endl;
          counter=0;
          //estructMaker->writeDiagnostics(0);
      }
      if ( maxNumEvents!=0 && i>=maxNumEvents ) {
          istat=2;
      }
    }
    estructMaker->stopTimer();

  //--- now write out stats and cuts ---
   ofstream ofs(getOutFileName(outputDir,jobName,"stats"));
    estructMaker->logAllStats(ofs);
    ecuts->printCuts(ofs);
    tcuts->printCuts(ofs);
    pcuts->printCuts(ofs);
   ofs<<endl;
   ofs.close();
   
   // --> root cut file 
   TFile* tf=new TFile(getOutFileName(outputDir,jobName,"cuts"),"RECREATE");
   ecuts->writeCutHists(tf);
   tcuts->writeCutHists(tf);
   tf->Close();

   // --> root qa histogram file 
   estructMaker->writeQAHists(getOutFileName(outputDir,jobName,"QA"));

   // --- write out the data 
   estructMaker->Finish();
}

/**********************************************************************
 *
 * $Log: doEStruct200.C,v $
 * Revision 1.1  2007/07/25 17:08:57  msd
 * My analysis macros for 2001 200 GeV and 2004 62 GeV Au-Au data sets.
 *
 * Revision 1.8  2006/04/04 22:15:56  porter
 * a large number of changes were done to simplify the doEStruct macros
 * in the sense that these are now more similar and should be easier
 * for Duncan's GUI to build.  Here are some examples.
 *
 * Revision 1.7  2005/03/03 01:33:36  porter
 * modified macros
 *
 * Revision 1.6  2004/08/23 19:12:46  msd
 * Added note about usage for pre-compiled cut database
 *
 * Revision 1.5  2004/06/26 16:28:42  porter
 * fixed typo in getDirNames.C
 *
 * Revision 1.4  2004/06/25 03:14:55  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
 * Revision 1.3  2004/04/15 18:46:31  msd
 * Updated centrality variable types
 *
 * Revision 1.2  2003/11/21 06:26:40  porter
 * macros for running pythia
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
