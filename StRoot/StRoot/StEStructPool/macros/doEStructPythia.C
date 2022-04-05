/************************************************************************
 * $Id: doEStructPythia.C,v 1.4 2006/04/04 22:15:57 porter Exp $
 *
 * Author: Jeff Porter 
 *
 *  example code for reading in MuDst files contained int "fileListFile"
 *  and running the 2pt correlations analysis, producing hist files in,
 *
 *  outputDir/jobName/data/XX/
 *  
 *    where XX = event level selection imposed by the cut files.
 *               This example (from PP) contains 3 selections on event mult
 *
 *************************************************************************/
void doEStructPythia( const char* filelist,
                const char* outputDir,
                const char* scriptDir,
                int maxNumEvents = 0 ) {

  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();

  gROOT->LoadMacro("loadPythiaLibs.C");
  loadPythiaLibs();
  gROOT->LoadMacro("getPythia.C");

  char* jobid=gSystem->Getenv("JOBID"); 
  const char* rframe="CMS";
  const char* cproj ="p";
  const char* ctarg ="p";
  float rts = 200.0;
  int pythiaTune = 0; // 1=TuneA, 2=TuneB, all else = minbias standard

  TPythia6* pythia=getPythia(rframe,cproj,ctarg,rts,pythiaTune,jobid); 

  char cutFile[1024];
  sprintf(cutFile,"%s/CutsFile.txt",scriptDir);

  gROOT->LoadMacro("getOutFileName.C"); // for laying out file names
  const char* scratchDir = "PPPythia";

  // define centrality
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double temp[2]={0,2000};
  cent->setCentralities(temp,2);
  int mbNBins=cent->numCentralities()-1;

  // choose the mode for the binning
  int cutBinMode = 1;
  StEStructCutBin* cb=StEStructCutBin::Instance();
  cb->setMode(cutBinMode);
  int mbNCutBins = cb->getNumBins();
  
  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2Pythia");

  // create the QAHist object (must come after centrality and cutbin objects)
  int EventType = 2; // pythia from generator
  StEStructQAHists* qaHists = new StEStructQAHists(EventType);
  qaHists->initHistograms();
  estructMaker->SetQAHists(qaHists);
 
  // Set up the reader with event/track cuts
  StEStructEventCuts*    ecuts = new StEStructEventCuts(cutFile);
  StEStructTrackCuts*    tcuts = new StEStructTrackCuts(cutFile);
  StEStructPythia*      reader = new StEStructPythia(pythia,ecuts,tcuts,false,0,maxNumEvents);

  estructMaker->SetEventReader(reader);

  StEStruct2ptCorrelations** analysis = new StEStructEmptyAnalysis*[mbNBins];
  //  StEStructPairCuts* pcuts = new StEStructPairCuts(cutFile);

  int analysisMode = 0; // 2pt correlations mode selection

  for(int i=0;i<mbNBins;i++){
   int ic=i;
   if(mbNBins==1)ic=-1;
   analysis[i]=new StEStructEmptyAnalysis;
   analysis[i]->setOutputFileName(getOutFileName(outputDir,scratchDir,"data",ic));
   analysis[i]->setQAHists(qaHists); // only 1 QA Object in this case
   analysis[i]->setZBuffLimits(ecuts); // common to all
  }
  estructMaker->SetAnalyses(analysis,mbNBins);
 

  // --- now do the work ---
    estructMaker->Init();
    estructMaker->startTimer();

    int counter=0, istat=0, i=0;

    while (istat!=2) {

      istat=estructMaker->Make(); // now includes filling qa histograms

      i++; counter++;
      if (counter==100) {
          cout<<"doing event ="<<i<<endl;
          counter=0;
      }
      if ( maxNumEvents!=0 && i>=maxNumEvents ) {
          istat=2;
      }
    }
    estructMaker->stopTimer();

  //--- now write out stats and cuts ---
   ofstream ofs(getOutFileName(outputDir,scratchDir,"stats"));
    estructMaker->logAllStats(ofs);
    ecuts->printCuts(ofs);
    tcuts->printCuts(ofs);
    pcuts->printCuts(ofs);
   ofs<<endl;
   ofs.close();
  
   // --> root cut file 
   TFile* tf=new TFile(getOutFileName(outputDir,scratchDir,"cuts"),"RECREATE");
   ecuts->writeCutHists(tf);
   tcuts->writeCutHists(tf);
   tf->Close();

   // --> root qa histogram file 
   estructMaker->writeQAHists(getOutFileName(outputDir,scratchDir,"QA"));

   // --- write out the data 
   estructMaker->Finish();
}

/**********************************************************************
 *
 * $Log: doEStructPythia.C,v $
 * Revision 1.4  2006/04/04 22:15:57  porter
 * a large number of changes were done to simplify the doEStruct macros
 * in the sense that these are now more similar and should be easier
 * for Duncan's GUI to build.  Here are some examples.
 *
 * Revision 1.3  2004/06/25 03:14:56  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
 * Revision 1.2  2004/04/15 18:46:33  msd
 * Updated centrality variable types
 *
 * Revision 1.1  2003/11/21 06:26:40  porter
 * macros for running pythia
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/


























































