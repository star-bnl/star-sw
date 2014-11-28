/************************************************************************
 * $Id: writeEStructExample.C,v 1.3 2006/04/10 23:44:06 porter Exp $
 *
 * Author: Jeff Porter 
 *
 *  example code for reading in MuDst files contained int "fileListFile"
 *  and writng out estruct pico dsts using new format
 *
 *****************************************************************/

void writeEStructExample( const char* filelist,
                   const char* outputDir,
                   const char* scriptDir,
                   int maxNumEvents = 0 ) {


  gROOT->LoadMacro("load2ptLibs.C");
  load2ptLibs();

  char cutFile[1024];
  sprintf(cutFile,"%s/CutsFile.txt",scriptDir);

  gROOT->LoadMacro("getOutFileName.C"); // for laying out file names
  const char* scratchDir = "CuCu200data";

  // define centrality (just making stuff up below!!!)
  StEStructCentrality* cent=StEStructCentrality::Instance();
  const double mbBins[]={2,30,100,180,240,320,400,999999}; 
  int mbNBins=1+1+1+1+1+1+1+1;
  cent->setCentralities(mbBins,mbNBins);
  int numberOfAnalyses=mbNBins-1;

  // create the low-level reader (here for MuDst)
  //  reader = reader interface + pointer to Data Maker + cut classes

  //***** depends on PDSF vs RCF .... below is RCF version of scheduler
  //  char fileListFile[1024];
  //  sprintf(fileListFile,"%s/%s",scriptDir,filelist);
  StMuDstMaker* mk = new StMuDstMaker(0,0,"",filelist,".",5000);

  // create the generic EStructAnalysisMaker
  StEStructAnalysisMaker *estructMaker = new StEStructAnalysisMaker("EStruct2pt");

  // Set up the reader with event/track cuts
  StEStructEventCuts*    ecuts = new StEStructEventCuts(cutFile);
  StEStructTrackCuts*    tcuts = new StEStructTrackCuts(cutFile);
  StEStructMuDstReader* reader = new StEStructMuDstReader(mk,ecuts,tcuts,false);

  estructMaker->SetEventReader(reader);

  // create the QAHist object (must come after centrality and cutbin objects)
  int EventType = 0; // real data
  StEStructQAHists* qaHists = new StEStructQAHists(EventType);
  qaHists->initHistograms();
  estructMaker->SetQAHists(qaHists);

  StEStructEventWriter** analysis    = new StEStructEventWriter*[mbNBins];
  StEStructEventMaker**  writeMakers = new StEStructEventMaker*[mbNBins];

  int analysisMode = 0; // 2pt correlations mode selection

  for(int i=0;i<numberOfAnalyses;i++){
   int ic=i;
   if(numberOfAnalyses==1)ic=-1;

   TString writerName("EStructWriterMaker");  writerName+=i;
   writeMakers[i]=new StEStructEventMaker(writerName.Data());
   
   analysis[i]=new StEStructEventWriter(writeMakers[i]);
   analysis[i]->setOutputFileName(getOutFileName(outputDir,scratchDir,"event",ic));
   /*
   analysis[i]->setQAHists(qaHists); // only 1 QA Object in this case
   analysis[i]->setZBuffLimits(ecuts); // common to all
   */
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
          estructMaker->writeDiagnostics(0);
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
   ofs<<endl;
   ofs.close();
   
   // --> root cut file 
   TFile* tf=new TFile(getOutFileName(outputDir,scratchDir,"cuts"),"RECREATE");
   ecuts->writeCutHists(tf);
   tcuts->writeCutHists(tf);
   tf->Close();


   // --- write out the data 
   estructMaker->Finish();
}


/**********************************************************************
 *
 * $Log: writeEStructExample.C,v $
 * Revision 1.3  2006/04/10 23:44:06  porter
 * Updated the writer example - tested it on pp and CuCu at 200
 *
 * Revision 1.2  2004/04/15 18:46:34  msd
 * Updated centrality variable types
 *
 * Revision 1.1  2003/10/15 18:20:57  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/

























































