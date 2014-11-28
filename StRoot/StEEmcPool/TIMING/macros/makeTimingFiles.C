/////////////////////////////////////////////////////////////////////////////
///
/// Analyzes timing-run muDst's. 
///
/// file   = muDst or file with list of muDst's
/// timing = timing delay (in ns please)
/// flavor = tower or mapmt.
/// dir    = directory in which we can find "file"
/// outdir = directory for output files
/// numberOfFilesInt = max number of muDst's to process
///
/// Once the code finishes, run macros/plotTiming.C on the output
/// directory to produce timing curves.  Output will be .gif and 
/// .ps files.
///
/////////////////////////////////////////////////////////////////////////////
void makeTimingFiles( Char_t *file="R6020047.lis",
		      Float_t timing= 8.00,
		      Int_t adc_cut=25, 
		      Char_t *flavor="tower",
		      Int_t numberOfFilesInt=100,
		      Char_t *dir ="./",
		      Char_t *outdir="towers/"
		      ) 
{
    

  //  cout << "RUNNING job " << jobId << ": reading " << numberOfFilesInt << " and writing output to "
  //       << scratchDir << endl;
  
  // load shared libraries
  // first MuDst libs.
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("libPhysics");
  }
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  

  // load my analysis library
  gSystem->Load("StEEmcPoolTIMING");  


  StChain *myChain = new StChain("myChain");

  StMuDstMaker *muDstMaker = new StMuDstMaker(0,0,dir,file,"MuDst.root",numberOfFilesInt);
  TChain* tree=muDstMaker->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  nEntries = 80000;  //kludge

  StMuEEmcCrateTimingMaker* myEEmcCrateTimingMaker = new StMuEEmcCrateTimingMaker(muDstMaker);
  myEEmcCrateTimingMaker->setFlavor(flavor);
  myEEmcCrateTimingMaker->setTiming(timing);
  myEEmcCrateTimingMaker->setDirectory(outdir);
  myEEmcCrateTimingMaker->setNumberOfChannels(adc_cut);
  myEEmcCrateTimingMaker->setMinCounts(100);
  myEEmcCrateTimingMaker->setPhase(6); 

  TString myOutName=file;
  myOutName.ReplaceAll("lis","");
  myOutName.ReplaceAll("MuDst.root","");
  myChain->PrintInfo();
  
  
  muDstMaker->SetStatus("*",0);
  muDstMaker->SetStatus("EztAll",1);
  if(myChain->Init() != 0) cerr << "Failure during Init()!" << endl;

  Int_t iRet = 0, runNumber = -1, nEventsAna = 0;
  while (iRet == 0) {
    myChain->Clear();
    iRet = myChain->Make();
    if (iRet) {
      cout << "rteutrn valuer snons ZEROOOOOOOOOOOOOOOOOOOOOOOOOOOOO" << endl;
      break;
    }
    if (nEventsAna % 100 == 0) {
      cout << "Analyzing event " << nEventsAna << endl;
    }
    nEventsAna++;
    if (nEventsAna > nEntries) break;
  }
  cout << "Analysed " << nEventsAna << " events" << endl;
  myChain->Finish();
  delete myChain;
}
