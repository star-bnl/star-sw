// forward declarations
class StChain;
class StMuDstMaker;    

// global variables
StChain            *analysisChain = 0;

void runTree(char* dir="hist_pAu2", char* filter="16142010", unsigned int neventsIn = 0){  
  // If nEvents is negative, reset to the maximum possible value for an Int_t
  if( neventsIn <= 0 ) neventsIn = 1<<31-1;

  TSystemDirectory DIR(dir, dir);
  TList *files = DIR.GetListOfFiles();
  TChain* trees = new TChain();
  int nfile=0;
  if (files) {
      TSystemFile *file;
      TString fname;
      TIter next(files);
      while ((file=(TSystemFile*)next())) {
	  fname = file->GetName();
	  if (!file->IsDirectory() && fname.BeginsWith(filter) && fname.EndsWith("tree.root")) {
	      cout << Form("Adding %s/%s to TChain",dir,fname.Data())<<endl;
	      trees->AddFile(Form("%s/%s/dipi0",dir,fname.Data()));
	      nfile++;
	  }
      }
  }
  cout << Form("%d files added",nfile) << endl;

  // load the shared libraries
  std::cout << "***** Loading libraries *****" << endl;
  LoadLibs();

  // Create the analysis chain
  analysisChain = new StChain("dipi0Chain");
  
  //cout << "Constructing StFmsDbMaker" << endl;
  //StFmsDbMaker* fmsDbMkr = new StFmsDbMaker( "fmsDb" );
  //fmsDbMkrName = fgtDbMkr->GetName();

  gSystem->Load("StFmsDiPi0");
  StFmsDiPi0* dipi0=new StFmsDiPi0;
  TString filenameDiPi0(Form("%s/%s.dipi0.root",dir,filter));
  cout << "DiPi0 outfile name = " << filenameDiPi0.Data()<<endl;
  dipi0->setFileName(filenameDiPi0.Data());
  dipi0->setReadTree(trees);

  analysisChain->Init();

  Int_t ierr  = kStOK;  // err flag
  Int_t nevents = 0;    // cumulative number of events in
  for( ; nevents < neventsIn && !ierr; ++nevents ){
    if(nevents%10000==0) cout <<"event: "<< nevents <<endl;
    analysisChain->Clear();
    ierr = analysisChain->Make();
  } 
  analysisChain->Finish(); 

  analysisChain->Delete();
  return;
};


void LoadLibs() {
  // commong shared libraries
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StStarLogger");
  //gSystem->Load("StTpcDb");
  //gSystem->Load("StDbUtilities");
  //gSystem->Load("StDetectorDbMaker");
  //gSystem->Load("StMagF");
  //gSystem->Load("StFmsUtil");
  //gSystem->Load("StFmsDbMaker");
};
