Int_t usePath = 0;
Int_t nFile = 0;
int ON=1;
TString  thePath;
TString  theFileName;
TString  originalPath;

Int_t        nBins = 200;
Double_t lBin    = 0.0;
Double_t uBin   = 2.0;

TH1D* signal1            = new TH1D("signal1","Signal1 Angle Distribution",nBins,lBin,uBin);
TH1D* background1 = new TH1D("background1","Background1 Angle Distribution",nBins,lBin,uBin);
TH1D* ratio1              = new TH1D("ratio1","Distribution1",nBins,lBin,uBin);

class StChain;
StChain *chain=0;
TBrowser *b=0;
 
void doCorr(Int_t,const Char_t **,const char *qaflag = "");
void doCorr(Int_t nevents=999,
              const Char_t *path="/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs_4/",
              const Char_t *file="*.root",
              const char *qaflag = "off");

void doCorr(Int_t nevents,const Char_t **fileList,const char *qaflag)
{
  
  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StMagF");
  gSystem->Load("StEventMaker");
  gSystem->Load("StAngleCorrMaker");
  
  // Handling depends on whether file is a ROOT file or XDF file
  chain  = new StChain("StChain");
  StFile *setFiles= new StFile();

  for (int ifil=0; fileList[ifil]; ifil++) {setFiles->AddFile(fileList[ifil]);}
  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");

  StEventMaker *readerMaker =  new StEventMaker("events","title");
  StAngleCorrMaker *corr    = new StAngleCorrMaker("corr");

     TString analysis1 = "analysis1";
     TString func1        = "InvariantMass";
     corr->AddAnalysis(analysis1);
    
     corr->SetCorrelationFunction(analysis1,func1);
     corr->SetPtCutsTrack1(analysis1,0.0,10.0); 
     corr->SetPtCutsTrack2(analysis1,0.0,10.0); 
     corr->SetRapidityCutsTrack1(analysis1,-1.,1.);
     corr->SetRapidityCutsTrack2(analysis1,-1.,1.);
     corr->SetChargeTrack1(analysis1,+1);
     corr->SetChargeTrack2(analysis1,-1);
     corr->SetSignalHist(analysis1,signal1);
     corr->SetBackgroundHist(analysis1,background1);
    
     // Initialize chain
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();

  // Event loop
  int istat=0,i=1;
  EventLoop: if (i <= nevents && !istat) 
    {
      cout << "============================ Event " << i << " start" << endl;
      chain->Clear();
      istat = chain->Make(i);
      cout << "finished make" << endl;

      if (istat) 
	{
	  cout << "Last event processed. Status = " << istat << endl;
	}

      i++; goto EventLoop;
    }
  
  cout << "============================ Event " << i << " finish" << endl;

  if (nevents > 1) 
    {
      
      chain->Clear();
      chain->Finish();
    } 
  else 
    {
      if (!b) 
	{
	  b = new TBrowser;
	}
    }
}


void doCorr(const Int_t nevents, const Char_t *path, const Char_t *file,const char *qaflag)
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
   
  doCorr(nevents,fileListQQ,qaflag);  
  
  Stat_t snf1 = signal1->GetEntries();
  Stat_t bnf1 = background1->GetEntries();
  ratio1->Divide(signal1,background1, bnf1/snf1, 1.);
  
  // draw canvus, histo's
     
}








