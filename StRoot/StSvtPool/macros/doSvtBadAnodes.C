///////////////////////////////////////////////////////////////////////////////
//
// $Id: doSvtBadAnodes.C,v 1.1 2004/02/06 02:31:31 munhoz Exp $
//
///////////////////////////////////////////////////////////////////////////////

Int_t    usePath = 0;
Int_t    nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class    StChain;
StChain  *chain=0;
class StEventDisplayMaker;
StEventDisplayMaker *dsMaker = 0;
TBrowser *b=0;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void doSvtBadAnodes(Int_t nevents=1, 
		      const Char_t *path="/scratch_star5a/pp_sep2003_RFF_P03if/",
		      const Char_t *file="st_physics_3006053_raw_0536.MuDst.root",
		      const Char_t *fileOut = "ntuple.root");

// ------------------ Here is the actual method -----------------------------------------
void doSvtBadAnodes(Int_t nevents, const Char_t **fileList, const Char_t *fileOut)
{

  cout <<  endl << endl <<" doEvents -  input # events = " << nevents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doEvents -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
 
    //
    // First load some shared libraries we need
    //

    gSystem->Load("St_base");
    gSystem->Load("StUtilities");
    gSystem->Load("StChain");
    gSystem->Load("StBFChain"); 
    gSystem->Load("St_Tables");

    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("geometry");

    gSystem->Load("StIOMaker");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");
    
    gSystem->Load("StSvtClassLibrary");
    gSystem->Load("StSvtDaqMaker");
    gSystem->Load("StSvtCalibMaker");

    //
    // Handling depends on whether file is a ROOT file or XDF file
    //
    chain  = new StChain("StChain");

    StFile *setFiles =0;
    if (fileList) {	//Normal case
      setFiles= new StFile(fileList);
    } else        {	//Grand Challenge
      gSystem->Load("StChallenger");
      setFiles = StChallenger::Challenge();
      setFiles->SetDebug();
      const char *Argv[]= {
	    "-s","daq",                           // list of components needed
	    "-q","mRunNumber=1228023",   // example of user query
	    "-c","/afs/rhic/star/incoming/GCA/daq/stacs.rc"  // pointer to GC servers for daq
        };
      Int_t Argc=sizeof(Argv)/4;
      setFiles->Init(Argc,Argv);
    }

    //
    // IO Maker
    //
    StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
    IOMk->SetIOMode("r");
    IOMk->SetDebug();

    //
    // SVT DAQ Maker
    //
    StSvtDaqMaker *svtDaqMaker = new StSvtDaqMaker("svtDaq","FULL");

    //
    // SVT Bad Anode Maker
    //
    StSvtBadAnodesMaker *svtBadAnodesMaker = new StSvtBadAnodesMaker("svtBadAnode");
    svtBadAnodesMaker->setOutputFile(fileOut);
    svtBadAnodesMaker->setThresholdOccup(120);
    svtBadAnodesMaker->setFrequencyOccup(0.9);
    svtBadAnodesMaker->setBadMeanPedMin(5);
    svtBadAnodesMaker->setBadMeanPedMax(254);
    svtBadAnodesMaker->setBadMeanRMSMin(0);
    svtBadAnodesMaker->setBadMeanRMSMax(4);
    svtBadAnodesMaker->SetDebug();

    //
    // Initialize chain
    //
    IOMk->Init();
    svtDaqMaker->Init();
    svtDaqMaker->SetSvtPed();
    svtDaqMaker->SetSvtRMSPed();
    svtBadAnodesMaker->Init();

    //
    // Event loop
    //
    int istat=0,i=1;
 EventLoop: if (i <= nevents && istat!=2 && istat!=3) {

   //     cout << endl << "============================ Event " << i
   //	  << " start ============================" << endl;

     chain->Clear();
     IOMk->Make();
     svtDaqMaker->Make();
     svtDaqMaker->GetSvtPed();
     svtDaqMaker->GetSvtRMSPed();
     svtBadAnodesMaker->Make();

     if (istat==2) 
         {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}

     i++;
     goto EventLoop;
 }

    chain->Finish();

    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;

}

//--------------------------------------------------------------------------

void doSvtBadAnodes(const Int_t nevents, const Char_t *path, const Char_t *file,
              const Char_t *fileOut)
{
    if (nevents==-1) { Help(); return;}

    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) {
      fileListQQ=0;
    } else if (path[0]=='-') {
	fileListQQ[0]=file;
    } else if (!file[0]) {
	fileListQQ[0]=path;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }

    doSvtBadAnodes(nevents,fileListQQ,fileOut);
}


