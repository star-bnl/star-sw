// $Id: strange.C,v 1.11 2000/04/12 15:06:53 kathy Exp $
// $Log: strange.C,v $
// Revision 1.11  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.10  2000/03/20 17:32:55  kathy
// setbranches in all macros so that they will work with softlinks - for StIOMaker
//
// Revision 1.9  2000/01/19 21:00:33  kathy
// update macros to use standard default xdf files in /afs/rhic/star/data/samples
//
// Revision 1.8  2000/01/19 16:58:32  genevb
// New input file
//
// Revision 1.7  1999/09/17 16:29:03  genevb
// Update macro for newer libraries.
//
// Revision 1.6  1999/06/28 00:08:45  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.5  1999/06/25 19:34:39  genevb
// Update strange.C
//
// Revision 1.4  1999/05/21 15:34:01  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.3  1999/01/21 00:53:24  fisyak
// Cleanup
//
// Revision 1.2  1999/01/19 23:05:14  genevb
// MakeDoc() removed
//
// Revision 1.1  1998/12/29 10:24:40 genevb
// strangeness dst analysis
//
//=======================================================================
// owner: Gene Van Buren
// what it does: reads .dst.root or .xdf files and then runs StEventMaker
//          to fill StEvent and StSmdstMaker for analysis.
//          Essentially a copy of doEvents.C.
//          
//=======================================================================
///////////////////////////////////////////////////////////////////////////////
//
// Functions included below which retrieve a single file or all files
// under a path

// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.

Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class StChain;
StChain *chain=0;

TBrowser *b=0;

const char *dstFile ="/afs/rhic/star/data/samples/gstar.dst.root";
const char *xdfFile ="/afs/rhic/star/data/samples/gstar.dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

//========================================================================================================

// ----------- Ways to run -------------------------------------------
// If you specify a path, all DST files below that path will be
// found, and 'nevents' events from each will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x strange.C(10,"-","/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
//
// example ROOT file invocation:
// .x strange.C(10,"-","/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/gstardata/psc0033_01_40evts.dst.root")
//
// example multi-ROOT file invocation:
// .x strange.C(9999,"/disk00001/star/auau200/hijing/b0_3/jet05/year_1b/hadronic_on/tfs/","*.dst.root")

//===================================================================================================
//
void strange(Int_t,const Char_t **,const char *qaflag = "");
void strange(Int_t nevents=999,
              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
              const Char_t *file="/afs/rhic/star/data/samples/gstar.dst.root",
              const char *qaflag = "off");


void strange(Int_t nevents,const Char_t **fileList,const char *qaflag)
{



  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libtpc_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StMagF");
  gSystem->Load("StEventMaker");
  gSystem->Load("StSmdstMaker");


  // Handling depends on whether file is a ROOT file or XDF file

  chain  = new StChain("StChain");

  StFile *setFiles= new StFile();

  for (int ifil=0; fileList[ifil]; ifil++)
  { setFiles->AddFile(fileList[ifil]);}
  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
   IOMk->SetIOMode("r");
   IOMk->SetBranch("*",0,"0");                 //deactivate all branches
   IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch
//  IOMk->SetDebug();

// Maker to read events from file or database into StEvent
  StEventMaker *readerMaker =  new StEventMaker("events","title");

//  SMDST analysis maker
  StSmdstMaker *analysisMaker = new StSmdstMaker("smdst");
  analysisMaker->DoHistograms();


  // Initialize chain
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
  TFile fsmdst("smdst.root","RECREATE");

  // Event loop
  int istat=0,i=1;
EventLoop: if (i <= nevents && !istat) {
    cout << "============================ Event " << i << " start" << endl;
    chain->Clear();
    istat = chain->Make(i);
    if (istat) {cout << "Last event processed. Status = " << istat << endl;}
    St_smdst_v0 *stable = (St_smdst_v0*) chain->GetDataSet("smdst_v0");
    if (stable) {
      stable->Shunt();
      stable->Write();
      delete stable;
    }
    i++; goto EventLoop;
}

    cout << "============================ Event " << i << " finish" << endl;
  if (nevents > 1) {
    chain->Clear();
    chain->Finish();
  } else {
    if (!b) {
    b = new TBrowser;
    }
  }
  fsmdst->Close();
}
void strange(const Int_t nevents, const Char_t *path, const Char_t *file,const char *qaflag)
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  strange(nevents,fileListQQ,qaflag);
}








