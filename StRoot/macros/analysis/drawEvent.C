//*-- Author :  Valeri Fine (fine@bnl.gov)
// $Id: drawEvent.C,v 1.3 1999/07/14 01:47:15 fine Exp $
// $Log: drawEvent.C,v $
// Revision 1.3  1999/07/14 01:47:15  fine
// New macros derived from doEvent to show StEventDisplayMaker
//
// Revision 1.2  1999/07/13 00:57:57  fine
// replace the author\'s name
//
// Revision 1.1  1999/07/13 00:54:53  fine
//  New macro based on doEvent but with 3D graphics
//
//=======================================================================
// owner: Valeri Fine (fine@bnl.gov)
// what it does: 
//=======================================================================
///////////////////////////////////////////////////////////////////////////////
//
// doEvents.C
//
// Description: 
// Chain to read events from files or database into StEvent and create its' 3D view
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Valeri Fine, BNL  7/99
//
// History:
//
///////////////////////////////////////////////////////////////////////////////

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
const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

  // const Char_t *file="/afs/rhic/star/data/samples/psc0016_05_35evts.root")
  // const Char_t *file="/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/./set0022_01_56evts_dst.xdf")
  // const Char_t *file="/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
  // const Char_t *file="/disk00000/star/auau200/hijing135/default/b0_20/year2x/hadronic_on/tfs_dst/pet213_02_190evts_h_dst.xdf")
  // const Char_t *path="-/disk00000/star/auau200/hijing135/",

// If you specify a path, all DST files below that path will be
// found, and 'nevents' events from each will be analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEvents.C(10,"-","/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
//
// example ROOT file invocation:
// .x doEvents.C(10,"-","/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/gstardata/psc0033_01_40evts.root")
//
// example multi-ROOT file invocation:
// .x doEvents.C(9999,"/disk00001/star/auau200/hijing/b0_3/jet05/year_1b/hadronic_on/tfs/","*.root")


void doEventsQQ(const Int_t nevents=999,
              const Char_t **fileList)
{

  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StMagF");
  gSystem->Load("StEvent");
//  gSystem->Load("StEventReaderMaker");
  gSystem->Load("StEventMaker");
//  gSystem->Load("StAnalysisMaker");
  gSystem->Load("St_geom_Maker");
  gSystem->Load("StEventDisplayMaker");


  // Handling depends on whether file is a ROOT file or XDF file

  chain  = new StChain("StChain");

  StFile *setFiles= new StFile();

  for (int ifil=0; fileList[ifil]; ifil++)
  { setFiles->AddFile(fileList[ifil]);}
  
  St_geom_Maker *geom = new St_geom_Maker; // this maker open its own TFile !!!
  StIOMaker *IOMk     = new StIOMaker("IO","r",setFiles,"bfcTree");
  IOMk->SetDebug();

// 		Maker to read events from file or database into StEvent
  StEventMaker *readerMaker =  new StEventMaker ("events","title");
// 		Sample analysis maker
//  StAnalysisMaker *analysisMaker = new  StAnalysisMaker("analysis");
  StEventDisplayMaker *disp      = new StEventDisplayMaker;

  // Initialize chain
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
  disp->SetTrackFilterFlag(1);
  disp->SetHitFilterFlag(0);

  // Event loop
  int istat;
  for (Int_t i=1; i<=nevents; i++) {
    cout << "============================ Event " << i << " start" << endl;
    chain->Clear();
    istat = chain->Make(i);
    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
      chain->Clear();
      break;
    }
    St_DataSet *set = chain->DataSet("dst");
    if (set)  {
      St_DataSetIter dirt(set);
      dirt.Du();
    }
    cout << "============================ Event " << i << " finish" << endl;
  }
  if (nevents > 1) {
    chain->Finish();
  } else {
//--    if (!b) b = new TBrowser;
     gROOT->LoadMacro("PadControlPanel.C");
  }
}
void drawEvent(const Int_t nevents=1,
              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
              const Char_t *file="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf")
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  doEventsQQ(nevents,fileListQQ);
}
