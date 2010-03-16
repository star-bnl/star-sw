//*-- Author :  Valeri Fine (fine@bnl.gov)
// $Id: drawEvent.C,v 1.14 2010/03/16 16:23:09 fisyak Exp $
// $Log: drawEvent.C,v $
// Revision 1.14  2010/03/16 16:23:09  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 1.13  2006/08/15 21:42:45  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.12  2005/08/31 15:03:09  fisyak
// Add dependence StMagF vs StarMagField
//
// Revision 1.11  2001/09/21 02:21:57  jeromel
// StTpcDb needed by StEventMaker.
//
// Revision 1.10  2000/04/13 21:46:34  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.9  2000/04/12 15:06:53  kathy
// changed all macros that read DSTs to load Tables from libraries: gen,sim,global,dst instead of ALL Tables (previously loaded St_Tables); currently, if you are using DEV to read a DST in NEW,PRO, you must comment out the loading of libtpc_Tables because of a mismatch with tpt_track table
//
// Revision 1.8  2000/01/19 21:00:32  kathy
// update macros to use standard default xdf files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.7  2000/01/19 16:29:51  kathy
// update macros to use default input files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.6  1999/08/20 22:49:33  fine
// StChain::Finish() has been removed to keep the last picture on the screen
//
// Revision 1.5  1999/08/02 00:07:10  fine
// use the new edition of StVirtualFiltr class
//
// Revision 1.4  1999/07/15 13:58:39  perev
// cleanup
//
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

class StEventDisplayMaker;
StEventDisplayMaker *disp=0;

TBrowser *b=0;
const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic.bnl.gov/star/data/samples/gstar.dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

  // const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/psc0016_05_35evts.root")
  // const Char_t *file="/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/./set0022_01_56evts_dst.xdf")
  // const Char_t *file="/afs/rhic.bnl.gov/star/strange/genevb/year1a_90evts_dst.xdf")
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
// .x doEvents.C(10,"-","/afs/rhic.bnl.gov/star/strange/genevb/year1a_90evts_dst.xdf")
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

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StEvent");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StEventMaker");
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
  disp      = new StEventDisplayMaker;
  disp->SetDebug();
  // Create an user's custom filter for "Tracks" 
  St_TLA_EventFilter *trackFilter = new St_TLA_EventFilter();
  disp->SetFilter(trackFilter,StEventDisplayMaker::kTrack);
  

  // Initialize chain
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();
//  chain->MakeDoc();

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

   gROOT->LoadMacro("PadControlPanel.C");
}
void drawEvent(const Int_t nevents=2,
              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
              const Char_t *file="/afs/rhic.bnl.gov/star/data/samples/gstar.dst.xdf")
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  doEventsQQ(nevents,fileListQQ);
}








