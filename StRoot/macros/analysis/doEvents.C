// $Id: doEvents.C,v 1.25 1999/07/16 15:13:05 fisyak Exp $
// $Log: doEvents.C,v $
// Revision 1.25  1999/07/16 15:13:05  fisyak
// clean up
//
// Revision 1.25  1999/07/16 15:13:05  fisyak
// clean up
//
// Revision 1.24  1999/07/15 13:58:39  perev
// cleanup
//
// Revision 1.23  1999/07/14 01:41:51  fisyak
// Add 0.5 Tesla magnetic field as default
//
// Revision 1.22  1999/07/02 22:45:47  perev
// Remove 2 events limit
//
// Revision 1.21  1999/06/27 22:45:33  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.2  1999/05/24 19:02:03  kathy
// put owner back into do*Events.C so it's standardized with all other macros
//
// Revision 1.1  1999/05/22 19:38:12  perev
// temporary macro for RootEvents
//
// Revision 1.13  1999/04/15 18:03:14  wenaus
// clean out duplicate/conflicting declarations
//
// Revision 1.12  1999/04/01 23:39:47  fisyak
// Cleanup old macros
//
// Revision 1.11  1999/03/10 14:28:17  fisyak
// Clean up for SL99c
//
// Revision 1.10  1999/03/02 03:34:43  fisyak
// Set LD_LIBRARY_PATH to Root.DynamicPath
//
// Revision 1.9  1999/02/28 00:08:18  wenaus
// add multi-file handling for .root files. But, using multiple files doesn't work for ROOT files yet.
//
// Revision 1.8  1999/02/25 23:10:41  wenaus
// fix multi-file bug
//
// Revision 1.7  1999/02/25 02:51:42  wenaus
// make sure default file is a working one
//
// Revision 1.6  1999/02/25 02:42:58  wenaus
// input file options
//
// Revision 1.5  1999/02/24 23:21:19  wenaus
// add ROOT file handling
//
// Revision 1.4  1999/02/20 05:39:24  wenaus
// turn off TBrowser (gives bus errors) and don't count run header as an event
//
// Revision 1.3  1999/02/16 18:15:48  fisyak
// Check in the latest updates to fix them
//
// Revision 1.2  1999/02/11 16:22:51  wenaus
// load StEvent for Linux
//
// Revision 1.1  1999/02/11 15:44:28  wenaus
// macro to read DSTs into StEvent and analyze
//
// what it does: 
// what it does: reads .dst.root or .xdf files and then runs StEventMaker
//          to fill StEvent and StAnalysisMaker to show example of analysis
//          
//=======================================================================
///////////////////////////////////////////////////////////////////////////////
//
// doEvents.C
//
// Description: 
// Chain to read events from files or database into StEvent and analyze
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL  2/99
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

TBrowser *b=0;

const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf";
  // const Char_t *file="/afs/rhic/star/data/samples/psc0016_05_35evts.root")
  // const Char_t *file="/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/./set0022_01_56evts_dst.xdf")
  // const Char_t *file="/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
  // const Char_t *file="/disk00000/star/auau200/hijing135/default/b0_20/year2x/hadronic_on/tfs_dst/pet213_02_190evts_h_dst.xdf")
  // const Char_t *path="-/disk00000/star/auau200/hijing135/",
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};
//========================================================================================================

// ----------- Ways to run -------------------------------------------
// If you specify a path, all DST files below that path will be
// If 'file ends in '.root', ROOT DSTs are searched for.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.xdf', XDF DSTs are searched for.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEvents.C(10,"-","/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/gstardata/psc0033_01_40evts.root")
//
// example ROOT file invocation:
// .x doEvents.C(9999,"/disk00001/star/auau200/hijing/b0_3/jet05/year_1b/hadronic_on/tfs/","*.root")

              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
void doEvents(const Int_t nevents,
              const Char_t **fileList)
    cout << "       doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doEvents.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
void doEvents(Int_t nevents,const Char_t **fileList,const char *qaflag)




  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
//  gSystem->Load("StEventReaderMaker");
  gSystem->Load("St_geom_Maker");
  gSystem->Load("StEventDisplayMaker");
  gSystem->Load("StAnalysisMaker");
//  gSystem->Load("St_geom_Maker");
//  gSystem->Load("StEventDisplayMaker");


  // Handling depends on whether file is a ROOT file or XDF file

  chain  = new StChain("StChain");

  StFile *setFiles= new StFile();

  for (int ifil=0; fileList[ifil]; ifil++)
  St_geom_Maker *geom = new St_geom_Maker; // this maker open its own TFile !!!
  // St_geom)Maker is to supply the GEANT/GEOM dataset, that will be provided by
  IOMk->SetDebug();
  //  St_geom_Maker *geom = new St_geom_Maker; // this maker open its own TFile !!!
// 		Maker to read events from file or database into StEvent


// 		Sample analysis maker
  StAnalysisMaker *analysisMaker = new StAnalysisMaker ("analysis");
  StEventDisplayMaker *disp  = new StEventDisplayMaker;

//  Event Display Maker
  //  StEventDisplayMaker *disp  = new StEventDisplayMaker;


  // Initialize chain
  Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  int istat;
  for (Int_t i=1; i<=nevents; i++) {
  // Event loop
  int istat=0,i=1;
EventLoop: if (i <= nevents && !istat) {
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
}
  }
     chain->Clear();
    cout << "============================ Event " << i << " finish" << endl;
  if (nevents > 1) {
    chain->Clear();
       gROOT->LoadMacro("PadControlPanel.C");
       b = new TBrowser;
    if (!b) {
      //       gROOT->LoadMacro("PadControlPanel.C");
    b = new TBrowser;
void doEvents(const Int_t nevents=999,
              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
              const Char_t *file="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf")
  }
    }
	if (!b) {
	    b = new TBrowser;
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  doEvents(nevents,fileListQQ);
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
}


// what it does: reads .dst.root or .dst.xdf file or files, fills StEvent &
//      then runs StAnalysisMaker 
//////////////////////////////////////////////////////////////////////////






