// $Id: strange.C,v 1.6 1999/06/28 00:08:45 fisyak Exp $
// $Log: strange.C,v $
// Revision 1.6  1999/06/28 00:08:45  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.5  1999/06/25 19:34:39  genevb
// Update strange.C
//
// Revision 1.18  1999/05/24 19:02:03  kathy
// put owner back into do*Events.C so it's standardized with all other macros
//
// Revision 1.17  1999/05/23 21:54:07  perev
// restore old default in doEvent
//
// Revision 1.16  1999/05/23 03:20:20  perev
// SetMaxEvent(2) removed
//
// Revision 1.15  1999/05/22 19:39:09  perev
// Big changes for all fmts(xdf,mdc2,root)
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
//
//=======================================================================
// owner: Torre Wenaus
// what it does: 
//=======================================================================
///////////////////////////////////////////////////////////////////////////////
//
// strange.C
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
// .x strange.C(10,"-","/afs/rhic/star/strange/genevb/year1a_90evts_dst.xdf")
//
// example ROOT file invocation:
// .x strange.C(10,"-","/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/gstardata/psc0033_01_40evts.root")
//
// example multi-ROOT file invocation:
// .x strange.C(9999,"/disk00001/star/auau200/hijing/b0_3/jet05/year_1b/hadronic_on/tfs/","*.root")

void strangeQQ(const Int_t nevents, const Char_t **fileList);


void strange(const Int_t nevents=999,
              const Char_t *path="-/disk00001/star/auau200/hijing135/jetq_on/b0_3/year_1b/hadronic_on/tfs/",
              const Char_t *file="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf")
{
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  strangeQQ(nevents,fileListQQ);
}

void Load(){
  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StMagF");
  gSystem->Load("StSmdstMaker");
}
void strangeQQ(const Int_t nevents,
              const Char_t **fileList)
{

  if (gClassTable->GetID("StChain") < 0) Load();

  // Handling depends on whether file is a ROOT file or XDF file

  chain  = new StChain("StChain");

  StFile *setFiles= new StFile();

  for (int ifil=0; fileList[ifil]; ifil++)
  { setFiles->AddFile(fileList[ifil]);}
  
  StIOMaker *IOMk = new StIOMaker("IO","r",setFiles,"bfcTree");
  IOMk->SetDebug();

// 		Maker to read events from file or database into StEvent
  StEventMaker readerMaker("events","title");
// 		Sample analysis maker
 		StSmdstMaker *analysis=new StSmdstMaker("analysis");
		analysis->DoHistograms();

  // Initialize chain
Int_t iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  chain->PrintInfo();

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
    if (!b) b = new TBrowser;
  }
}
