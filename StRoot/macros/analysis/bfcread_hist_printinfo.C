// $Id: bfcread_hist_printinfo.C,v 2.2 2002/01/29 20:03:08 genevb Exp $
// $Log: bfcread_hist_printinfo.C,v $
// Revision 2.2  2002/01/29 20:03:08  genevb
// Switched default dir from QA to EventQA
//
// Revision 2.1  2000/06/23 18:04:54  kathy
// new macro that uses new method in StHistUtil to dump hist info (name,entries,mean,rms) to screen & file
//
//
//=======================================================================
// owner:  Kathy Turner
//
// bfcread_hist_printinfo.C 
//
// what it does: 
//  - reads in *.hist.root file produced from a chain 
//               (such as bfc)
//  - prints info about each histogram found to screen & output file
//
// inputs: 
//    MainFile - *.hist.root file from bfc output
//    fname - output text file with histogram info 
//    MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//   TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

class StHistUtil;

//------------------------------------------------------------------------

void bfcread_hist_printinfo(
  const Char_t *MainFile=
    "/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_standard/hc_standard.40_evts.hist.root",
  const Char_t *fname="HistInfo.out",
  const Char_t *MakerHistDir="EventQA",
  const Char_t *TopDirTree="bfcTree")
{
  cout << "bfcread_hist_printinfo.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_printinfo.C, output hist info file = " 
       << fname << endl;
  cout << "bfcread_hist_printinfo.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_printinfo.C, top level directory in hist file = " 
       << TopDirTree << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("libglobal_Tables");

// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// read in file:
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,TopDirTree);
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");          //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch

// --- each file contains only histograms (1 "event" == 1 Make call)
  IOMk->Init();
  IOMk->Clear();
  IOMk->Make();
// - end of read

  HU->SetPntrToMaker((StMaker *)IOMk);

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = HU->FindHists(MakerHistDir);

// now make a copy of all histograms into my new histograms!
  Int_t hCCount=0;
  hCCount = HU->PrintInfoHists(dirList,fname);

  cout << "bfcread_hist_printinfo.C, # histograms found = " << 
       hCCount << endl;


} // end of the macro!
 






