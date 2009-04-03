// $Id: bfcread_hist_anal.C,v 3.1 2009/04/03 02:35:30 genevb Exp $
// $Log: bfcread_hist_anal.C,v $
// Revision 3.1  2009/04/03 02:35:30  genevb
// Introduction of reference histogram analysis macro
//
//
//======================================================================
// bfcread_hist_anal.C 
// author: G. Van Buren (BNL)
//
// purpose: reads the *.hist.root file produced from a chain 
//               (such as bfc) and
//               then draws & sends to ps file the 
//               histograms from given input Maker
//               also performing an analysis if a reference
//               histogram file is supplied 
//
// inputs: MainFile - *.hist.root file from bfc output
//         MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//         TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//           NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//         psFile - output postscript file name
//         PageTitle - title at top of each page - if it's "", then it's
//                set to MainFile by default
//         PrintList - name of subset histogram list that you want printed
//                   - these are defined in StHistUtil, method SetDefaultPrintList
//                   - default = "", prints all histograms in directory MakerHistDir
//         refOutFile - name of file where histograms will be saved
//         refResultsFile - name of file where results of analysis will be saved
//         refCutsFile - name of file from where analysis cuts will be read
//         refInFile - name of file from where reference histograms will be read
//                   - without this file, no analysis will be done
// 
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void bfcread_hist_anal(
  const Char_t *MainFile=
     "/afs/rhic.bnl.gov/star/data/samples/gstar.hist.root",
  const Char_t *MakerHistDir="EventQA",
  const Char_t *TopDirTree="bfcTree",
  const Char_t *psFile="QA_hist.ps",
  const Char_t *PageTitle="",
  const Char_t *PrintList="",
  const Int_t ZoneH=2,
  const Int_t ZoneV=3,
  const Char_t *refOutFile="resultHists.root",
  const Char_t *refResultsFile="results.txt",
  const Char_t *refCutsFile=0,
  const Char_t *refInFile=0
)
{             

  cout << "bfcread_hist_anal.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_anal.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_anal.C, top level directory in hist file = " 
       << TopDirTree << endl;
  cout << "bfcread_hist_anal.C, output ps file  = " 
       << psFile << endl;
  cout << "bfcread_hist_anal.C, page title for histograms = " 
       << PageTitle << endl;
  cout << "bfcread_hist_anal.C, subset list name of which histograms to draw,print = " 
       << PrintList  << endl;
  cout << "bfcread_hist_anal.C, # histograms on page horizontally = "
       << ZoneH << endl;
  cout << "bfcread_hist_anal.C, # histograms on page vertically = "
       << ZoneV << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");


// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,TopDirTree);
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate dst Branch


// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)IOMk);

// ONLY use StIOMaker in chain 
// --- now execute chain member functions - 1 event (histograms) only
  IOMk->Init();
  IOMk->Clear();
  IOMk->Make();

// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");

    HU->SetHistsNamesDraw("*","*");
    HU->GetRunYear(MainFile);
    HU->SetOutFile(psFile);
    HU->SetZones(ZoneH,ZoneV);
    HU->SetPaperSize();
    HU->SetDefaultLogXList(MakerHistDir);
    HU->SetDefaultLogYList(MakerHistDir);
      if (PageTitle=="") PageTitle=MainFile;
    HU->SetGlobalTitle(PageTitle);

    HU->SetDefaultPrintList(MakerHistDir,PrintList);

   Int_t numLog = 0;
   numLog = HU->ExamineLogYList();
   cout <<" bfcread_hist_anal.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " bfcread_hist_anal.C, Number hist to print = " << numPrint << endl;

  HU->SetRefAnalysis(refOutFile,refResultsFile,refCutsFile,refInFile);

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDir);
   
   cout <<" bfcread_hist_anal.C, end of macro" << endl;

}












