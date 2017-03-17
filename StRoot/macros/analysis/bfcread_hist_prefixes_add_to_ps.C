// $Id: bfcread_hist_prefixes_add_to_ps.C,v 3.6 2017/02/27 21:41:36 genevb Exp $
// $Log: bfcread_hist_prefixes_add_to_ps.C,v $
// Revision 3.6  2017/02/27 21:41:36  genevb
// StEvent.so now depends on StBichsel.so
//
// Revision 3.5  2014/07/22 20:37:52  genevb
// Remove unnecessary library loads
//
// Revision 3.4  2013/03/14 17:28:31  genevb
// StTpcDb.so now depends on StEvent.so
//
// Revision 3.3  2011/06/03 00:09:21  genevb
// Calling CopyHist twice was dangerous, AddHist handles copying if not found after first CopyHist
//
// Revision 3.2  2010/03/16 16:23:09  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 3.1  2008/05/28 05:16:08  genevb
// Allow summing over (ignoring) histogram prefixes
//
//
//======================================================================
// author:  G. Van Buren (BNL)
// what it does:  see below
//=======================================================================
// bfcread_hist_prefixes_add_to_ps.C 
//
// what it does: reads the *.hist.root file produced from a chain 
//               (such as bfc) and
//               adds histograms with the same name, but different prefix,
//               then draws & sends to ps file the 
//                histograms from given input Maker
//
//    (based on a combination of bfcread_hist_to_ps.C and
//                               bfcread_hist_prefixes_add.C)
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
// 
//
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void bfcread_hist_prefixes_add_to_ps(
  const Char_t *MainFile=
     "/afs/rhic.bnl.gov/star/data/samples/gstar.hist.root",
  const Char_t *MakerHistDir="EventQA",
  const Char_t *TopDirTree="bfcTree",
  const Char_t *psFile="QA_hist.ps",
  const Char_t *PageTitle="",
  const Char_t *PrintList="",
  const Int_t ZoneH=2,
  const Int_t ZoneV=3
)
{             

  cout << "bfcread_hist_prefixes_add_to_ps.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, top level directory in hist file = " 
       << TopDirTree << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, output ps file  = " 
       << psFile << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, page title for histograms = " 
       << PageTitle << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, subset list name of which histograms to draw,print = " 
       << PrintList  << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, # histograms on page horizontally = "
       << ZoneH << endl;
  cout << "bfcread_hist_prefixes_add_to_ps.C, # histograms on page vertically = "
       << ZoneV << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    gSystem->Load("StTpcDb");
    gSystem->Load("StPmdUtil");
    gSystem->Load("St_QA_Maker");

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,TopDirTree);
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate dst Branch

  TString HMname = MakerHistDir;
  HMname.Prepend("Summed");
  const char* MakerHistDirS = HMname.Data();

// temporary storage of summed histograms
  StHistMaker   *HM = new StHistMaker(MakerHistDirS);


// constructor for other maker (not used in chain)
   StHistUtil   *HO  = new StHistUtil;
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
   HO->SetPntrToMaker(IOMk);
   HU->SetPntrToMaker(IOMk);
   HO->IgnorePrefixes();
   HU->IgnorePrefixes();

// ONLY use StIOMaker in chain 
// --- now execute chain member functions - 1 event (histograms) only
  IOMk->Init();
  IOMk->Clear();
  IOMk->Make();
  HM->Init();


    HO->SetDefaultPrintList(MakerHistDir,PrintList);
    TList* dirList = HO->FindHists(MakerHistDir,HO->GetPrefix(0));
    Int_t hCCount = 0;

     int prefixNum;
     int nPrefixes = HO->GetNumOfPosPrefixes();
     Bool_t firstSet = kTRUE;
     for (prefixNum=0; prefixNum < nPrefixes; prefixNum++) {
 
// get the TList pointer to the histograms for this branch:
      dirList = HO->FindHists(MakerHistDir,HO->GetPrefix(prefixNum));
      if (dirList->GetSize()) {

       if (firstSet) {
         firstSet = kFALSE;

// now make a copy of all histograms into the new histograms!
         hCCount = HO->CopyHists(dirList);
         cout << "bfcread_hist_prefixes_add_to_ps.C, # histograms copied with prefix " <<
           HO->GetPrefix(prefixNum) << " = " << hCCount << endl;

         HM->SetHArraySize(HO->getNewHistSize());
         HM->SetHArray(HO->getNewHist());
         HM->Make();
       } else { // first set or not
         hCCount = HO->AddHists(dirList);
         cout << "bfcread_hist_prefixes_add_to_ps.C, # histograms added with prefix " <<
           HO->GetPrefix(prefixNum) << " = " << hCCount << endl;
       } // first set or not
      } // found hists

     } // loop over prefixes



// method to print out list of histograms
// - can do this anytime after they're booked
// - default is to print out QA hist branch
   Int_t NoHist=0;
   NoHist = HU->ListHists(MakerHistDirS);
   cout << " in bfcread_hist_list: Num of Hist = " << NoHist << endl;
      
// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");
//    gStyle->SetOptStat(111111);

    HU->SetHistsNamesDraw("*","*");
    HU->GetRunYear(MainFile);
    HU->SetOutFile(psFile);
    HU->SetZones(ZoneH,ZoneV);
    HU->SetPaperSize();
    HU->SetDefaultLogXList(MakerHistDirS);
    HU->SetDefaultLogYList(MakerHistDirS);
      if (PageTitle=="") PageTitle=MainFile;
    HU->SetGlobalTitle(PageTitle);

    HU->SetDefaultPrintList(MakerHistDirS,PrintList);

   Int_t numLog = 0;
   numLog = HU->ExamineLogYList();
   cout <<" bfcread_hist_prefixes_add_to_ps.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " bfcread_hist_prefixes_add_to_ps.C, Number hist to print = " << numPrint << endl;

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDirS);
   
   cout <<" bfcread_hist_prefixes_add_to_ps.C, end of macro" << endl;

}












