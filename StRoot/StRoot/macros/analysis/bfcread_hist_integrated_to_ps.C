// $Id: bfcread_hist_integrated_to_ps.C,v 3.5 2018/03/21 02:53:12 genevb Exp $
// $Log: bfcread_hist_integrated_to_ps.C,v $
// Revision 3.5  2018/03/21 02:53:12  genevb
// Improper use of endl corrected
//
// Revision 3.4  2017/02/27 21:41:36  genevb
// StEvent.so now depends on StBichsel.so
//
// Revision 3.3  2013/03/14 17:28:31  genevb
// StTpcDb.so now depends on StEvent.so
//
// Revision 3.2  2010/03/16 16:23:08  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 3.1  2008/05/30 17:49:12  genevb
// Macro to sum over multiple files and prefixes to create output plots
//
//
//
//======================================================================
// author:  G. Van Buren (BNL)
// what it does:  see below
//=======================================================================
// bfcread_hist_integrated_to_ps.C 
//
// what it does: reads multiple *.hist.root files produced from a chain 
//               (such as bfc) and
//               adds histograms with the same name, but different prefix,
//               from all the files
//               then draws & sends to ps file the 
//                histograms from given input Maker
//
//    (based on a combination of bfcread_hist_prefixes_add_to_ps.C and
//                               bfcread_hist_files_add.C)
//
// inputs: nfiles - number of files to process (set large to do all files)
//         fileName - text file with list of hist.root files from bfc output
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

void bfcread_hist_integrated_to_ps(
  int nfiles=3,
  const Char_t *fileName="filelist",
  const Char_t *MakerHistDir="EventQA",
  const Char_t *TopDirTree="bfcTree",
  const Char_t *psFile="QA_hist.ps",
  const Char_t *PageTitle="",
  const Char_t *PrintList="",
  const Int_t ZoneH=2,
  const Int_t ZoneV=3
)
{             

  cout << "bfcread_hist_integrated_to_ps.C, input file list = " 
       << fileName << endl;
  cout << "bfcread_hist_integrated_to_ps.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_integrated_to_ps.C, top level directory in hist file = " 
       << TopDirTree << endl;
  cout << "bfcread_hist_integrated_to_ps.C, output ps file  = " 
       << psFile << endl;
  cout << "bfcread_hist_integrated_to_ps.C, page title for histograms = " 
       << PageTitle << endl;
  cout << "bfcread_hist_integrated_to_ps.C, subset list name of which histograms to draw,print = " 
       << PrintList  << endl;
  cout << "bfcread_hist_integrated_to_ps.C, # histograms on page horizontally = "
       << ZoneH << endl;
  cout << "bfcread_hist_integrated_to_ps.C, # histograms on page vertically = "
       << ZoneV << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("libtpc_Tables");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    gSystem->Load("StTpcDb");
    gSystem->Load("StPmdUtil");
    gSystem->Load("St_QA_Maker");

// read file list from text file (maximum of 8192 files)
  char **fList = new char[8192][256];
  ifstream fin(fileName);
  if (fin) {
    int j=0;
    cout << "Input file " << fileName << " found." << endl;
    for (Int_t i=0;i<nfiles;i++){
      char *line = new char[256];
      fin >> line;
      if (fin.eof()) break;
      fList[j] = line;
      cout << " Added file : " << fList[j] << endl;
      j++;
    }
  }
  fin.close();

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StFile* fff = new StFile(fList);
  StIOMaker *IOMk = new StIOMaker("IO","r",fff,TopDirTree);
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate dst Branch

// temporary storage of summed histograms
  TString HMname = MakerHistDir;
  HMname.Prepend("Summed");
  const char* MakerHistDirS = HMname.Data();
  StHistMaker   *HM = new StHistMaker(MakerHistDirS);

  StHistUtil   *SHU[2];
  StHistUtil   *HU;
  SHU[0]  = new StHistUtil;
  SHU[1]  = new StHistUtil;
  Bool_t firstSet[2];
  Int_t SHUidx, prefixNum, nPrefixes = SHU[0]->GetNumOfPosPrefixes();

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
  for (SHUidx=0;SHUidx<2;SHUidx++) {
    HU = SHU[SHUidx];
    HU->SetPntrToMaker(IOMk);
    HU->IgnorePrefixes();
    HU->SetDefaultPrintList(MakerHistDir,PrintList);
    firstSet[SHUidx] = kTRUE;
  }

// ONLY use StIOMaker in chain 
// --- now execute chain member functions - 1 event (histograms) only
  IOMk->Init();
  HM->Init();

  Int_t istat=0;
  Int_t ifl=0;

  while (ifl<nfiles && istat==0) {  
  
   cout << "\nNow processing file : " << ifl+1 << "\n" << endl;

// --- each file contains only histograms (1 "event" == 1 Make call)

   IOMk->Clear();
   istat = IOMk->Open();
   istat = IOMk->Make();

 // from list_all macro
   if (!(IOMk->GetDataSet("hist"))) {
     cout << " No histogram branch found in file!" << endl;
   } else {

    Int_t hCCount = 0;


     for (prefixNum=0; prefixNum < nPrefixes; prefixNum++) {
      SHUidx = (prefixNum>0 ? 1 : 0);
      HU = SHU[SHUidx];
// get the TList pointer to the histograms for this branch:
      TList* dirList = HU->FindHists(MakerHistDir,HU->GetPrefix(prefixNum));
      if (dirList && (dirList->GetSize())) {

       if (firstSet[SHUidx]) {
         firstSet[SHUidx] = kFALSE;

// now make a copy of all histograms into the new histograms!
         hCCount = HU->CopyHists(dirList);
         cout << "bfcread_hist_integrated_to_ps.C, # histograms copied with prefix " <<
           HU->GetPrefix(prefixNum) << " = " << hCCount << endl;

         HM->SetHArraySize(HU->getNewHistSize());
         HM->SetHArray(HU->getNewHist());
         HM->Make();
       } else { // first set or not
         hCCount = HU->AddHists(dirList);
         cout << "bfcread_hist_integrated_to_ps.C, # histograms added with prefix " <<
           HU->GetPrefix(prefixNum) << " = " << hCCount << endl;
       } // first set or not
       delete dirList; // Only when using PrintList or Prefixes
      } // found hists

     } // loop over prefixes

   } // histogram branch found

   ifl++;
   istat = IOMk->Close();
 } // loop over files



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
    HU->GetRunYear(fList[0]);
    HU->SetOutFile(psFile);
    HU->SetZones(ZoneH,ZoneV);
    HU->SetPaperSize();
    HU->SetDefaultLogXList(MakerHistDirS);
    HU->SetDefaultLogYList(MakerHistDirS);
    if (PageTitle=="") PageTitle=fileName;
    HU->SetGlobalTitle(PageTitle);


   Int_t numLog = 0;
   numLog = HU->ExamineLogYList();
   cout <<" bfcread_hist_integrated_to_ps.C, Number hist to plot with log scale = " << numLog << endl;

  Int_t numPrint = 0;
  numPrint = HU->ExaminePrintList();
  cout << " bfcread_hist_integrated_to_ps.C, Number hist to print = " << numPrint << endl;

//  Now draw the actual histograms to canvas and to ps file
    HU->DrawHists(MakerHistDirS);
   
   cout <<" bfcread_hist_integrated_to_ps.C, end of macro" << endl;

}












