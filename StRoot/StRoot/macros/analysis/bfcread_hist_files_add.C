// $Id: bfcread_hist_files_add.C,v 2.24 2018/03/21 02:37:32 genevb Exp $
// $Log: bfcread_hist_files_add.C,v $
// Revision 2.24  2018/03/21 02:37:32  genevb
// Improper use of endl corrected
//
// Revision 2.23  2017/02/27 21:41:36  genevb
// StEvent.so now depends on StBichsel.so
//
// Revision 2.22  2016/06/10 15:51:47  genevb
// Fix memory leak
//
// Revision 2.21  2015/03/19 17:51:28  genevb
// Minimize dependencies (tpc_Tables, number of files)
//
// Revision 2.20  2013/03/14 17:28:31  genevb
// StTpcDb.so now depends on StEvent.so
//
// Revision 2.19  2010/03/17 02:53:06  genevb
// Add hists even if not in first file
//
// Revision 2.18  2010/03/16 15:05:22  jeromel
// Fix for the move of TpcC def
//
// Revision 2.17  2007/12/10 18:03:22  genevb
// Additional library needed
//
// Revision 2.16  2007/04/03 21:15:13  genevb
// Minor fix for StIOMaker/StFile pairing
//
// Revision 2.15  2004/09/01 14:41:23  jeromel
// Thanks to Janet S., some correction to broken macro
//  fin logic change
//  gPad / example commented out
//
// Revision 2.14  2002/09/06 02:50:22  genevb
// Update compatibility with library changes
//
// Revision 2.13  2000/07/26 19:53:45  lansdell
// made changes for creating new QA histograms
//
// Revision 2.12  2000/07/07 04:18:55  lansdell
// loops over all branches of multiple hist.root files and saves the summed histograms to a new hist.root file (thanks Gene!)
//
// Revision 2.11  2000/06/29 22:42:22  lansdell
// now write to specified hist branch properly
//
// Revision 2.10  2000/06/29 05:47:17  lansdell
// checked for null pointer
//
// Revision 2.9  2000/06/29 05:04:24  lansdell
// now outputs .hist.root file with particular maker's histograms (need to update to do all histogram branches)
//
// Revision 2.8  2000/06/23 23:16:12  lansdell
// now reads .hist.root file list from text file; be sure to use nevents <= the number of .hist.root files
//
// Revision 2.7  2000/06/23 21:34:21  kathy
// remove unneeded macro; make bfcread_hist_files_add work with a chain - cleanup
//
// Revision 2.6  2000/06/23 20:57:21  kathy
// getting ready to write output hist file
//
// Revision 2.4  2000/06/23 20:18:09  kathy
// move example draw out of file loop to prepare for writing hist out to .hist.root file
//
// Revision 2.3  2000/06/23 15:52:34  kathy
// cleanup - only need to use IOMk, not IOMk1,2 etc
//
// Revision 2.2  2000/06/23 15:25:30  kathy
// now use the new methods CopyHists & AddHists in StHistUtil
//
// Revision 2.1  2000/06/22 21:16:14  kathy
// new macro that will read in 2 hist.root files and add the contents of the histograms together into a new set of histograms
//

//=======================================================================
// owner:  Kathy Turner
//
// bfcread_hist_files_add.C 
//
// what it does: 
//  - reads in different *.hist.root files produced from a chain 
//               (such as bfc)
//  - creates a copy of all histograms in the first file (newHist)
//  - adds contents of all histograms in subsequent files
//          with the same NAME into the newHist
//
// inputs: 
//   nevents - number of files to process (assuming 1 "event" per file)
//   fileName - .txt file with list of hist.root files from bfc output
//   outHistFile - output .hist.root file
//   TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//
//======================================================================


void bfcread_hist_files_add(
  int nevents=3,
  const Char_t *fileName="fileList.txt",
  const Char_t *outHistFile="kathy",
  const Char_t *TopDirTree="bfcTree")
{
  cout << "bfcread_hist_files_add.C, input file list = " 
       << fileName << endl;
  cout << "bfcread_hist_files_add.C, output file list = "
       << outHistFile << endl;
  cout << "bfcread_hist_files_add.C, top level directory in hist file = " 
       << TopDirTree << endl;

  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libStDb_Tables");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StBichsel");
  gSystem->Load("StEvent");
  gSystem->Load("StTpcDb");
  gSystem->Load("StPmdUtil");
  gSystem->Load("St_QA_Maker");
  gSystem->Load("StTreeMaker");

// read file list from text file (maximum of 8192 files)
  char **fList = new char[8192][256];
  ifstream fin(fileName);
  if (fin) {
    int j=0;
    cout << "Input file " << fileName << " found." << endl;
    for (Int_t i=0;i<nevents;i++){
      char *line = new char[256];
      fin >> line;
      if (fin.eof()) break;
      fList[j] = line;
      cout << " Added file : " << fList[j] << endl;
      j++;
    }
  }
  fin.close();

// set up write chain which knows about hist and tree makers
  StChain* chainW = new StChain("writeChain");
  char *MakerHistDir= new char[256];

// set up output file structure
  StTreeMaker* treeMk = new StTreeMaker("outTree",outHistFile,TopDirTree);
  treeMk->SetIOMode("w");
  treeMk->SetBranch("histBranch");
  treeMk->Init();

// loop over files:
  Int_t istat=0;
  Int_t ifl=0;
  Int_t hCCount=0;

  StFile* fff = new StFile(fList);
  nevents = TMath::Min(nevents,fff->GetNFiles());
  StIOMaker *IOMk = new StIOMaker("IO","r",fff,TopDirTree);
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch
  IOMk->Init();
// we DON'T want IOMaker in the write chain
  IOMk->Shunt();

// create arrays for histmakers, histutils for multiple branches/files
  StHistMaker* HM[128];
  StHistUtil*  HU[128];
  int nbranch=0;

 EventLoop: if (ifl<nevents && istat==0) {  
  
   cout << "\nNow processing file : " << ifl+1 << "\n" << endl;

// --- each file contains only histograms (1 "event" == 1 Make call)

   IOMk->Clear();
   istat = IOMk->Open();
   if (istat) goto EventLoop;
   istat = IOMk->Make();
   if (istat) goto EventLoop;
 // from list_all macro
   Event = IOMk->GetDataSet("hist");
   if (!Event) {
     cout << " No histogram branch found in file!" << endl;
   }

 // loop over all branches in the file
   TDataSetIter nextHistList(Event);
   St_ObjectSet *histContainer = 0;
   TList *dirList = 0;
   while (histContainer = (St_ObjectSet *)nextHistList.Next()) {
     dirList = (TList *) histContainer->GetObject();
     strcpy(MakerHistDir,histContainer->GetName());
     MakerHistDir[strlen(MakerHistDir)-4] = 0;
     //if (strcmp(MakerHistDir,"IO")==0 || strcmp(MakerHistDir,"IO_Root")==0) {
     //  cout << " --- Skipping " << MakerHistDir << endl;
     //  continue;
     //}
     cout << "\n --- MakerHistDir : " << MakerHistDir << endl;

// check for new branches and instantiate them
     int bnum=-1;
     for (int l=0; l<nbranch; l++) {
       if (!strcmp(MakerHistDir,HM[l]->GetName())) {
         bnum = l;
	 break;
       }
     }

     if (bnum==-1) {
       bnum = nbranch++;
       HM[bnum] = new StHistMaker(MakerHistDir);
       HM[bnum]->Init();
       HU[bnum] = new StHistUtil();
       HU[bnum]->SetPntrToMaker(IOMk);

// get the TList pointer to the histograms for this branch:
       dirList = HU[bnum]->FindHists(MakerHistDir);

// now make a copy of all histograms into the new histograms!
       hCCount = HU[bnum]->CopyHists(dirList);

       cout << "bfcread_hist_files_add.C, # histograms copied = " << 
	 hCCount << endl;

       HM[bnum]->SetHArraySize(HU[bnum]->getNewHistSize());
       HM[bnum]->SetHArray(HU[bnum]->getNewHist());
       HM[bnum]->Make();
     }  // first time

     else {
       dirList = HU[bnum]->FindHists(MakerHistDir);

// now make a copy of all histograms into my new histograms!
       hCCount = HU[bnum]->AddHists(dirList);

       cout << "bfcread_hist_files_add.C, # histograms added = " << 
	 hCCount << endl;

       HM[bnum]->SetHArraySize(HU[bnum]->getNewHistSize());
       HM[bnum]->SetHArray(HU[bnum]->getNewHist());
       HM[bnum]->Make();

     }  //else (ifl not #1)

     dirList->Delete();

// to see an example of histograms being added together:   
//   TH1** kathyArray = HU[bnum]->getNewHist();
//   for (Int_t imk=0;imk<hCCount;imk++) {
//     if (strcmp(kathyArray[imk]->GetName(),"TabQaEvsumTrkTot")==0) {       
//       kathyArray[imk]->Draw();
//       gPad->Update();
//     } // if strcmp -- to draw
//     } // for -- end of example
   } // end while loop

   ifl++;
   istat = IOMk->Close();
   goto EventLoop;
 } // loop over files

  treeMk->Make();
  treeMk->Finish();
  IOMk->Finish();
} // end of the macro!
