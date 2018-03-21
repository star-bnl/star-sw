// $Id: bfcread_hist_prefixes_add.C,v 3.6 2018/03/21 02:53:12 genevb Exp $
// $Log: bfcread_hist_prefixes_add.C,v $
// Revision 3.6  2018/03/21 02:53:12  genevb
// Improper use of endl corrected
//
// Revision 3.5  2017/02/27 21:41:36  genevb
// StEvent.so now depends on StBichsel.so
//
// Revision 3.4  2013/03/14 17:28:31  genevb
// StTpcDb.so now depends on StEvent.so
//
// Revision 3.3  2011/06/03 00:09:21  genevb
// Calling CopyHist twice was dangerous, AddHist handles copying if not found after first CopyHist
//
// Revision 3.2  2010/03/16 16:23:08  fisyak
// StTpcDb requires StDetectorDbMaker
//
// Revision 3.1  2008/05/28 05:16:07  genevb
// Allow summing over (ignoring) histogram prefixes
//
//

//=======================================================================
// author:  G. Van Buren (BNL)
//
// bfcread_hist_prefixes_add.C 
//
//   (based loosely on bfcread_hist_files_add.C)
//
// what it does: 
//  - reads in a hist.root file produced from a chain 
//               (such as bfc)
//  - adds contents of histograms with same name, but different prefix,
//          according to the list of prefixes kept in StHistUtil
//  - outputs a new "summed" version of the same hist.root file
//
// inputs: 
//   fileName - hist.root file from bfc output
//   outHistFile - output .hist.root file
//   TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//
//======================================================================


void bfcread_hist_prefixes_add(
  const Char_t *fileName="",
  const Char_t *outHistFile="summed",
  const Char_t *TopDirTree="bfcTree")
{
  cout << "bfcread_hist_prefixes_add.C, input file = " 
       << fileName << endl;
  cout << "bfcread_hist_prefixes_add.C, output file = "
       << outHistFile << endl;
  cout << "bfcread_hist_prefixes_add.C, top level directory in hist file = " 
       << TopDirTree << endl;

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
  gSystem->Load("StTreeMaker");

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
  Int_t hCCount=0;

  StIOMaker *IOMk = new StIOMaker("IO","r",fileName,TopDirTree);
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

  

// --- each file contains only histograms (1 "event" == 1 Make call)

   IOMk->Clear();
   istat = IOMk->Open();
   istat = IOMk->Make();
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

     HU[nbranch] = new StHistUtil();
     HU[nbranch]->SetPntrToMaker(IOMk);
     HU[nbranch]->IgnorePrefixes();
     HM[nbranch]=0;

     int prefixNum;
     int nPrefixes = HU[nbranch]->GetNumOfPosPrefixes();
     Bool_t firstSet = kTRUE;
     for (prefixNum=0; prefixNum < nPrefixes; prefixNum++) {
 
// get the TList pointer to the histograms for this branch:
      dirList = HU[nbranch]->FindHists(MakerHistDir,HU[nbranch]->GetPrefix(prefixNum));
      if (dirList->GetSize()) {

       if (firstSet) {
         firstSet = kFALSE;

// now make a copy of all histograms into the new histograms!
         hCCount = HU[nbranch]->CopyHists(dirList);
         cout << "bfcread_hist_prefixes_add.C, # histograms copied with prefix " <<
           HU[nbranch]->GetPrefix(prefixNum) << " = " << hCCount << endl;

         if (HM[nbranch]==0) {
           HM[nbranch] = new StHistMaker(MakerHistDir);
           HM[nbranch]->Init();
         }
         HM[nbranch]->SetHArraySize(HU[nbranch]->getNewHistSize());
         HM[nbranch]->SetHArray(HU[nbranch]->getNewHist());
         HM[nbranch]->Make();
       } else { // first set or not
         hCCount = HU[nbranch]->AddHists(dirList);
         cout << "bfcread_hist_prefixes_add.C, # histograms added with prefix " <<
           HU[nbranch]->GetPrefix(prefixNum) << " = " << hCCount << endl;
       } // first set or not
      } // found hists

     } // loop over prefixes

     nbranch++;

  } // end while loop over branches
  istat = IOMk->Close();

  treeMk->Make();
  treeMk->Finish();
  IOMk->Finish();
} // end of the macro!
