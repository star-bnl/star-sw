// $Id: bfcread_hist_files_add.C,v 2.5 2000/06/23 20:46:36 lansdell Exp $
// $Log: bfcread_hist_files_add.C,v $
// Revision 2.5  2000/06/23 20:46:36  lansdell
// now reads text file of hist.root filenames, # of events in argument list must equal # of files to process
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
//  - reads in 2 different *.hist.root file produced from a chain 
//               (such as bfc)
//  - creates a copy of all histograms in the first file (newHist)
//  - adds contents of all histograms in subsequent files
//          with the same NAME into the newHist
//
//  **** currently only works with 2 input files!
//  **** needs to be changed to work with input file list!
//  **** needs to have new histograms written out to new file
//
// inputs: 
//   nevents - number of files to process (assuming 1 "event" per file)
//   filName - .txt file with list of hist.root files from bfc output
//   MakerHistDir - directory name of Maker that you want histograms 
//                  from (this will be first input when you did constructor)
//                - see standard Maker names note below!
//   TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//   mxCopy - maximum number of histograms in first file 
//            (this is the max number that will be copied)
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

class StHistUtil;

//------------------------------------------------------------------------

void bfcread_hist_files_add(
  int nevents=3,
  const Char_t *fileName="fileList.txt",
  const Char_t *MakerHistDir="QA",
  const Char_t *TopDirTree="bfcTree")
{
  cout << "bfcread_hist_filelist_add.C, input file list = " 
       << fileName << endl;
  cout << "bfcread_hist_files_add.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_files_add.C, top level directory in hist file = " 
       << TopDirTree << endl;

//
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StIOMaker");
  //    gSystem->Load("St_QA_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("libglobal_Tables");

// read file list from text file
  char **fList = new char[512][200];
  ifstream fin(fileName);
  if (fin) {
    int j=0;
    cout << "Input file " << fileName << " found." << endl;
    while (!fin.eof()) {
      char *line = new char[200];
      fin >> line;
      fList[j] = line;
      cout << " Added file : " << fList[j] << endl;
      j++;
    }
  }
  fin.close();

// constructor for other maker (not used in chain)
  StHistUtil   *HU  = new StHistUtil;

// loop over files:
  Int_t istat=0;
  Int_t ifl=0;
  Int_t hCCount=0;
  Int_t hACount=0;

 EventLoop: if (ifl<nevents && istat==0) {  
   cout << "Processing Event : " << ifl+1 << endl;
   cout << endl << " NOW GOING TO POINT TO FILE " << ifl+1 <<  endl;

   StIOMaker *IOMk = new StIOMaker("IO","r",fList[ifl],TopDirTree);
   IOMk->SetIOMode("r");
   IOMk->SetBranch("*",0,"0");                 //deactivate all branches
   IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch

// clone(copy) histograms from first file  -----------------------------
   if (ifl==0) {

// --- each file contains only histograms (1 "event" == 1 Make call)
     IOMk->Init();
     IOMk->Clear();
     istat = IOMk->Make();
// - end of read

     HU->SetPntrToMaker((StMaker *)IOMk);

// get the TList pointer to the histograms:
     TList  *dirList = 0;
     dirList = HU->FindHists(MakerHistDir);

// now make a copy of all histograms into my new histograms!
     hCCount = HU->CopyHists(dirList);

     cout << "bfcread_hist_files_add.C, # histograms copied = " << 
       hCCount << endl;

   }  // if ifl==1

   else {
// --- each file contains only histograms (1 "event" == 1 Make call)
    IOMk->Init();
    IOMk->Clear();
    istat = IOMk->Make();

    HU->SetPntrToMaker((StMaker *)IOMk);

// get the TList pointer to the histograms:
    TList  *dirList = 0;
    dirList = HU->FindHists(MakerHistDir);

// now make a copy of all histograms into my new histograms!
    Int_t hACount=0;
    hACount = HU->AddHists(dirList,hCCount);

    cout << "bfcread_hist_files_add.C, # histograms added = " << 
      hACount << endl << endl;

   }  //else (ifl not #1)

   ifl++;                                
   goto EventLoop;   

 } // loop over files

// to see an example of histograms being added together:   
  cout << "bfcread_hist_files_add.C, an example! = " <<  endl;
  Int_t imk = 0;
  for (imk=0;imk<hCCount;imk++) {
    TH1** kathyArray = HU->getNewHist();
    if (strcmp(kathyArray[imk]->GetName(),"TabQaEvsumTrkTot")==0) {       
      kathyArray[imk]->Draw();
      gPad->Update();
    } // if strcmp -- to draw
  } // for -- end of example

} // end of the macro!
 






