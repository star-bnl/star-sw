// $Id: bfcread_hist_files_add.C,v 2.7 2000/06/23 21:34:21 kathy Exp $
// $Log: bfcread_hist_files_add.C,v $
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
//    MainFile - *.hist.root file from bfc output
//    MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//   TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//   mxCopy -- maximum number of histograms in first file 
//              (this is the max number that will be copied)
//
//======================================================================

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

class StHistMaker;

class StHistUtil;

//------------------------------------------------------------------------

void bfcread_hist_files_add(
  const Char_t *MainFile1=
    "/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_standard/hc_standard.40_evts.hist.root",
 const Char_t *MainFile2=
    "/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_lowdensity/hc_lowdensity.400_evts.hist.root",
  const Char_t *outHistFile="kathy",
  const Char_t *MakerHistDir="QA",
  const Char_t *TopDirTree="bfcTree")
{
  cout << "bfcread_hist_files_add.C, input hist file 1= " 
       << MainFile1 << endl;
  cout << "bfcread_hist_files_add.C, input hist file 2= " 
       << MainFile2 << endl;
  cout << "bfcread_hist_files_add.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_files_add.C, top level directory in hist file = " 
       << TopDirTree << endl;

  Int_t fnum=2;
  cout << "bfcread_hist_files_add.C, hardwired # input files = " << 
    fnum << endl << endl <<endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("St_QA_Maker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("libglobal_Tables");

//  Setup top part of chain
   chain = new StChain("MyChain");

// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// loop over files:
 Int_t ifl=1;
 Int_t hCCount=0;

 EventLoop: if (ifl<=fnum) {  

 cout << endl << " NOW GOING TO POINT TO FILE " << ifl <<  endl;

 Char_t *MainFile=0;
 if (ifl==1) MainFile=MainFile1;
 if (ifl==2) MainFile=MainFile2;
  
 StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,TopDirTree);
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");                 //deactivate all branches
    IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch

  HU->SetPntrToMaker((StMaker *)IOMk);

// --- each file contains only histograms (1 "event" == 1 Make call)
  chain->Init();
  chain->Clear();
  chain->Make();

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = HU->FindHists(MakerHistDir);

// clone(copy) histograms from first file  -----------------------------
 if (ifl==1) {

// now make a copy of all histograms into my new histograms!
  hCCount = HU->CopyHists(dirList);

  cout << "bfcread_hist_files_add.C, # histograms copied = " << 
       hCCount << endl;

 }  // if ifl==1

 else{

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


// constructor 
   //   StHistMaker   *HM  = new StHistMaker;
   //  HM->SetHArray(kathyArray);
   // need HM->SetHArraySize 

// output hist.root file:
   //StTreeMaker* treeMk = new StTreeMaker("tree",outHistFile,TopDirTreeOut);
   //  treeMk->SetIOMode("w");
   // treeMk->SetBranch("histBranch");

   //HM->Init();
   // treeMk->Init();
   // HM->Make();
   // treeMk->Make();

} // end of the macro!
 






