// $Id: bfcread_hist_files_add.C,v 2.1 2000/06/22 21:16:14 kathy Exp $
// $Log: bfcread_hist_files_add.C,v $
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
//  **** the file pointers IOMk1,IOMk2 are currently hardwired in code!
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
StIOMaker *IOMk1=0;
StIOMaker *IOMk2=0;

//------------------------------------------------------------------------

void bfcread_hist_files_add(
  const Char_t *MainFile1=
    "/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_standard/hc_standard.40_evts.hist.root",
 const Char_t *MainFile2=
    "/star/rcf/test/dev/tfs_redhat61/Tue/year_1h/hc_lowdensity/hc_lowdensity.400_evts.hist.root",
  const Char_t *MakerHistDir="QA",
  const Char_t *TopDirTree="bfcTree",
  const Int_t mxCopy=500)
{
  cout << "bfcread_hist_files_add.C, input hist file 1= " 
       << MainFile1 << endl;
  cout << "bfcread_hist_files_add.C, input hist file 2= " 
       << MainFile2 << endl;
  cout << "bfcread_hist_files_add.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_files_add.C, top level directory in hist file = " 
       << TopDirTree << endl;
  cout << "bfcread_hist_files_add.C, max number histograms to copy = "
       << mxCopy << endl;

  Int_t fnum=2;
  cout << "bfcread_hist_files_add.C, hardwired # input files = " << 
         fnum << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("libglobal_Tables");

//  Setup top part of chain
  chain = new StChain("addHist");

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk1 = new StIOMaker("IO","r",MainFile1,TopDirTree);
  IOMk1->SetIOMode("r");
  IOMk1->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk1->SetBranch("histBranch",0,"r"); //activate dst Branch


  StIOMaker *IOMk2 = new StIOMaker("IO","r",MainFile2,TopDirTree);
  IOMk2->SetIOMode("r");
  IOMk2->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk2->SetBranch("histBranch",0,"r"); //activate dst Branch


// constructor for other maker (not used in chain)
   StHistUtil   *HU  = new StHistUtil;

// now must set pointer to StMaker so HistUtil can find histograms
//  with StHistUtil methods
// -- input any maker pointer but must cast as type StMaker
   HU->SetPntrToMaker((StMaker *)chain);

// --- now execute chain member functions 
// --- each file contains only histograms (1 "event" == 1 Make call)
  chain->Init();
  chain->Clear();
  chain->Make();

// create array of pointers to the new histograms I will create
 TH1 *newHist[mxCopy];
 Int_t ilg = 0;
 for (ilg=0;ilg<mxCopy;ilg++) {
      newHist[ilg]=0;
 } 
 
// loop over files:
 Int_t ifl=1;
 EventLoop: if (ifl<=fnum) {  

// now point to histograms in first file -----------------------------
 cout << endl << " NOW GOING TO POINT TO FILE " << ifl <<  endl;

 if (ifl==1) {
   HU->SetPntrToMaker((StMaker *)IOMk1);
// now make a copy of all histograms into my new histograms!
// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = HU->FindHists(MakerHistDir);
  Int_t ijk=0;
   if (dirList)
   {
   TIter nextObj(dirList);
   Int_t histCopyCount = 0;
   TObject *obj = 0;
    while ((obj = nextObj())) {    
     if (obj->InheritsFrom("TH1")) {
       histCopyCount++;         
       if (ijk<mxCopy){
         newHist[ijk] = ((TH1 *)obj->Clone());
	 //         cout << "clone hist # " << ijk << endl;
       }
       ijk++;
     }
    }
    cout << endl << " COPIED tot num hist = " << histCopyCount << endl;
   }
 }

// Now see if we can find these copies:
 // Int_t imk = 0;
 //for (imk=0;imk<histCopyCount;imk++) {
 //  if (newHist[imk]->InheritsFrom("TH1")) {       
 //        cout << " !!! NEW Type: " << newHist[imk]->ClassName() << 
 //             ", Name: "    << newHist[imk]->GetName() << 
 //             ", Title: "   << newHist[imk]->GetTitle() << 
 //	    ", Max: " << ((TH1 *)newHist[imk])->GetMaximum() << endl; 
 //  }
 //} 

 else{

   if (ifl==2) {
      HU->SetPntrToMaker((StMaker *)IOMk2); 
   }

// get the TList pointer to the histograms:
  TList  *dirList = 0;
  dirList = HU->FindHists(MakerHistDir);
  if (dirList){
   TIter nextObj(dirList);
   Int_t histReadCount = 0;
   TObject *obj = 0;
    while ((obj = nextObj())) {    
     if (obj->InheritsFrom("TH1")) {
      histReadCount++;
      
// now want to add these histograms to the copied ones:
      Int_t imk = 0;
      for (imk=0;imk<histCopyCount;imk++) {
       if (strcmp( (newHist[imk]->GetName()), (obj->GetName()) )==0) {       
	 //	cout << "  ---- hist num to add --- " << imk << endl;
         newHist[imk]->Add((TH1 *)obj);
         cout << " !!! Added histograms with Name: "
	      << newHist[imk]->GetName() <<  endl;

         if (strcmp(newHist[imk]->GetName(),"TabQaEvsumTrkTot")==0) {       
	   newHist[imk]->Draw();
           gPad->Update();
         }

       } // strcmp
      }  // loop over imk
     }   // if obj inherits from th1
    }    //while

    cout << " Added tot num hist = " << histReadCount << 
            " from file " << ifl <<endl;
  } //dirlist
 }  //else

   ifl++;                                
   goto EventLoop;   

 } // loop over files

} // end of the macro!
 






