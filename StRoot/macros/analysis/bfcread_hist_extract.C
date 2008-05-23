// $Id: bfcread_hist_extract.C,v 3.3 2008/05/23 17:54:55 genevb Exp $ 
// $Log: bfcread_hist_extract.C,v $
// Revision 3.3  2008/05/23 17:54:55  genevb
// Allow subset histogram list when copying/extracting
//
// Revision 3.2  2006/08/15 21:42:38  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 3.1  2004/07/01 18:33:50  genevb
// Macro to extract hists from BFC output hist.root files
//
//
//======================================================================
// owner:  Gene Van Buren
// what it does:  see below
//=======================================================================
// bfcread_hist_extract.C 
//
// what it does: reads the *.hist.root file produced from a chain 
//               (such as bfc) and
//               then extracts list of histograms from given input Maker
//               to an output file
//
// inputs: MainFile - *.hist.root file from bfc output
//         MakerHistDir - directory name of Maker that you want histograms 
//                   from (this will be first input when you did constructor)
//             -- see standard Maker names note below!
//         TopDirTree - top level directory tree in your input hist file
//                (this is 3rd argument of constructor for StTreeMaker that
//                 you probably used to write the *.hist.root file)
//            NOTE: if you ran bfc, then the TopDirTree = bfcTree !!
//         OutFile - file where histograms will be written. Default is
//                same as MainFile with last part of MakerHistDir in the name.
//         PrintList - name of subset histogram list that you want extracted
//                   - these are defined in StHistUtil, method SetDefaultPrintList
//                   - default = 0, extracts all histograms in directory MakerHistDir
//
// standard Maker names in bfc 
//   (but if you run your own Maker here, then use whatever name you give it)
//  are listed at 
//  http://www.star.bnl.gov/STAR/html/comp_l/train/tut/bfc_maker_names.html
//
//
// Documentation on StHistUtil class is at:
//   http://duvall.star.bnl.gov/STARAFS/comp/pkg/dev/StRoot/StAnalysisUtilities/doc/
//
// Note: most of this code is copied from bfcread_hist_list.C
//======================================================================


class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void bfcread_hist_extract(
  const Char_t *MainFile=
    "/afs/rhic.bnl.gov/star/data/samples/gstar.hist.root",
  const Char_t *MakerHistDir="EventQA",
  const Char_t *TopDirTree="bfcTree",
  Char_t *OutFile=0,
  const Char_t *PrintList="")
{

  cout << "bfcread_hist_extract.C, input hist file = " 
       << MainFile << endl;
  cout << "bfcread_hist_extract.C, directory name for hist = " 
       << MakerHistDir << endl;
  cout << "bfcread_hist_extract.C, top level directory in hist file = " 
       << TopDirTree << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("libglobal_Tables");

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

// method to print out list of histograms
// - can do this anytime after they're booked
// - default is to print out QA hist branch
   Int_t NoHist=0;
   //NoHist = HU->ListHists(MakerHistDir);
   TList* dList = HU->FindHists(MakerHistDir);
   if (PrintList) HU->SetDefaultPrintList(MakerHistDir,PrintList);
   NoHist = HU->CopyHists(dList);
   TH1** nh = HU->getNewHist();

   TString name = MainFile;
   if (!OutFile) {
     name.Remove(0,name.Last('/')+1);
     TString name2 = MakerHistDir;
     name2.Remove(0,name2.Last('/')+1);
     name.Insert(name.First('.'),"_");
     name.Insert(name.First('.'),name2);
     OutFile = name.Data();
   }
   cout <<  "Output hist file: " << OutFile << endl;
   TFile* ofile = new TFile(OutFile,"RECREATE");
   for (int i=0; i<NoHist; i++) {
     printf("Extracting: %d. %s : %s\n",
       i+1,nh[i]->GetName(),nh[i]->GetTitle());
     nh[i]->Write();
   }
   ofile->Close();
      
}
 






