// $Id: bfcread_eventBranch.C,v 1.10 2006/08/15 21:43:10 jeromel Exp $
// $Log: bfcread_eventBranch.C,v $
// Revision 1.10  2006/08/15 21:43:10  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.9  2004/01/23 15:06:23  fisyak
// Add gSystem->Load(StBichsel) by Lee request
//
// Revision 1.8  2001/09/20 20:13:57  genevb
// Remove StEventMaker
//
// Revision 1.7  2000/06/20 14:03:05  kathy
// now unpack and print out BfcStatus table, if it exists, from .event.root file
//
// Revision 1.6  2000/06/13 01:41:50  lansdell
// load libglobal_Tables to prevent crash
//
// Revision 1.5  2000/06/02 18:54:38  kathy
// updated to find new BfcStatus table and report
//
// Revision 1.4  2000/05/19 21:43:07  kathy
// change print statements so that autoQA can find events easily
//
// Revision 1.3  2000/05/16 21:00:00  kathy
// clean up
//
// Revision 1.2  2000/05/16 19:29:27  kathy
// update some print statements
//
// Revision 1.1  2000/05/15 20:42:08  kathy
// adding new example macro to show how to read in a .event.root file  and print info about it
//

//======================================================================
// owner:  Kathy Turner
// what it does:  reads .event.root file produced from bfc 
//                runs StAnalysisMaker
//
// Inputs to macro:
//   nevents  -  # events to process
//   MainFile - input *.event.root file  (you can use any branch here)
//   fname    - output file name with qa info
//   
//
//======================================================================

class StChain;
StChain *chain;

void bfcread_eventBranch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic.bnl.gov/star/data/samples/gstar.event.root",
  const char *fname="qa_event.out")
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_eventBranch.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    //    gSystem->Load("StMagF");
    gSystem->Load("StAnalysisMaker");


//  Setup top part of chain
    chain = new StChain("AChain");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("eventBranch",0,"r"); //activate event Branch


// sample analysis Maker to check StEvent objects
   StAnalysisMaker *analysisMaker = new StAnalysisMaker("analysis");

// --- now execute chain member functions
  chain->Init();

  TTable   *tabl=0;
  TDataSet *deventBranch=0;
  TDataSet *dde=0;

  int countev=0;
  int countevB=0;
  int countevBfc=0;
  int countevSt=0;

  Float_t countevobj=0.0;
  Float_t countevtab=0.0;

  int istat=0;
  int ijk=1;

  EventLoop: if (ijk <= nevents && istat!=2) {

    Int_t Countevobj=0;
    Int_t Countevtab=0;

    cout << endl << "============================ Event " << ijk
	  << " start ============================" << endl;

    fout << endl << "============================ Event " << ijk
	  << " start ============================" << endl;

     chain->Clear();
     istat = chain->Make(ijk);

      if (!istat) {
         countev++;
         deventBranch=chain->GetDataSet("eventBranch");
         TDataSetIter eventBIter(deventBranch);

         if (deventBranch) {
           countevB++;   
           cout << endl << " QAInfo: in eventBranch " << endl;
           fout << endl << " QAInfo: in eventBranch " << endl;

           while (dde=eventBIter.Next()) {        
             countevobj++;
             Countevobj++;

             cout << " QAInfo:   found object: " << dde->GetName() << endl;
             fout << " QAInfo:   found object: " << dde->GetName() << endl;

             if (dde->InheritsFrom("TTable")) { 
               tabl = (TTable *)dde;
               cout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
               fout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
               countevtab++;
               Countevtab++;
             }

             TString deName =  dde->GetName();
             if (deName == "BfcStatus") {
               countevBfc++;
// Now print out contents of BfcStatus for QA purposes
                 TDataSetIter bfcstatiter(dde);
                 St_dst_bfc_status *bfcstat = 
                  (St_dst_bfc_status *) bfcstatiter.Find("BfcStatus");
                 dst_bfc_status_st *bth = bfcstat->GetTable();
//  loop over all rows in table BfcStatus:
                 Int_t ij = 0;
                 for (ij=0; ij< bfcstat->GetNRows(); ij++)
                  {
		    cout << 
                      " QAInfo:       BfcStatus table -- row " << ij <<
		      ", Maker: "     <<  bth[ij]->maker_name <<
                      " has istat = "  <<  bth[ij]->status << endl;
		    fout << 
                      " QAInfo:       BfcStatus table -- row " << ij <<
		      ", Maker: "     <<  bth[ij]->maker_name <<
                      " has istat = "  <<  bth[ij]->status << endl;
                 }     
             }
             elseif (deName == "StEvent") {
               countevSt++;
             }
           }
         }

      cout << endl << " QAInfo: ev# " << countev <<
	", # objects/tables found in eventBranch = " <<
              Countevobj << "  " << 
              Countevtab << endl << endl;

      fout << endl << " QAInfo: ev# " << countev <<
	", # objects/tables found in eventBranch = " <<
              Countevobj << "  " << 
              Countevtab << endl << endl;

      }

      elseif (istat==2) 
        {cout << "Last  event processed. Status = " << istat << endl;}

      elseif (istat==3) 
        {cout << "Error event processed. Status = " << istat << endl;}

     ijk++;

     goto EventLoop;
 } 
 
    countevobj /= countev;
    countevtab /= countev;

    cout << endl;
    cout << " QAInfo: End of Job " << endl; 
    cout << " QAInfo: # times Make called = " << ijk << endl;
    cout << " QAInfo:  # events read = " << countev << endl;
    cout << " QAInfo:   #ev with eventBranch dataset = " << countevB << endl;
    cout << " QAInfo:     #ev with StEvent dataset = " <<  countevSt << endl;
    cout << " QAInfo:     #ev with BfcStatus table = " <<  countevBfc << endl;
    cout << " QAInfo: avg # tables per event  = " << countevtab << endl;
    cout << " QAInfo: avg # objects per event = " << countevobj << endl << endl;

    fout << endl;
    fout << " QAInfo: End of Job " << endl; 
    fout << " QAInfo: # times Make called = " << ijk << endl;
    fout << " QAInfo:  # events read = " << countev << endl;
    fout << " QAInfo:   #ev with eventBranch dataset = " << countevB << endl;
    fout << " QAInfo:     #ev with StEvent dataset = " <<  countevSt << endl;
    fout << " QAInfo:     #ev with BfcStatus table = " <<  countevBfc << endl;
    fout << " QAInfo: avg # tables per event  = " << countevtab << endl;
    fout << " QAInfo: avg # objects per event = " << countevobj << endl << endl;
  
 chain->Finish();   

}
 

 



