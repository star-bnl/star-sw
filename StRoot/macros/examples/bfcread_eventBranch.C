// $Id: bfcread_eventBranch.C,v 1.3 2000/05/16 21:00:00 kathy Exp $
// $Log: bfcread_eventBranch.C,v $
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
 "/afs/rhic/star/data/samples/gstar.event.root",
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

    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    //    gSystem->Load("StMagF");
    gSystem->Load("StEventMaker");
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

  int istat=0;
  int ijk=0;
  int countev=0;

// Event loop
EventLoop: if (ijk < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(ijk);
    
//  count # times Make is called
    ijk++;

    cout << " Call Make # " << ijk << endl; 
    cout << "     istat value returned from chain Make = " << istat << endl;

    if (!istat) {

// count # events found
    countev++;
 
    cout << " QAInfo: event # " << countev  << endl << endl;
    fout << " QAInfo: event # " << countev  << endl << endl;

    }  // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }

    goto EventLoop;

}  // EventLoop
     
 
  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << ijk << endl;
  cout << "QAInfo:  # events read = " << countev << endl;

  fout << endl;
  fout << "QAInfo: # times Make called = " << ijk << endl;
  fout << "QAInfo:  # events read = " << countev << endl;

 chain->Finish();   

}
 

 


