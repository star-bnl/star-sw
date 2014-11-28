// $Id: bfcread_histBranch.C,v 1.7 2006/08/15 21:43:12 jeromel Exp $
// $Log $

//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the hist.root branch
//                - finds data set "hist"
//                - loops over and prints out list of objects 
//                  - then for each object (which is a directory)
//                    prints out all objects  (which are histograms)
//                    inside it
//
//  - the hist branch only has 1 "event" per dst run!
//
// Inputs to macro:
//   nevents  -  # events to process  (should always leave at 1 !!)
//   MainFile - input *.dst.root file  (you can use any branch here)
//   fname    - output file name with qa info
//  
//=======================================================================

class StChain;
StChain *chain;

void bfcread_histBranch(
 Int_t nevents=1, 
 const char *MainFile=
 "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
  const char *fname="qa_hist.out")
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_histBranch.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("libglobal_Tables");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch

// --- now execute chain member functions
  chain->Init();

  TDataSet *ds=0;
  TDataSet *obj=0;

  int istat=0;
  int i=0;
  int countev=0;
  int countevhds=0;

// Event loop
EventLoop: if (i < nevents && !istat) {

    chain->Clear();
    istat = chain->Make(i);

//  count # times Make is called
    i++;

// Now look at the data in the event:
    int countObj=0;
    int countHist=0;

    if (!istat) {

    countev++;

    cout << " start event # " << countev << endl;

      ds=chain->GetDataSet("hist");
      TDataSetIter tabiter(ds);
      if (ds) {

        countevhds++;

        TDataSetIter nextHistList(ds);
        St_ObjectSet *histContainer = 0;
        TList *dirList = 0;

// loop over directories:
        while (histContainer = (St_ObjectSet *)nextHistList()) {
          dirList = (TList *) histContainer->GetObject();

	  cout << " QAInfo: found directory: " << 
                    histContainer->GetName() << endl;
	  fout << " QAInfo: found directory: " << 
                    histContainer->GetName() << endl;

          countObj++;

// Notes for future reference (if we want to generalize this...)
//    dirList is returned  0 for non-histogram file
//       in that case, use GetList instead of GetObject 

         TIter nextHist(dirList);
         TObject  *o = 0;

// loop over histograms in the directory:
          while (o= nextHist()) {
           countHist++;
           cout << " QAInfo:   Hist name: " << o->GetName() << 
                     " ==> Title: " << o->GetTitle() << endl; 
           fout << " QAInfo:   Hist name: " << o->GetName() << 
                     " ==> Title: " << o->GetTitle() << endl; 

          } // nextHist
        } // histContainer
      } // ds

    cout << " QAInfo: event # " << countev
            << ", # directories found = " << countObj 
            << ", # hist found = " << countHist
            << endl << endl;


    fout << " QAInfo: event # " << countev
            << ", # directories found = " << countObj 
            << ", # hist found = " << countHist
            << endl << endl;


    } // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }


    goto EventLoop;
} //EventLoop

  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << i << endl;
  cout << "QAInfo:  # events read = " << countev << endl;
  cout << "QAInfo:   # events with hist dataset = " << countevhds << endl;

  fout << endl;
  fout << "QAInfo: End of Job " << endl;
  fout << "QAInfo: # times Make called = " << i << endl;
  fout << "QAInfo:  # events read = " << countev << endl;
  fout << "QAInfo:   # events with hist dataset = " << countevhds << endl;

 chain->Finish();   
}
 


