// $Id: bfcread_histBranch.C,v 1.1 2000/03/13 17:50:17 kathy Exp $
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
//=======================================================================

class StChain;
StChain *chain;

void bfcread_histBranch(
 Int_t nevents=1, 
 const char *MainFile=
"/star/rcf/test/new/tfs_redhat61/year_1h/hc_standard/hc_standard.40_evts.dst.root")
{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StIOMaker");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("runcoBranch",0,"r"); //activate runco Branch

// --- now execute chain member functions
  chain->Init();

  St_DataSet *ds=0;
  St_Table   *tabl=0;
  St_DataSet *obj=0;

// Event loop
  int istat=0,i=1;

EventLoop: if (i <= nevents && !istat) {
    cout << "============================ Event " << i << " start" << endl;
    chain->Clear();
    istat = chain->Make(i);
    cout << "     istat value returned from chain Make = " << istat << endl;


// Now look at the data in the event:
    int countObj=0;
    int countHist=0;

    if (!istat) {
      ds=chain->GetDataSet("hist");
      St_DataSetIter tabiter(ds);
      if (ds) {
//        ds->ls(2); 

        St_DataSetIter nextHistList(ds);
        St_ObjectSet *histContainer = 0;
        TList *dirList = 0;

// loop over directories:
        while (histContainer = (St_ObjectSet *)nextHistList()) {
          dirList = (TList *) histContainer->GetObject();
          cout << "  Found Object (directory) = " 
               << histContainer->GetName() << endl;
          countObj++;

// Notes for future reference (if we want to generalize this...)
//    dirList is returned  0 for non-histogram file
//       in that case, use GetList instead of GetObject 

         TIter nextHist(dirList);
         TObject  *o = 0;

// loop over histograms in the directory:
          while (o= nextHist()) {
           countHist++;
           cout << "    Hist name: " << o->GetName() << " ==> Title: " 
	        << o->GetTitle() << endl; 
          }
        }
      }
    }

    cout << " End of Event " << i << endl;
    cout << "  # objects found = " << countObj << endl;
    cout << "  # hist found = " << countHist << endl;

    if (istat) {
      cout << "Last event processed. Status = " << istat << endl;
    }

    i++;
    goto EventLoop;
   }

  cout << " bfcread HIST Branch: passed event loop " << endl;

 chain->Finish();   
}
 


