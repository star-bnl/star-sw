// $Id: bfcread_geantBranch.C,v 1.13 2006/08/15 21:43:11 jeromel Exp $
// $Log: bfcread_geantBranch.C,v $
// Revision 1.13  2006/08/15 21:43:11  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.12  2000/06/20 19:07:23  lansdell
// had to load libglobal_Tables to prevent crashes
//
// Revision 1.11  2000/06/01 18:57:07  kathy
// updating to separate out BfcStatus stats
//
// Revision 1.10  2000/05/31 21:25:28  kathy
// updated so it now finds all tables/objects (e.g. BfcStatus) under geantBranch
//
//
//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the geant.root branch
//                - finds data set "geant"
//                - loops over and prints out list of objects 
//                   and tables
//
// Inputs to macro:
//   nevents  -  # events to process
//   MainFile - input *.dst.root file  (you can use any branch here)
//   fname    - output file name with qa info
//   
//
//=======================================================================

class StChain;
StChain *chain;

void bfcread_geantBranch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic.bnl.gov/star/data/samples/gstar.geant.root",
 const char *fname="qa_geant.out") 
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_geantBranch.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StIOMaker");


//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch

// --- now execute chain member functions
  chain->Init();
  cout << " Finished Init()..." << endl;
  TDataSet *ddb=0;
  TTable   *tabl=0;
  TDataSet *obj=0;
  TDataSet *dgeantBranch=0;

  int istat=0;
  int iev=0;

  Float_t countev=0.0;

  Float_t countevgB=0.0;
  Float_t countevg=0.0;
  Float_t countevobjg=0.0;
  Float_t countevtabg=0.0;
  Float_t countevobjb=0.0;
  Float_t countevtabb=0.0;


// Event loop
EventLoop: if (iev < nevents && !istat) {

  Int_t Countevobjg=0;
  Int_t Countevtabg=0;
  Int_t Countevobjb=0;
  Int_t Countevtabb=0;


    chain->Clear();
    cout << " Calling Make()..." << endl;
    istat = chain->Make(iev);
    cout << " Make() finished..." << endl;
    
//  count # times Make is called
    iev++;

      cout << " Call Make # " << iev << endl; 
      cout << "     istat value returned from chain Make = " << istat << endl;

// Now look at the data in the event:


    if (!istat) {

    countev++;

    cout << " start event # " << countev << endl;
    fout << " start event # " << countev << endl;

      dgeantBranch=chain->GetDataSet("geantBranch");
      TDataSetIter geantBIter(dgeantBranch);

    if (dgeantBranch) {
        countevgB++;

      cout << endl << " QAInfo: in geantBranch " << endl;
      fout << endl << " QAInfo: in geantBranch " << endl;

      while (ddb=geantBIter.Next()) {

      cout << " QAInfo:   found object: " << ddb->GetName() << endl;
      fout << " QAInfo:   found object: " << ddb->GetName() << endl;
     
      TString dsName =  ddb->GetName();

      if (dsName == "BfcStatus") {
        countevobjb++;
        Countevobjb++;
      }
      else {
        countevobjg++;
        Countevobjg++;
    }

       if (ddb->InheritsFrom("TTable")) { 

        if (dsName == "BfcStatus") {	
          countevtabb++;
          Countevtabb++;
        }
        else{
          countevtabg++;
          Countevtabg++;
        }

         tabl = (TTable *)ddb;
         cout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
         fout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
       }


      } // while geantBranch

    cout << endl << " QAInfo: ev# " << countev << 
            ", #geant obj/tab, #Bfc obj/tab found = " << 
              Countevobjg << "  " << 
              Countevtabg << "  " <<
              Countevobjb << "  " <<  
              Countevtabb << endl << endl;

    fout << endl << " QAInfo: ev# " << countev << 
            ", #geant obj/tab, #Bfc obj/tab found = " << 
              Countevobjg << "  " <<  
              Countevtabg << "  " <<
              Countevobjb << "  " <<  
              Countevtabb << endl << endl;


      } // if geantBranch

    }  // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }

    goto EventLoop;

}  // EventLoop
     
   countevobjg /= countev;
   countevtabg /= countev;
   countevobjb /= countev;
   countevtabb /= countev;

  cout << endl;
  cout << "QAInfo: End of Job " << endl; 
  cout << "QAInfo: # times Make called = " << iev << endl;
  cout << "QAInfo:  # events read = " << countev << endl;
  cout << "QAInfo:   # events with geantBranch dataset = " << 
                      countevgB << endl;
  //  cout << "QAInfo:   # events with geant dataset = " << 
  //                    countevg << endl;
  cout << "QAInfo: avg # geant tables per event  = " << countevtabg << endl;
  cout << "QAInfo: avg # geant objects per event = " << countevobjg << endl;
  cout << "QAInfo: avg # Bfc tables per event    = " << countevtabb << endl;
  cout << "QAInfo: avg # Bfc objects per event   = " << countevobjb << endl << endl;


  fout << endl;
  fout << "QAInfo: End of Job " << endl; 
  fout << "QAInfo: # times Make called = " << iev << endl;
  fout << "QAInfo:  # events read = " << countev << endl;
  fout << "QAInfo:   # events with geantBranch dataset = " << 
                      countevgB << endl;
  //fout << "QAInfo:   # events with geant dataset = " << 
  //                    countevg << endl;
  fout << "QAInfo: avg # geant tables per event  = " << countevtabg << endl;
  fout << "QAInfo: avg # geant objects per event = " << countevobjg << endl;
  fout << "QAInfo: avg # Bfc tables per event    = " << countevtabb << endl;
  fout << "QAInfo: avg # Bfc objects per event   = " << countevobjb << endl << endl;

 chain->Finish();   

}
 




