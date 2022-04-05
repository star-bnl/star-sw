// $Id: bfcread_dstBranch.C,v 1.18 2006/08/15 21:43:08 jeromel Exp $
// $Log: bfcread_dstBranch.C,v $
// Revision 1.18  2006/08/15 21:43:08  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.17  2000/06/20 14:03:04  kathy
// now unpack and print out BfcStatus table, if it exists, from .event.root file
//
// Revision 1.16  2000/06/19 16:19:56  kathy
// fixed counthold defnition
//
// Revision 1.15  2000/06/13 15:41:44  kathy
// add a few more counters so we know how many events have dst tables and how many have RunEvent object
//
// Revision 1.14  2000/06/02 19:33:35  kathy
// now unpack and print out contents of BfcStatus table every event
//
// Revision 1.13  2000/06/01 18:57:03  kathy
// updating to separate out BfcStatus stats
//
// Revision 1.12  2000/06/01 18:13:27  kathy
// update to print out info needed properly
//
// Revision 1.11  2000/05/31 21:25:17  kathy
// updated so it now finds all tables/objects (e.g. BfcStatus) under dstBranch
//
// Revision 1.10  2000/05/03 18:36:30  kathy
// ooopps - my mistake - autoQA is using bfcread_dstBranch now instead of QA_bfcread_dst_tables - so put it back in...
//
//======================================================================
// owner:  Kathy Turner
// what it does:  reads .dst.root file produced from bfc & then goes to
//                the dst branch
//                - finds data set "dst"
//                - loops over and prints out list of objects 
//                   and tables
//
// Inputs to macro:
//   nevents  -  # events to process
//   MainFile - input *.dst.root file  (you can use any branch here)
//   fname    - output file name with qa info
//   
//
//======================================================================

class StChain;
StChain *chain;

void bfcread_dstBranch(
 Int_t nevents=2, 
 const char *MainFile=
 "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
  const char *fname="qa_dst.out")
{
//
  cout << " events to process  = " << nevents << endl;
  cout << " Input File Name = " << MainFile << endl;
  cout << " Output file containing printouts = " << fname << endl;

  ofstream fout(fname);

  fout << " Running: bfcread_dstBranch.C " << endl;
  fout << " events to process  = " << nevents << endl;
  fout << " Input File Name = " << MainFile << endl;
  fout << " Output file containing printouts = " << fname << endl;
  fout << endl << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StIOMaker");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch

// --- now execute chain member functions
  chain->Init();

  TTable   *tabl=0;

  TDataSet *obj=0;
  TDataSet *ddb=0;
  TDataSet *ddstBranch=0;

  int istat=0;
  int iev=0;

  Float_t countev=0.0;

  Float_t countevdstB=0.0;
  Float_t countevdst=0.0;
  Float_t countevdstTab=0.0;
  Float_t countevdstRE=0.0;
  Float_t countevobjd=0.0;
  Float_t countevtabd=0.0;
  Float_t countevobjb=0.0;
  Float_t countevtabb=0.0;

// Event loop
EventLoop: if (iev < nevents && !istat) {

  Int_t Countevobjd=0;
  Int_t Countevtabd=0;
  Int_t Countevobjb=0;
  Int_t Countevtabb=0;

  Int_t counthold=0;
  Int_t countre=0;

    chain->Clear();
    istat = chain->Make(iev);
    
//  count # times Make is called
    iev++;

    cout << " Call Make # " << iev << endl; 
    cout << "     istat value returned from chain Make = " << istat << endl;

    if (!istat) {

    countev++;
    cout << " start event # " << countev << endl;
    fout << " start event # " << countev << endl;

    ddstBranch=chain->GetDataSet("dstBranch");

    TDataSetIter dstbranchIter(ddstBranch);

    if (ddstBranch) {

    countevdstB++;

    cout << endl << " QAInfo: in dstBranch " << endl;
    fout << endl << " QAInfo: in dstBranch " << endl;

    while (ddb=dstbranchIter.Next()) {

      cout << endl << " QAInfo:   found object: " << ddb->GetName() << endl;
      fout << endl << " QAInfo:   found object: " << ddb->GetName() << endl;
           
      TString dsName =  ddb->GetName();

      if (dsName == "BfcStatus") {
        countevobjb++;
        Countevobjb++;
      }
      else {
        countevobjd++;
        Countevobjd++;
      }

      if (ddb->InheritsFrom("TTable")) { 

         tabl = (TTable *)ddb;
         cout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
         fout << " QAInfo:     it's a table with #rows = " 
                        << tabl->GetNRows() << endl;

        if (dsName == "BfcStatus") {	
          countevtabb++;
          Countevtabb++;
// Now print out contents of BfcStatus for QA purposes
            TDataSetIter bfcstatiter(ddb);
            St_dst_bfc_status *bfcstat = 
              (St_dst_bfc_status *) bfcstatiter.Find("BfcStatus");
            dst_bfc_status_st *bth = bfcstat->GetTable();
//  loop over all rows in table BfcStatus:
            Int_t ij = 0;
            for (ij=0; ij< bfcstat->GetNRows(); ij++)
            {
	      cout << " QAInfo:       BfcStatus table -- row " << ij <<
		", Maker: "     <<  bth[ij]->maker_name <<
                " has istat = "  <<  bth[ij]->status << endl;
	      fout << " QAInfo:       BfcStatus table -- row " << ij <<
		", Maker: "     <<  bth[ij]->maker_name <<
                " has istat = "  <<  bth[ij]->status << endl;
             }       
        }
        else{
          countevtabd++;
          Countevtabd++;
        }
 
      }

// now look under dst branch
      if (dsName == "dst") {

        countevdst++;

    cout << " QAInfo:    in dst object " << endl;
    fout << " QAInfo:    in dst object " << endl;

// look for dst objects/tables

        TDataSetIter tabiter(ddb);

//        ddb->ls(2);  
        while (obj = tabiter.Next()) {

	  cout << " QAInfo:     found object: " << obj->GetName() << endl;
	  fout << " QAInfo:     found object: " << obj->GetName() << endl;

          countevobjd++;
          Countevobjd++;

         TString dstobjName =  obj->GetName();
         if (dstobjName == "RunEvent") {
	   countre=1;
        }

//.. count all tables that exist:
          if (obj->InheritsFrom("TTable")) {
            tabl = (TTable *)tabiter.Find(obj->GetName());
            if (tabl) {
              counthold=1;
	      countevtabd++;
              Countevtabd++;

             cout << " QAInfo:       it's a table with #rows = " 
                        << tabl->GetNRows() << endl;
             fout << " QAInfo:       it's a table with #rows = " 
                        << tabl->GetNRows() << endl;

	    } // tabl
          }  // obj
//.. end of counting all tables that exist

	}  // while obj

      } // dsName = dst


    } // while dstBranch

    cout << endl << " QAInfo: ev# " << countev << 
            ", #dst obj/tab, #Bfc obj/tab found = " << 
              Countevobjd << "  " << 
              Countevtabd << "  " <<
              Countevobjb << "  " <<  
              Countevtabb << endl << endl;

    fout << endl << " QAInfo: ev# " << countev << 
            ", #dst obj/tab, #Bfc obj/tab found = " << 
              Countevobjd << "  " <<  
              Countevtabd << "  " <<
              Countevobjb << "  " <<  
              Countevtabb << endl << endl;


      } // if dstBranch

      if (counthold) countevdstTab++;
      if (countre)   countevdstRE++;

    }  // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
      }

    goto EventLoop;

}  // EventLoop
     
    countevobjd /= countev;
    countevtabd /= countev;
    countevobjb /= countev;
    countevtabb /= countev;

  cout << endl;
  cout << " QAInfo: End of Job " << endl; 
  cout << " QAInfo: # times Make called = " << iev << endl;
  cout << " QAInfo:  # events read = " << countev << endl;
  cout << " QAInfo:   # events with dstBranch dataset = " << countevdstB << endl;
  cout << " QAInfo:    # events with dst dataset = " <<  countevdst << endl;
  cout << " QAInfo:     # events with RunEvent object = " <<  countevdstRE << endl;
  cout << " QAInfo:     # events with dst Tables = " <<  countevdstTab << endl;
  cout << " QAInfo: avg # dst tables per event  = " << countevtabd << endl;
  cout << " QAInfo: avg # dst objects per event = " << countevobjd << endl;
  cout << " QAInfo: avg # Bfc tables per event  = " << countevtabb << endl;
  cout << " QAInfo: avg # Bfc objects per event = " << countevobjb << endl << endl;

  fout << endl;
  fout << " QAInfo: End of Job " << endl; 
  fout << " QAInfo: # times Make called = " << iev << endl;
  fout << " QAInfo:  # events read = " << countev << endl;
  fout << " QAInfo:   # events with dstBranch dataset = " << countevdstB << endl;
  fout << " QAInfo:    # events with dst dataset = " <<  countevdst << endl;
  fout << " QAInfo:     # events with RunEvent object = " <<  countevdstRE << endl;
  fout << " QAInfo:     # events with dst Tables = " <<  countevdstTab << endl;
  fout << " QAInfo: avg # dst tables per event  = " << countevtabd << endl;
  fout << " QAInfo: avg # dst objects per event = " << countevobjd << endl;
  fout << " QAInfo: avg # Bfc tables per event  = " << countevtabb << endl;
  fout << " QAInfo: avg # Bfc objects per event = " << countevobjb << endl << endl;

  chain->Finish();   

}


