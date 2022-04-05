// $Id: Example_read_dst_write_table_ntup.C,v 1.6 2006/08/15 21:42:56 jeromel Exp $
// $Log: Example_read_dst_write_table_ntup.C,v $
// Revision 1.6  2006/08/15 21:42:56  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.5  2000/05/09 20:15:43  kathy
// transfer obsolete macros to /macros/obsolete;  update other macros so that they use standard default inputs plus only few events by default so they'll be easy to run in autoQA macro testing
//
// Revision 1.4  2000/04/18 20:37:25  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.3  2000/04/13 21:46:21  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.2  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.1  2000/01/21 18:20:45  kathy
// now have macro that writes a table-based ntuple and another that reads the ntuple back in and draws from it
//
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    - Reads an .dst.root produced from bfc
//    - finds a table (dst/globtrk) 
//    - fills an ntuple with this table. 
//    - Draws some plots from the ntuple.  
//    - Writes out ntuple at end.
//=======================================================================


void Example_read_dst_write_table_ntup(
  Int_t nevents=2, 
  const Char_t *MainFile=
     "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root",
  const Char_t *NtupFile="globtrk_ntup.root")
{
 
  cout << " nevents to process = " << nevents << endl;
  cout << " Input file       = " << MainFile << endl;
  cout << " Output ntup file = " << NtupFile << endl;

    gSystem->Load("St_base");
    gSystem->Load("StChain");

    gSystem->Load("libglobal_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");

    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
//  IOMk->SetBranch("tpc_tracks",0,"r"); //activate tpc_tracks Branch
//  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch


// construct output ntuple file
  TFile *ntupfile = 0;

// construct ntuple
  St_TableNtuple *myNtuple=0;

// --- now execute chain member functions
  chain->Init();
 

// loop over events
  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;

    cout << " !!!!! Now read  event # " << iev << endl;

    TDataSet *ds=chain->GetDataSet("dst/globtrk");
    TDataSetIter dsiter(ds);
    St_dst_track *glob = (St_dst_track *) dsiter.Find("globtrk");

    //    cout << "    globtrk table pointer = " << glob << endl;

// if pointer is zero, go back to top and read in next event
// last event is really end-run record, so it doesn't have the
// data on it. After this record is passed, the while loop ends

  if (!glob)  continue;

// Create ntuple & file in first event!
// Ntuple gets named "globtrk" here!
// Must have variable list setup already in order to define an ntuple
// We don't have this done until after we get to the table in the
//  first event - so am defining the ntuple below.
// See Manuel's StMCAnalysisMaker class to see how to setup and fill
//   a general (not necessarily table) ntuple!
// I *think* that this ntuple gets written to the last TFile opened!
//

    if (iev==0){
      cout << " CREATE NTUPLE! " << endl;
      ntupfile = new TFile(NtupFile,"RECREATE","My Ntuple");
      myNtuple = new St_TableNtuple((St_Table&)*glob);
    }

   
// fill ntuple
    myNtuple->Fill((TTable&)*glob);
}

 cout << " ==> finished loop, now write ntuple to output file" << endl;
// write ntuple in memory to the Tfile:
 myNtuple->Write();

// draw some things from ntuple which is in memory
 gStyle->SetOptStat(111111);
 myNtuple->Draw("n_point");
 myNtuple->Draw("r0:z0","n_point<50 && n_fit_point<40");
 myNtuple->Draw("x_first0:x_first1","n_point<50 && n_fit_point<40");

 cout << " ==> close output file" << endl;
// close ntuple in TFile
 ntupfile->Close();

}




