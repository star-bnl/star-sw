// $Id: Example_read_dst_write_ntup.C,v 1.1 2000/01/20 22:50:40 kathy Exp $
// $Log: Example_read_dst_write_ntup.C,v $
// Revision 1.1  2000/01/20 22:50:40  kathy
// put in new macro to show how to write a table from a dst.root file to an ntuple and save to an output disk file
//
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    - Reads an .dst.root or .dst.xdf file, 
//    - finds a table (dst/globtrk) 
//    - fills an ntuple with this table. 
//    - Draws some plots from the ntuple.  
//    - Writes out ntuple at end.
//=======================================================================


void Example_read_dst_write_ntup(
  Int_t nevents=3, 
  const Char_t *MainFile=
     "/afs/rhic/star/data/samples/gstar.dst.root",
  const Char_t *NtupFile="test_ntup.root")
{
 
  cout << " nevents to process = " << nevents << endl;
  cout << " Input file       = " << MainFile << endl;
  cout << " Output ntup file = " << NtupFile << endl;


    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
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

    St_DataSet *ds=chain->GetDataSet("dst/globtrk");
    St_DataSetIter dsiter(ds);
    St_dst_track *glob = (St_dst_track *) dsiter.Find("globtrk");

    //    cout << "    globtrk table pointer = " << glob << endl;

// if pointer is zero, go back to top and read in next event
// last event is really end-run record, so it doesn't have the
// data on it. After this record is passed, the while loop ends
  if (!glob)  continue;

// Create ntuple & file in first event!
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
    myNtuple->Fill((St_Table&)*glob);
}

 gStyle->SetOptStat(111111);

 myNtuple->Draw("n_point");

// draw 2D hist with conditions
 myNtuple->Draw("r0:z0","n_point<50 && n_fit_point<40");

// below Draw doesn't work... don't know why
// myNtuple->Draw("x_first[0]:x_first[1]","n_point<50 && n_fit_point<40");

 cout << " ==> finished loop, now close output file" << endl;

// close ntuple in TFile
 ntupfile->Close();

}




