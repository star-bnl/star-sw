// $Id: Example_read_dst_print_tables.C,v 1.2 1999/11/03 16:57:00 kathy Exp $
// $Log: Example_read_dst_print_tables.C,v $
// Revision 1.2  1999/11/03 16:57:00  kathy
// fix macros to use StIOMaker instead of StTreeMaker
//
// Revision 1.1  1999/10/11 17:17:57  kathy
// changed names of some macros to make them more standard; changed default input file to MakeHists since previous no longer existed; combined some macros so that the one example will show all functionality
//
//
//======================================================================
// owner: Kathy Turner  
// what it does: 
// Kathy's notes (10/11/99):
//     - read dstbranch from *.dst.root file
//     - read 1 event and print out information from the tables
//======================================================================

class StChain;
StChain *chain;

class St_DataSet;
St_DataSet *Event;

void Example_read_dst_print_tables(Int_t nevents=1, const char
*MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0208_01_40evts.dst.root")

{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");

    cout << "  .. Example_read_dst_print_tables.C, have loaded libraries " << endl;

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
//  
// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
//  IOMk->SetBranch("tpc_tracks",0,"r"); //activate tpc_tracks Branch
//  IOMk->SetBranch("geantBranch",0,"r"); //activate geant Branch
  IOMk->SetBranch("dstBranch",0,"r"); //activate dst Branch

  
// --- now execute chain member functions
  chain->Init();
 
  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;

    cout << " Now print info about event # " << iev << endl;

// ------ List all tables -----------
// GetDataSet is a member function of St_Maker
    Event = chain->GetDataSet("dst");

// ls() returns a virtual void, so don't have to set it = to anything
    if (Event) Event->ls();
   

// ---------------------- globtrk table ---------------------
//  get dataset for globtrk
St_DataSet *ds=chain->GetDataSet("dst/globtrk");

if (ds) {

 cout << " Now print info about globtrk, row 1 "  << endl;

// create iterator for the dataset
St_DataSetIter globtrkiter(ds);

// Du,Pwd return things, but we choose not to keep the return value
globtrkiter.Du();
globtrkiter.Pwd();

// find the table
St_dst_track *glob = (St_dst_track *) globtrkiter.Find("globtrk");

// print out info about it
// using ls() from St_DataSetIter
glob->ls();

// Print() is a member function of St_Table
//glob->Print(9,1);
//glob->Print(8,1);
//glob->Print(8,2);
//glob->Print(1,1);
glob->Print(0,5);
 
// get the table header data using member function of St_Table
table_head_st *tdt_h = glob->GetHeader();
 cout << " header name   = " << tdt_h->name << endl;
 cout << " header type   = " << tdt_h->type << endl;
 cout << " header maxlen = " << tdt_h->maxlen << endl;
 cout << " header nok    = " << tdt_h->nok << endl;

// get the table and print out info about it (it's printing row 0)
dst_track_st *sth = glob->GetTable();
 cout << " globtrk: row0 " << endl; 
 cout << "  ndegf   = " << sth->ndegf  << endl;
 cout << "  x0      = " << sth->x0     << endl;
 cout << "  impact  = " << sth->impact << endl;
 cout << "  invpt   = " << sth->invpt  << endl;
 cout << "  y0      = " << sth->y0     << endl;
// now go to next row 
//  - just increment the pointer to do this (probably just STAR-specific
//    way - can't always increment pointers and get to right place!

 sth++;
 cout << " globtrk: row1 " << endl; 
 cout << "  ndegf   = " << sth->ndegf  << endl;
 cout << "  x0      = " << sth->x0     << endl;
 cout << "  impact  = " << sth->impact << endl;
 cout << "  invpt   = " << sth->invpt  << endl;
 cout << "  y0      = " << sth->y0     << endl;

}

//--------------------------------------------------------
  }

  chain->Finish();    
  
}
 


        


