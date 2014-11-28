// $Id: Example_read_dst_print_tables.C,v 1.9 2006/08/15 21:42:55 jeromel Exp $
// $Log: Example_read_dst_print_tables.C,v $
// Revision 1.9  2006/08/15 21:42:55  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.8  2000/06/05 16:35:35  kathy
//  remove use of member function GetHeader since it is no longer available - now use memb functions of TTable
//
// Revision 1.7  2000/04/18 20:37:25  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.6  2000/04/13 21:46:21  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.5  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.4  2000/01/19 15:46:04  kathy
// change default input files to point to ones in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.3  1999/11/30 20:04:17  kathy
// fix Example macros so that they work from .dst.root files or .dst.xdf files & update documentation; also had to change which values printed in *read_dst_print_tables* macro since the names have changed in dst tables
//
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
//     - read dstbranch from *.dst.root or *.dst.xdf file
//     - read 1 event and print out information from the tables
//======================================================================

class StChain;
StChain *chain;

class TDataSet;
TDataSet *Event;

void Example_read_dst_print_tables(
 Int_t nevents=1, 
 const char *MainFile=
"/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root")
{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");

    gSystem->Load("libglobal_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libtpc_Tables");

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

    cout << endl << endl << 
       " !!! Now print info about event # " << iev << endl;

// ------ List all tables -----------
// GetDataSet is a member function of St_Maker
    Event = chain->GetDataSet("dst");

// ls() returns a virtual void, so don't have to set it = to anything
    if (Event) Event->ls();
   

// ---------------------- globtrk table ---------------------
//  get dataset for globtrk
TDataSet *ds=chain->GetDataSet("dst/globtrk");

if (ds) {

 cout << " Now print info about globtrk table "  << endl;

// create iterator for the dataset
TDataSetIter globtrkiter(ds);

// Du,Pwd return things, but we choose not to keep the return value
globtrkiter.Du();
globtrkiter.Pwd();

// find the table
St_dst_track *glob = (St_dst_track *) globtrkiter.Find("globtrk");

// print out info about it
// using ls() from TDataSetIter
glob->ls();

// Print() is a member function of TTable
//glob->Print(9,1);
//glob->Print(8,1);
//glob->Print(8,2);
//glob->Print(1,1);
glob->Print(0,5);
 

// can't use this method anymore!!! 5June00 Kathy
// get the table header data using member function of TTable
//table_head_st *tdt_h =  glob->GetHeader();
// cout << " header name   = " << tdt_h->name << endl;
// cout << " header type   = " << tdt_h->type << endl;
// cout << " header maxlen = " << tdt_h->maxlen << endl;
// cout << " header nok    = " << tdt_h->nok << endl;


// get the table header data using member functions of TTable
   cout << " table header info:  name = " << glob->GetName() << endl;
   cout << " table header info:  type = " << glob->GetType() << endl;
   cout << " table header info:  #rows used = " << glob->GetNRows() << endl;
   cout << " table header info:  #rows allocated = " << glob->GetTableSize() << endl;
   cout << " table header info:  row size (bytes) = " << glob->GetRowSize() << endl;
   cout << " table header info:  #columns = " << glob->GetNumberOfColumns() << endl;

// get the table and print out info about it (it's printing row 0)
 dst_track_st *sth = glob->GetTable();
 cout << " globtrk: row0 " << endl; 
 cout << "  r0      = " << sth->r0     << endl;
 cout << "  impact  = " << sth->impact << endl;
 cout << "  invpt   = " << sth->invpt  << endl;
 cout << "  z0      = " << sth->z0     << endl;

// now go to next row 
//  - just increment the pointer to do this (probably just STAR-specific
//    way - can't always increment pointers and get to right place!

 sth++;
 cout << " globtrk: row1 " << endl; 
 cout << "  r0      = " << sth->r0     << endl;
 cout << "  impact  = " << sth->impact << endl;
 cout << "  invpt   = " << sth->invpt  << endl;
 cout << "  z0      = " << sth->z0     << endl;

}

//--------------------------------------------------------
  }

  chain->Finish();    
  
}
 


        


