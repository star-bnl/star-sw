// $Id: Example_readdst_qa_tables.C,v 1.2 1999/07/13 01:13:01 kathy Exp $
// $Log: Example_readdst_qa_tables.C,v $
// Revision 1.2  1999/07/13 01:13:01  kathy
// moved rch.C to obsolete, put in id,log,owner into HbtExample, removed loading of StRootEvent and changed default input file in bfcread.C and Example_readdst_qa_tables.C
//
// Revision 1.1  1999/06/22 21:25:29  kathy
// example to read read dst.root file and print out info about tables
//
//
//======================================================================
// owner: Kathy Turner  
// what it does: 
//=======================================================================
// Example_readdst_qa_tables.C
//
// Kathy's notes (6/22/99):
//     - read dstbranch from *.dst.root file
//     - read 1 event and print out information from the tables
//======================================================================

class StChain;
StChain *chain;

class St_DataSet;
St_DataSet *Event;

void Example_readdst_qa_tables(Int_t nevents=1, const char
*MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0208_01_40evts.dst.root")

{
//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");

    cout << "  .. Example_readdst_qa_tables.C, have loaded libraries " << endl;

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
//  Input Tree
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  treeMk->SetBranch("dstBranch",0,"r");	//activate EventBranch

  
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
 


        


