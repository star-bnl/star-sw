// $Id: Example_look_at_tables_after_bfcread.C,v 1.2 1999/06/07 21:10:35 kathy Exp $
// $Log: Example_look_at_tables_after_bfcread.C,v $
// Revision 1.2  1999/06/07 21:10:35  kathy
// fixing up macros - removed or renamed some, fixed others so the default input file is there
//
// Revision 1.1  1999/06/07 17:31:22  kathy
// clean up some macros
//
// Revision 1.2  1999/05/21 15:33:49  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: see below
//=======================================================================
//
//  Example_look_at_tables_after_bfcread.C
//
// Kathy (5/13/99):
//  This is an example showing how to navigate through tables and
//  look at the data.
//   - I assume you've run $STAR/StRoot/macros/bfcread.C on a DST and
//     are now at the command line in root.
//   - You did chain->Make for 1 event from a *.dst.root file in bfcread.C
//   - Now you just want to go to a table and print out data 
//
//=======================================================================

{
  cout << " !!! ----------------------------------------------- !!! " << endl;
  cout << " !!! This is Example_look_at_tables_after_bfcread.C !!! " << endl;
  cout << "    --> You must have already run bfcread.C beforehand!" << endl;
  cout << " !!! ----------------------------------------------- !!! " << endl;

//  get dataset for globtrk
St_DataSet *ds=chain->GetDataSet("dst/globtrk");

// create iterator for the dataset
St_DataSetIter globtrkiter(ds);
globtrkiter.Du();
globtrkiter.Pwd();

// find the table
St_dst_track *glob = (St_dst_track *) globtrkiter.Find("globtrk");

// print out info about it
glob.ls();
glob->Print(9,1);
glob->Print(8,1);
glob->Print(8,2);
glob->Print(1,1);
glob->Print(0,5);

// get the table header data
table_head_st *tdt_h = glob->GetHeader();
tdt_h->name;
tdt_h->type;
tdt_h->maxlen;
tdt_h->nok;

// get the table and print out info about it (it's printing row 0)
dst_track_st *sth = glob->GetTable();
sth->ndegf;
sth->x0;
sth->impact;
sth->invpt;
sth->y0;


}




