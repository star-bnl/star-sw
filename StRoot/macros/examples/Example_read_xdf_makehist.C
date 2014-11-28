// $Id: Example_read_xdf_makehist.C,v 1.9 2006/08/15 21:42:58 jeromel Exp $
// $Log: Example_read_xdf_makehist.C,v $
// Revision 1.9  2006/08/15 21:42:58  jeromel
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
// Revision 1.4  2000/01/19 21:00:40  kathy
// update macros to use standard default xdf files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.3  2000/01/06 19:35:48  kathy
// change to use available xdf file as input
//
// Revision 1.2  1999/11/03 19:03:06  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.1  1999/10/11 17:17:59  kathy
// changed names of some macros to make them more standard; changed default input file to MakeHists since previous no longer existed; combined some macros so that the one example will show all functionality
//
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    Reads an xdf file, finds a table (dst/vertex) and fills 2
//    histograms with variable "z" from the table.  Draws hist. to
//    canvas and sends to output ps file at the end.
//
//=======================================================================


void Example_read_xdf_makehist(
  const Char_t *InputXdfFile=
  "/afs/rhic.bnl.gov/star/data/samples/gstar.dst.xdf")
{
 // load libraries 
 gSystem.Load("St_base");
 gSystem.Load("xdf2root");
 gSystem->Load("libglobal_Tables");
 gSystem->Load("libgen_Tables");
 gSystem->Load("libsim_Tables");

 // create instance of St_XDFFile called f1
 // use method OpenXDF of St_XDFFile with instance f1 to open input file
 St_XDFFile f1;
 f1.OpenXDF(InputXdfFile);
 // create a pointer to an TDataSet called record
 TDataSet *record;
 // point record to an event from the XDFFile previously opened to f1
 // using method NextEventGet from St_XDFFile - get first record (run header)
 record = f1.NextEventGet();

 // put  record in browser
 TBrowser b;
 b.Add(record);

  // open output file:
 TFile *hist_outfile = 0;
 const Char_t *root_file = "Example_read_xdf_makehist.root";
 hist_outfile = new TFile(root_file,"RECREATE");

  // book the 1d histogram
  // I am allocating memory on the heap - saved, not deleted (by using new)
  // Then am calling the constructor for TH1F
  // To allocate memory on stack - deleted - and then call constructor do:
  //  TH1F h1("h1","my first hist",100,-10.,10.);
 TH1F *h1 = new TH1F("h1","z position of prim vtx",100,-100.,100.);
  // set x & y titles
 h1->SetXTitle("vertex z position ");
 h1->SetYTitle("num events");
 TH1F *h2 = new TH1F("h2","z position of all vtx in event",100,-100.,100.);
 h2->SetXTitle("vertex z position ");
 h2->SetYTitle("num events");


// now passed header - want to loop over events
 // create a pointer to an TDataSet called recorde
 TDataSet *recorde=0;
 Int_t ijk=0;

 while (recorde=f1.NextEventGet())
  {

  //    cout << " recorde = " << recorde << endl;

  ijk++;
  cout << " ==> event # " << ijk << endl;
 //   create instance of DataSetIter - uses the event record defined previously
  TDataSetIter roote(recorde);
 // now want to go down to a table so create another dataset
  TDataSet *sete=0;
 // now cd to table globtrk 
  sete = roote.Cd("/dst/vertex");
  cout << "   find vertex table pointer = " << sete << endl;
// if pointer is zero, go back to top and read in next event
// last event is really end-run record, so it doesn't have the
// data on it. After this record is passed, the while loop ends
  if (!sete)  continue;
 // now create pointer of type dst_vertex (vertex is of type dst_vertex)
  St_dst_vertex *pdt=0;
 // Set the pointer pdt = to sete which is already a pointer to vertex
 // Must "cast" sete as a type St_dst_vertex for this to work !
 // since sete is a pointer to an TDataSet and St_dst_vertex inherits
 // from TDataSet, you can do this - then can use St_dst_vertex functions
  pdt = (St_dst_vertex *)sete;


// get the table header data using member functions of TTable
   cout << " table header info:  name = " << pdt->GetName() << endl;
   cout << " table header info:  type = " << pdt->GetType() << endl;
   cout << " table header info:  #rows used = " << pdt->GetNRows() << endl;
   cout << " table header info:  #rows allocated = " << pdt->GetTableSize() << endl;
   cout << " table header info:  row size (bytes) = " << pdt->GetRowSize() << endl;
   cout << " table header info:  #columns = " << pdt->GetNumberOfColumns() << endl;

  pdt->ls();
 // now get actual values in table
  dst_vertex_st *tdt_v = pdt->GetTable();
 // print out values:
  cout << "   prim vtx z : " << tdt_v->z   << endl;
  h1->Fill(tdt_v->z);
  // Now setup loop over all rows in table and fill histogram
 Int_t ij = 0;
 for (ij=0; ij< pdt->GetNRows(); ij++)
  { 
    // point to correct row & fill histogram
    // dst_vertex_st *p = tdt_v[ij]; or = tdt_v++
   h2->Fill(tdt_v[ij]->z);
  }
}

 cout << " ==> finished loop" << endl;

  // create canvas to show results - this is how it is displayed
 TCanvas *c1 = new TCanvas("c1"," from table dst/vertex",200,10,600,880);
  // to set grid
 c1->SetGrid();
  // to do zone(1,2)
  c1->Divide(1,2);
  // can also do: c1->SetLogz();

// open output ps file 
 TPostScript ps("Example_read_xdf_makehist.ps",111);

  // update histogram in canvas - can do this every event or at end of loop!
  // Draw histogram  - this should be done ONCE to link hist with canvas
 h1->Draw();
 c1->Update();
 h2->Draw();
 c1->Update(); 

 // Send to output file:
 hist_outfile->Write();
 hist_outfile->ls();

 // close ps file
 ps.Close();

}




