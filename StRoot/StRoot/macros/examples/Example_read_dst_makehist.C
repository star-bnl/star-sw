// $Id: Example_read_dst_makehist.C,v 1.10 2006/08/15 21:42:54 jeromel Exp $
// $Log: Example_read_dst_makehist.C,v $
// Revision 1.10  2006/08/15 21:42:54  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.9  2000/06/05 16:35:35  kathy
//  remove use of member function GetHeader since it is no longer available - now use memb functions of TTable
//
// Revision 1.8  2000/04/18 20:37:24  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.7  2000/04/13 21:46:21  kathy
// remove loading of libtpc_Tables since l3Track table is now dst_track type from global
//
// Revision 1.6  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.5  2000/01/19 15:46:04  kathy
// change default input files to point to ones in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.4  1999/11/30 20:04:17  kathy
// fix Example macros so that they work from .dst.root files or .dst.xdf files & update documentation; also had to change which values printed in *read_dst_print_tables* macro since the names have changed in dst tables
//
// Revision 1.3  1999/11/03 19:03:05  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.2  1999/11/03 16:56:59  kathy
// fix macros to use StIOMaker instead of StTreeMaker
//
// Revision 1.1  1999/10/11 17:17:56  kathy
// changed names of some macros to make them more standard; changed default input file to MakeHists since previous no longer existed; combined some macros so that the one example will show all functionality
//
// Revision 1.1  1999/09/28 21:59:08  kathy
// add new macro to show how to read .dst.root file, find a value in a table and make a histogram of it and print out
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    Reads an .dst.root or .dst.xdf file, finds a table (dst/vertex) and fills
//    histogram with variable "z" from the table.  Draws hist. to
//    canvas and sends to output ps file at the end.
//
//=======================================================================


void Example_read_dst_makehist(
  Int_t nevents=3, 
  const char *MainFile=
"/afs/rhic.bnl.gov/star/data/samples/gstar.dst.root")
{
 
    gSystem->Load("St_base");
    gSystem->Load("StChain");

    gSystem->Load("libglobal_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");

    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");


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

  
// --- now execute chain member functions
  chain->Init();
 
// open output hist file 
 TFile *hist_outfile = 0;
 const Char_t *root_file = "Example_read_dst_makehist.root";
 hist_outfile = new TFile(root_file,"RECREATE");

// book histogram
 TH1F *h1 = new TH1F("h1","z position of primary vertex",100,-100.,100.);
   h1->SetXTitle("vertex z position ");
   h1->SetYTitle("num events");
 TH1F *h2 = new TH1F("h2","z position of all vtx in event",100,-100.,100.);
   h2->SetXTitle("vertex z position ");
   h2->SetYTitle("num events");

  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;

    cout << " !!!!! Now read  event # " << iev << endl;

    TDataSet *ds=chain->GetDataSet("dst/vertex");
    TDataSetIter dsiter(ds);
    St_dst_vertex *vert = (St_dst_vertex *) dsiter.Find("vertex");

  cout << "    vertex table pointer = " << vert << endl;

// if pointer is zero, go back to top and read in next event
// last event is really end-run record, so it doesn't have the
// data on it. After this record is passed, the while loop ends
  if (!vert)  continue;


// get the table header data using member functions of TTable
   cout << " table header info:  name = " << vert->GetName() << endl;
   cout << " table header info:  type = " << vert->GetType() << endl;
   cout << " table header info:  #rows used = " << vert->GetNRows() << endl;
   cout << " table header info:  #rows allocated = " << vert->GetTableSize() << endl;
   cout << " table header info:  row size (bytes) = " << vert->GetRowSize() << endl;
   cout << " table header info:  #columns = " << vert->GetNumberOfColumns() << endl;


  vert->ls();

// get actual values in table

  dst_vertex_st *sth = vert->GetTable();
  cout << "   prim vtx z : " << sth->z   << endl;

// fill hist h1
  h1->Fill(sth->z);

// Now setup loop over all rows in table and fill histogram h2
 Int_t ij = 0;
 for (ij=0; ij< vert->GetNRows(); ij++)
  { 
   h2->Fill(sth[ij]->z);
  }

}

 cout << " ==> finished loop" << endl;

 TCanvas *c1 = new TCanvas("c1"," from table dst/vertex",200,10,600,880);

  // to set grid
 c1->SetGrid();
  // to do zone(1,2)
  c1->Divide(1,2);
  // can also do: c1->SetLogz();

 TPostScript ps("Example_read_dst_makehist.ps",111);

  // update histogram in canvas - can do this every event or at end of loop!
  // Draw histogram  - this should be done ONCE to link hist with canvas
 h1->Draw();
 c1->Update();
 h2->Draw();
 c1->Update(); 

 hist_outfile->Write();
 hist_outfile->ls();

 ps.Close();

}




