// $Id: Example_read_xdffile_make_hist.C,v 1.2 1999/05/21 15:33:48 kathy Exp $
// $Log: Example_read_xdffile_make_hist.C,v $
// Revision 1.2  1999/05/21 15:33:48  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: 
//=======================================================================
// root_read_xdf_file_make_hist.C
//
// - minimal set of commands to open xdf file and look at a table &
//   then make a histogram and write to output ps file
// - this is combination of root_look_at_table.C & root_make_histogram.C
//
{
 // load libraries
 gSystem.Load("St_base");
 gSystem.Load("xdf2root");
 gSystem.Load("St_Tables");
 // create instance of St_XDFFile called f1
 // use method OpenXDF of St_XDFFile with instance f1 to open input file
 St_XDFFile f1;
 f1.OpenXDF("/disk00000/star/mdc1_test_data/mdc1_year2a_psc079_01_46evts_dst.xdf");
 // create a pointer to an St_DataSet called record
 St_DataSet *record;
 St_DataSet *recorde;
 // point record to an event from the XDFFile previously opened to f1
 // using method NextEventGet from St_XDFFile - get first 2 records (run & dst)
 record = f1.NextEventGet();
 recorde = f1.NextEventGet();
 // put these 2 records in browser
 TBrowser b;
 b.Add(record);
 b.Add(recorde);
 //   create instance of DataSetIter - uses the event record defined previousl 
 St_DataSetIter root(record);
 St_DataSetIter roote(recorde);
 // now want to go down to a table in the 2nd record
 St_DataSet *sete=0;
 // now cd to table globtrk 
 sete = roote.Cd("/dst/globtrk");
 // now create pointer of type dst_track (globtrk is type of dst_track)
 St_dst_track *pdt;
 // Set the pointer pdt = to sete which is already a pointer to globtrk
 // Must "cast" sete as a type St_dst_track for this to work !
 // since sete is a pointer to an St_DataSet and St_dst_track inherits
 // from St_DataSet, you can do this - then can use St_dst_track functions
 pdt = (St_dst_track *)sete;
 // create pointer of type table_head_st and use function GetHeader in St_Table
 // from which St_dst_track inherits 
 table_head_st *tdt_h =  pdt->GetHeader();
 // print out results 
 cout << "name of object:        " << tdt_h->name   << endl;
 cout << "type of object:        " << tdt_h->type   << endl;
 cout << "maxlen of object:      " << tdt_h->maxlen << endl;
 cout << "num of rows of object: " << tdt_h->nok    << endl;
 pdt->ls();
 // now get actual values in table
 dst_track_st *tdt_v = pdt->GetTable();
 // print out values:
 cout << " number of points on track: " << tdt_v->n_point   << endl;
 cout << " x0 value: " << tdt_v->x0   << endl;
  // open output file:
 TFile *hist_outfile = 0;
 const Char_t *root_file = "Kathy_hist.root";
 hist_outfile = new TFile(root_file,"RECREATE");
  // book the 1d histogram
  // I am allocating memory on the heap - saved, not deleted (by using new)
  // Then am calling the constructor for TH1F
  // To allocate memory on stack - deleted - and then call constructor do:
  //  TH1F h1("h1","my first hist",100,-10.,10.);
 TH1F *h1 = new TH1F("h1","my first hist",100,-10.,10.);
  // set x & y titles
 h1->SetXTitle("x coor at start ");
 h1->SetYTitle("num events");
  // create canvas to show results - this is how it is displayed
 TCanvas *c1 = new TCanvas("c1"," from STAF table dst/globtrk",200,10,600,880);
  // to set grid
 c1->SetGrid();
  // to do zone(1,2)
  c1->Divide(1,2);
  // can also do: c1->SetLogz();
  // Now setup loop and fill histogram
 Int_t ijk = 0;
 for (ijk=0; ijk< pdt->GetNRows(); ijk++)
  { 
    // point to correct row & fill histogram
    // dst_track_st *p = tdt_v[ijk]; or = tdt_v++
   h1->Fill(tdt_v[ijk]->x0);
  }
  // update histogram in canvas - can do this every event or at end of loop!
  // Draw histogram  - this should be done ONCE to link hist with canvas
 h1->Draw("e1p");
 c1->Update();
 // Send to output file:
 hist_outfile->Write();
 hist_outfile->ls();
}




