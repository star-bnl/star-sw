// $Id: Example_readxdfevents_makehist.C,v 1.4 1999/07/29 19:57:14 kathy Exp $ 
// $Log: Example_readxdfevents_makehist.C,v $
// Revision 1.4  1999/07/29 19:57:14  kathy
// get working for current version
//
// Revision 1.3  1999/06/24 20:24:57  kathy
// updated comment lines at top
//
// Revision 1.2  1999/06/22 20:50:56  kathy
// fixed macro so its more general
//
// Revision 1.1  1999/06/18 21:49:27  kathy
// new example macro for Lanny,Spiros
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    Reads an xdf file, finds a table (dst/vertex) and fills
//    histogram with variable "z" from the table.  Draws hist. to
//    canvas and sends to output ps file at the end.
//
//=======================================================================


void Example_readxdfevents_makehist(const Char_t *InputXdfFile=
"/disk00000/star/test/new/tfs_Solaris/year_2a/psc0208_01_40evts_dst.xdf")
{
 
 gSystem.Load("St_base");
 gSystem.Load("xdf2root");
 gSystem.Load("St_Tables");

 St_XDFFile f1;
 f1.OpenXDF(InputXdfFile);

 St_DataSet *record;
 record = f1.NextEventGet();

// now passed header - want to loop over events

 TFile *hist_outfile = 0;
 const Char_t *root_file = "Kathy_hist.root";
 hist_outfile = new TFile(root_file,"RECREATE");

 TH1F *h1 = new TH1F("h1","my first hist",100,-100.,100.);

 St_DataSet *recorde=0;
 Int_t ijk=0;

 while (recorde=f1.NextEventGet())
  {

  //    cout << " recorde = " << recorde << endl;

  ijk++;
  cout << " ==> event # " << ijk << endl;

  St_DataSetIter roote(recorde);
  St_DataSet *sete=0;
  sete = roote.Cd("/dst/vertex");
  cout << "   find vertex table pointer = " << sete << endl;
// if pointer is zero, go back to top and read in next event
// last event is really end-run record, so it doesn't have the
// data on it. After this record is passed, the while loop ends
  if (!sete)  continue;
  St_dst_vertex *pdt=0;
  pdt = (St_dst_vertex *)sete;
  table_head_st *tdt_h =  pdt->GetHeader();
  cout << "   type of object:        " << tdt_h->type   << endl;
  cout << "   num of rows of object: " << tdt_h->nok    << endl;
  pdt->ls();
  dst_vertex_st *tdt_v = pdt->GetTable();
  cout << "   prim vtx z : " << tdt_v->z   << endl;
  h1->Fill(tdt_v->z);

}

 cout << " ==> finished loop" << endl;

 TCanvas *c1 = new TCanvas("c1"," from table dst/vertex",200,10,600,880);
 TPostScript ps("MyHist.ps",111);

 h1->Draw();
 c1->Update();

 hist_outfile->Write();
 hist_outfile->ls();

 ps.Close();

}




