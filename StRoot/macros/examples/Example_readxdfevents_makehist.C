// $Id: Example_readxdfevents_makehist.C,v 1.1 1999/06/18 21:49:27 kathy Exp $ 
// $Log: Example_readxdfevents_makehist.C,v $
// Revision 1.1  1999/06/18 21:49:27  kathy
// new example macro for Lanny,Spiros
//


//=======================================================================
// owner: Kathy Turner
// what it does:
//    Reads an xdf file, finds a table (dst/vertex) and fills
//    histogram with variable "z" from the table.  Draws hist. to
//    canvas and sends to output ps file at the end.
// NOTE: somehow doesn't know when to break out of xdf file, so you
//  must enter the exact # events to run
//=======================================================================


{
 
 gSystem.Load("St_base");
 gSystem.Load("xdf2root");
 gSystem.Load("St_Tables");

 St_XDFFile f1;
 f1.OpenXDF("/disk00000/star/test/rcf013_05_49evts_h_dst.xdf");

 St_DataSet *record;
 record = f1.NextEventGet();

// now passed header - want to loop over events

 TFile *hist_outfile = 0;
 const Char_t *root_file = "Kathy_hist.root";
 hist_outfile = new TFile(root_file,"RECREATE");
 TH1F *h1 = new TH1F("h1","my first hist",100,-0.1,0.1);

 St_DataSet *recorde=0;
 Int_t ijk=0;


for (int ijk=0; ijk<48; ijk++)
  {

 recorde = f1.NextEventGet();
 if (!recorde) break;

 ijk++;
 cout << " event # " << ijk << endl;

  St_DataSetIter roote(recorde);
  St_DataSet *sete=0;
  sete = roote.Cd("/dst/vertex");
  St_dst_vertex *pdt=0;
  pdt = (St_dst_vertex *)sete;
  table_head_st *tdt_h =  pdt->GetHeader();
  cout << "type of object:        " << tdt_h->type   << endl;
  cout << "num of rows of object: " << tdt_h->nok    << endl;
  pdt->ls();
  dst_vertex_st *tdt_v = pdt->GetTable();
  cout << " prim vtx z : " << tdt_v->z   << endl;
  h1->Fill(tdt_v->z);

}

cout << "finished loop" << endl;

 TPostScript ps("kathynew.ps",111);
 TCanvas *c1 = new TCanvas("c1"," from STAF table dst/vertex",200,10,600,880);
 h1->Draw();
 c1->Update();
 hist_outfile->Write();
 hist_outfile->ls();

 ps.Close();

}
