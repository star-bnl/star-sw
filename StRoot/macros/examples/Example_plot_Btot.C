// $Id: Example_plot_Btot.C,v 1.2 2006/08/15 21:42:52 jeromel Exp $
// $Log: Example_plot_Btot.C,v $
// Revision 1.2  2006/08/15 21:42:52  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.1  2000/01/24 21:29:21  love
// Installed example of POISSON field calculation plotting macro
//
// Revision 1.1  2000/1/24 22:00:30  love

//=======================================================================
// owner: Bill Love
// An example of using the ROOT Ntuple to make a plot of the POISSON
// calculation of the STAR Magnet Full Field.
//=======================================================================
{
 gROOT->Reset();
// Open the root file with bz, br, bt versus r and z in 10 cm steps.
 TFile f("/afs/rhic.bnl.gov/star/data/samples/POIzto6m.root");
// Set a small circle plotting symbol and plot total field vs z at r = 90.
 ntb->SetMarkerStyle(24);
 ntb->SetMarkerSize(.6);
 ntb->Draw("bt:z","r<95 && r>85");
// change to a black dot and add another r value (190).
 ntb->SetMarkerStyle(20);
 ntb->Draw("bt:z","r<195 && r>185","same");
// change to an open square and do the largest r (280 cm)
 ntb->SetMarkerStyle(25);
 ntb->Draw("bt:z"," r>275","same");
// Label the plot nicely.
   text = new TLatex(420,-950,"z(cm)");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   text = new TLatex(-56,11000,"Btot");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   text = new TLatex(38,10750,"STAR Magnet");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   text = new TLatex(38,10000,"POISSON calculation");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   text = new TLatex(336,10780,"r = 90");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   text = new TLatex(326,5000,"r = 190");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   text = new TLatex(170,2000,"r = 280");
   text->SetTextSize(0.05);
   text->SetLineWidth(2);
   text->Draw();
   printf (" to exit type .q\n");
}
