{
//=========Macro generated from canvas: c1/c1
//=========  (Thu Aug 12 15:35:25 1999) by ROOT version 2.22/06
   TCanvas *c1 = new TCanvas("c1", "c1",0,3,799,598);
   gStyle->SetOptFit(1);
   c1->Range(-0.785398,-0.13125,7.06858,1.18125);
   c1->SetFillColor(10);
   c1->SetBorderSize(2);
   
   Flow_Psi_Pair1_Har1->GetXaxis()->SetTitleOffset(1.2);
   Flow_Psi_Pair1_Har1->Draw("");
   c1->Modified();
   c1->cd();
}
