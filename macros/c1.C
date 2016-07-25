{
//=========Macro generated from canvas: c1/c1
//=========  (Tue Dec 26 20:42:13 2000) by ROOT version2.26/00
   TCanvas *c1 = new TCanvas("c1", "c1",10,10,700,500);
   gStyle->SetOptFit(1);
   c1->Range(-0.1375,-2,2.2375,2);
   c1->SetFillColor(10);
   c1->SetBorderMode(0);
   c1->SetBorderSize(2);
   c1->SetFrameBorderMode(0);
   
   pipT09_16N->GetXaxis()->SetTitleOffset(1.2);
   pipT09_16N->Draw("colz");
   
   TPaveText *pt = new TPaveText(0.0100575,0.867089,0.969828,0.995781,"blNDC");
   pt->SetBorderSize(2);
   pt->SetFillColor(10);
   TText *text = pt->AddText("Faction for pipT09_16NCentrality = 9, Charge = -1");
   pt->Draw();
   c1->Modified();
   c1->cd();
}
