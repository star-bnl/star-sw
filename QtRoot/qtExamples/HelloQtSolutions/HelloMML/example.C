{
//=========Macro generated from canvas: c1/c1
//=========  (Sat Jul  4 10:53:09 2009) by ROOT version5.22/00b
// Turn off the latex rendering
   int font =gStyle->GetTextFont()/10;
   gStyle->SetTextFont(font*10+1);

   TCanvas *c1 = new TCanvas();
   c1->Range(0,0,1,1);
   c1->SetBorderSize(2);
   c1->SetFrameFillColor(0);
   
   TPaveLabel *pl = new TPaveLabel(0.2,0.5,0.6,0.7,"@example.mml","br");
   // Turn off the latex rendering
   pl->SetTextFont(61);
   pl->SetTextSize(0.99);
   pl->Draw();
   c1->Modified();
   c1->cd();
   c1->SetSelected(c1);
}
