void mml3() {
   //example illustrating a TPaveText with Latex inside
   //Author: Rene Brun
   
   TCanvas *mml = new TCanvas("mml");
   TPaveText *pt = new TPaveText(.05,.1,.95,.8);
   pt->SetTextFont(61); //turn off latex (mis)aligment
   pt->AddText("@latex3.mml");

   pt->SetLabel("Born equation");
   pt->Draw();
}
