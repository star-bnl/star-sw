void gaerrors() {
   TCanvas *c1 = new TCanvas("c1");
   c1->SetGrid();
   const Int_t n = 10;
   Float_t x[n];
   Float_t y[n]  = {1,2.9,5.6,7.4,9,9.6,8.7,6.3,4.5,1};
   Float_t exl[n] = {.5,1,.7,.7,.4,.5,.6,.7,.8,.5};
   Float_t eyl[n] = {.8,.7,.6,.5,.4,.4,.5,.6,.7,.8};
   Float_t exh[n] = {.2,.8,.5,.5,.3,.3,.4,.5,.6,.3};
   Float_t eyh[n] = {.6,.5,.4,.3,.2,.2,.3,.4,.5,.6};
   TH1F *h = new TH1F("h","Asymm errors histogram with labels",n,0,n);
   const char *labels[n] = {"Chris","John","Rene","Fons","Marc",
                            "Fred", "Ted","Louis","Ed","Jim"};
   for (Int_t i=0;i<n;i++) {
      x[i] = h->GetXaxis()->GetBinCenter(i+1);
      h->GetXaxis()->SetBinLabel(i+1,labels[i]);
      h->Fill(x[i],y[i]);
   }
   h->Draw("a");
   //h->GetXaxis()->LabelsOption("v");
   gr = new TGraphAsymmErrors(n,x,y,exl,exh,eyl,eyh);
   gr->SetMarkerStyle(21);
   gr->Draw("p");
}

