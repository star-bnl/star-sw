TFile *fd=0;

plPro(TString run="R3012013", TString path="./wrkLcpX/") {
  path="/star/data04/sim/balewski/LcpRun2/maxEta1.0";
  // run="R616";
  

  gStyle->SetPalette(1,0);
   gStyle->SetOptStat(1111111);

  
  TString fname=path+"/"+run;  
  fname+=".tree.root";
  fd=new TFile(fname);

  if(!fd->IsOpen()) {
    printf("plCut: %s  not exits \n",fname.Data());
    return;
  }

  printf("plCut: %s  opened \n",fname.Data());
  fd->ls();

  TString cname=run;

  TCanvas *c1=new TCanvas(cname,cname,600,350);
  TH1F* h=0;

  //h=(TH1F*)fd->Get("Vz"); h->Draw();   return;
  h=(TH1F*)fd->Get("bX120"); h->Draw();   return;

  h=(TH1F*)fd->Get("nPrim");// h->Draw();  // return;
     

  c1->Divide(2,1);
  c1->cd(1);
  h->Draw();
  c1->cd(2);
  h1=(TH1F*) h->Clone();
  h1->Draw();
  gPad->SetLogy();
  return;

 mL=new TLegend(0.15,.5,.7,.7);

  // fd->ls();
  h=(TH1F*)fd->Get("CnFP-A"); 
  h->Draw();  mL->AddEntry(h,"#Delta#phi <1/8 #pi (data X 1/10) ");
  h->Scale(0.1);
  
  //  h->SetMaximum(1401);
  //  h->SetMaximum(1800);
 
  h=(TH1F*)fd->Get("CnFP-C"); h->SetLineColor(kBlue);
  h->Draw("same");  mL->AddEntry(h," #Delta#phi #in [ 3/8 #pi, 5/8 #pi ] ");

  h=(TH1F*)fd->Get("CnFP-E"); h->SetLineColor(kRed);
  h->Draw("same");  mL->AddEntry(h,"#Delta#phi > 7/8 #pi  ");

  mL->Draw(); 
  ln->Draw();

  // right plot

  c1->cd(2);
 
  mL=new TLegend(0.15,.45,.7,.70);

  h=(TH1F*)fd->Get("CnFP-A"); 
  h->Draw();  mL->AddEntry(h,"#Delta#phi <1/8 #pi  (data x 1/10)");
  
  h=(TH1F*)fd->Get("CnFP-B"); h->SetLineColor(kBlue);
  h->SetLineStyle(2);
  h->Draw("same");  mL->AddEntry(h," #Delta#phi #in [ 1/8 #pi, 3/8 #pi ] ");

  h=(TH1F*)fd->Get("CnFP-D"); h->SetLineColor(kRed);
  h->Draw("same");  mL->AddEntry(h," #Delta#phi #in [ 5/8 #pi, 7/8 #pi ] ");

  h=(TH1F*)fd->Get("CnFP-L"); h->SetLineColor(kGreen);
  h->Draw("same");  mL->AddEntry(h,"lost LCP");

  h=(TH1F*)fd->Get("CnFP-W"); h->SetLineColor(kRed);
  h->SetLineStyle(2);
  h->Draw("same");  mL->AddEntry(h,"won LCP");

  mL->Draw(); 
  ln->Draw();

  

#if 0
  c1->Divide(2,4);
  c1->cd(1); (fd->Get("CnFP-A"))->Draw();
  c1->cd(2); (fd->Get("CnFP-B"))->Draw();
  c1->cd(3); (fd->Get("CnFP-C"))->Draw();
  c1->cd(4); (fd->Get("CnFP-D"))->Draw();
  c1->cd(5); (fd->Get("CnFP-E"))->Draw();
  c1->cd(7); (fd->Get("CnFP-L"))->Draw();
   c1->cd(8);  (fd->Get("CnFP-W"))->Draw();
#endif
  
}
 
