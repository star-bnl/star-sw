void doBsmdScaleRatio(char *f_100,char *f_110,char *f_90){

  gStyle->SetErrorX(0);

  TFile *f_nom=new TFile(f_100,"OPEN");
  TH1F *h_effHT2=f_nom->Get("h_effHT2");
  TH1F *h_effDHT2=f_nom->Get("h_effDaughtersHT2");
  h_effHT2->Divide(h_effDHT2);

  TFile *f_plus=new TFile(f_110,"OPEN");
  TH1F *h_effHT2_plus=f_plus->Get("h_effHT2");
  TH1F *h_effDHT2_plus=f_plus->Get("h_effDaughtersHT2");  
  h_effHT2_plus->Divide(h_effDHT2_plus);

  TFile *f_min=new TFile(f_90,"OPEN");
  TH1F *h_effHT2_min=f_min->Get("h_effHT2");
  TH1F *h_effDHT2_min=f_min->Get("h_effDaughtersHT2");
  h_effHT2_min->Divide(h_effDHT2_min);


  h_effHT2_plus->Divide(h_effHT2);
  h_effHT2_min->Divide(h_effHT2);


  TF1 *sub=new TF1("sub","1.",0.,20.);
  h_effHT2_plus->Add(sub,-1.);
  h_effHT2_min->Add(sub,-1.);

  TGraphErrors *gHT2_plus=new TGraphErrors(h_effHT2_plus);
  gHT2_plus->SetName("gHT2_plus");
  TGraphErrors *gHT2_min=new TGraphErrors(h_effHT2_min);
  gHT2_min->SetName("gHT2_min");

  TCanvas *c=new TCanvas("c","c",400,250);
  TMultiGraph *m=new TMultiGraph();
  TMultiGraph *mm=new TMultiGraph();

  gHT2_plus->SetLineColor(2);
  gHT2_min->SetLineColor(2);

  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);

  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);

  TF1 *fit=new TF1("fit","[0]",1.,15.);

  TF1 *fit2=new TF1("fit2","[0]",1.,15.);
 

 
  m->Add(gHT2_plus);
  m->Fit(fit,"R0");

  mm->Add(gHT2_min);
  mm->Fit(fit2,"R0");

  m->Add(gHT2_min);

  m->SetMinimum(-1.);
  m->SetMaximum(1.);

  m->Draw("ap");
  fit->Draw("same");
  fit2->Draw("same");

  TLegend *leg=new TLegend(0.5,0.5,0.7,0.7);
  leg->AddEntry(gHT2_plus,"BSMD E scale +100%","p");
  leg->AddEntry(gHT2_min,"BSMD E scale -100%","p");
  leg->Draw();

  c->SaveAs("bsmd_escale_ratio_pp.eps");
  c->SaveAs("bsmd_escale_ratio_pp.root");
  
}
