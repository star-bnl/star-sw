void doBsmdScale(char *f_100,char *f_110,char *f_90){

  gStyle->SetErrorX(0);

  TFile *f_nom=new TFile(f_100,"OPEN");
  TH1F *h_effMB=f_nom->Get("h_effMB");
  TH1F *h_effHT1=f_nom->Get("h_effHT1");
  TH1F *h_effHT2=f_nom->Get("h_effHT2");

  TFile *f_plus=new TFile(f_110,"OPEN");
  TH1F *h_effMB_plus=f_plus->Get("h_effMB");
  TH1F *h_effHT1_plus=f_plus->Get("h_effHT1");
  TH1F *h_effHT2_plus=f_plus->Get("h_effHT2");
  
  TFile *f_min=new TFile(f_90,"OPEN");
  TH1F *h_effMB_min=f_min->Get("h_effMB");
  TH1F *h_effHT1_min=f_min->Get("h_effHT1");
  TH1F *h_effHT2_min=f_min->Get("h_effHT2");
  
  h_effMB_plus->Divide(h_effMB);
  h_effHT1_plus->Divide(h_effHT1);
  h_effHT2_plus->Divide(h_effHT2);

  h_effMB_min->Divide(h_effMB);
  h_effHT1_min->Divide(h_effHT1);
  h_effHT2_min->Divide(h_effHT2);

  TF1 *sub=new TF1("sub","1.",0.,20.);
  h_effMB_plus->Add(sub,-1.);
  h_effHT1_plus->Add(sub,-1.);
  h_effHT2_plus->Add(sub,-1.);
  h_effMB_min->Add(sub,-1.);
  h_effHT1_min->Add(sub,-1.);
  h_effHT2_min->Add(sub,-1.);

  TGraphErrors *gMB_plus=new TGraphErrors(h_effMB_plus);
  gMB_plus->SetName("gMB_plus");
  TGraphErrors *gHT1_plus=new TGraphErrors(h_effHT1_plus);
  gHT1_plus->SetName("gHT1_plus");
  TGraphErrors *gHT2_plus=new TGraphErrors(h_effHT2_plus);
  gHT2_plus->SetName("gHT2_plus");

  TGraphErrors *gMB_min=new TGraphErrors(h_effMB_min);
  gMB_min->SetName("gMB_min");
  TGraphErrors *gHT1_min=new TGraphErrors(h_effHT1_min);
  gHT1_min->SetName("gHT1_min");
  TGraphErrors *gHT2_min=new TGraphErrors(h_effHT2_min);
  gHT2_min->SetName("gHT2_min");

  TCanvas *c=new TCanvas("c","c",400,250);
  TMultiGraph *m=new TMultiGraph();
  TMultiGraph *mm=new TMultiGraph();

  gMB_plus->SetLineColor(1);
  gMB_min->SetLineColor(1);
  gHT1_plus->SetLineColor(4);
  gHT1_min->SetLineColor(4);
  gHT2_plus->SetLineColor(2);
  gHT2_min->SetLineColor(2);

  gMB_plus->RemovePoint(0);
  gMB_plus->RemovePoint(0);
  gHT1_plus->RemovePoint(0);
  gHT1_plus->RemovePoint(0);
  gHT1_plus->RemovePoint(0);
  gHT1_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);
  gHT2_plus->RemovePoint(0);

  gMB_min->RemovePoint(0);
  gMB_min->RemovePoint(0);
  gHT1_min->RemovePoint(0);
  gHT1_min->RemovePoint(0);
  gHT1_min->RemovePoint(0);
  gHT1_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);
  gHT2_min->RemovePoint(0);

  TF1 *fit=new TF1("fit","[0]+[1]*exp([2]*x)",1.,15.);
  fit->SetParameters(0.,1.,-1.);
  TF1 *fit2=new TF1("fit2","[0]+[1]*exp([2]*x)",1.,15.);
  fit2->SetParameters(0.,1.,-1.);

 

 
  m->Add(gMB_plus);
  m->Add(gHT1_plus);
  m->Add(gHT2_plus);
  m->Fit(fit,"R0");

  mm->Add(gMB_min);
  mm->Add(gHT1_min);
  mm->Add(gHT2_min);
  mm->Fit(fit2,"R0");

  m->Add(gMB_min);
  m->Add(gHT1_min);
  m->Add(gHT2_min);

  m->SetMinimum(-1.);
  m->SetMaximum(1.);

  m->Draw("ap");
  fit->Draw("same");
  fit2->Draw("same");

  TLegend *leg=new TLegend(0.5,0.5,0.7,0.7);
  leg->AddEntry(gMB_plus,"minimum bias, BSMD E scale +10%","p");
  leg->AddEntry(gHT1_plus,"hightower 1","p");
  leg->AddEntry(gHT2_plus,"hightower 2","p");
  leg->AddEntry(gMB_min,"BSMD E scale -10%","p");
  leg->Draw();

  c->SaveAs("bsmd_escale_pp.eps");
  c->SaveAs("bsmd_escale_pp.root");
  
}
