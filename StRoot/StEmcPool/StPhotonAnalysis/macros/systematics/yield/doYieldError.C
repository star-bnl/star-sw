void divideGraphWithFunction(TGraphErrors *graph,TF1 *func)
{
  for(int i=0;i<graph->GetN();i++){
    double x=0.;
    double y=0.;
    graph->GetPoint(i,x,y);
    double ey=graph->GetErrorY(i);
    graph->SetPoint(i,x,y/func->Eval(x));
    graph->SetPointError(i,0.,ey/func->Eval(x));
  }
}
void divideGraphs(TGraphErrors *g1,TGraphErrors *g2)
{
  for(int i=0;i<g1->GetN();i++){
    double x=0.;
    double y=1.;
    double ex=0.;
    double ey=0;
    double y2=1.;
    double ey2=0.;
    g1->GetPoint(i,x,y);
    ey=g1->GetErrorY(i);
    g2->GetPoint(i,x,y2);
    ey2=0.;//take error into account once..

    double xn=x;
    double exn=0.;
    double yn=y/y2;
    double eyn=(ey*ey/(y*y) + ey2*ey2/(y2*y2));
    eyn=sqrt(eyn)*yn;
    yn+=-1.;
    g1->SetPoint(i,xn,yn);
    g1->SetPointError(i,exn,eyn);
  }

}
void doYieldError(char *nom,char *left,char *right,char *mid)
{
  TF1 *err_mb_hi=new TF1("err_mb_hi","0.035",1.,5.);
  TF1 *err_mb_lo=new TF1("err_mb_hi","-0.035",1.,5.);
  TF1 *err_ht_hi=new TF1("err_mb_hi","0.05",4.,15.);
  TF1 *err_ht_lo=new TF1("err_mb_hi","-0.05",4.,15.);

  bool isPP05=false;
  TPad *p_c;
  int i_mb=0;
  int i_ht1=1;
  int i_ht2=2;

  TFile *f_nom=new TFile(nom,"OPEN");
  TCanvas *c_nom=f_nom->Get("compare");
  c_nom->Draw();
  if(isPP05){
    i_mb=3;
    i_ht1=4;
    i_ht2=5;
    p_c=(TPad*)gPad->FindObject("padt");
    p_c->cd();
  }
  TMultiGraph *m_nom=gPad->FindObject("m_pions")->Clone();
  TGraphErrors *mb_nom=m_nom->GetListOfGraphs()->At(i_mb)->Clone();
  mb_nom->SetName("mb_nom");
  TGraphErrors *ht1_nom=m_nom->GetListOfGraphs()->At(i_ht1)->Clone();
  ht1_nom->SetName("ht1_nom");
  TGraphErrors *ht2_nom=m_nom->GetListOfGraphs()->At(i_ht2)->Clone();
  ht2_nom->SetName("ht2_nom");
  delete c_nom;
  delete m_nom;  
  

  TFile *f_left=new TFile(left,"OPEN");
  TCanvas *c_left=f_left->Get("compare");
  c_left->Draw();
  if(isPP05){
    p_c=(TPad*)gPad->FindObject("padt");
    p_c->cd();
  }
  TMultiGraph *m_left=gPad->FindObject("m_pions")->Clone();
  TGraphErrors *mb_left=m_left->GetListOfGraphs()->At(i_mb)->Clone();
  mb_left->SetName("mb_left");
  TGraphErrors *ht1_left=m_left->GetListOfGraphs()->At(i_ht1)->Clone();
  ht1_left->SetName("ht1_left");
  TGraphErrors *ht2_left=m_left->GetListOfGraphs()->At(i_ht2)->Clone();
  ht2_left->SetName("ht2_left");
  delete c_left;
  delete m_left;
  

  TFile *f_right=new TFile(right,"OPEN");
  TCanvas *c_right=f_right->Get("compare");
  c_right->Draw();
  if(isPP05){
    p_c=(TPad*)gPad->FindObject("padt");
    p_c->cd();
  }
  TMultiGraph *m_right=gPad->FindObject("m_pions")->Clone();
  TGraphErrors *mb_right=m_right->GetListOfGraphs()->At(i_mb)->Clone();
  mb_right->SetName("mb_right");
  TGraphErrors *ht1_right=m_right->GetListOfGraphs()->At(i_ht1)->Clone();
  ht1_right->SetName("ht1_right");
  TGraphErrors *ht2_right=m_right->GetListOfGraphs()->At(i_ht2)->Clone();
  ht2_right->SetName("ht2_right");
  delete c_right;
  delete m_right;
  

  TFile *f_mid=new TFile(mid,"OPEN");
  TCanvas *c_mid=f_mid->Get("compare");
  c_mid->Draw();
  if(isPP05){
    p_c=(TPad*)gPad->FindObject("padt");
    p_c->cd();
  }
  TMultiGraph *m_mid=gPad->FindObject("m_pions")->Clone();
  TGraphErrors *mb_mid=m_mid->GetListOfGraphs()->At(i_mb)->Clone();
  mb_mid->SetName("mb_mid");
  TGraphErrors *ht1_mid=m_mid->GetListOfGraphs()->At(i_ht1)->Clone();
  ht1_mid->SetName("ht1_mid");
  TGraphErrors *ht2_mid=m_mid->GetListOfGraphs()->At(i_ht2)->Clone();
  ht2_mid->SetName("ht2_mid");
  delete c_mid;
  delete m_mid;
  f_mid->Close();


  divideGraphs(mb_left,mb_nom);
  divideGraphs(ht1_left,ht1_nom);
  divideGraphs(ht2_left,ht2_nom);

  divideGraphs(mb_right,mb_nom);
  divideGraphs(ht1_right,ht1_nom);
  divideGraphs(ht2_right,ht2_nom);

  divideGraphs(mb_mid,mb_nom);
  divideGraphs(ht1_mid,ht1_nom);
  divideGraphs(ht2_mid,ht2_nom);

  divideGraphs(mb_nom,mb_nom);
  divideGraphs(ht1_nom,ht1_nom);
  divideGraphs(ht2_nom,ht2_nom);


  TCanvas *c=new TCanvas("c","c",400,300);
  TMultiGraph *m=new TMultiGraph();
  
  //m->Add(mb_nom);
  //m->Add(ht1_nom);
  //m->Add(ht2_nom);
  m->Add(mb_left);
  m->Add(ht1_left);
  m->Add(ht2_left);
  m->Add(mb_right);
  m->Add(ht1_right);
  m->Add(ht2_right);
  m->Add(mb_mid);
  m->Add(ht1_mid);
  m->Add(ht2_mid);

  m->SetMinimum(-0.5);
  m->SetMaximum(0.5);


  TLegend *leg=new TLegend(0.5,0.5,0.7,0.7);
  //leg->AddEntry(mb_nom,"nominal","p");
  leg->AddEntry(mb_left,"{-3#sigma,2#sigma}","p");
  leg->AddEntry(mb_right,"{-2#sigma,3#sigma}","p");
  leg->AddEntry(mb_mid,"{-2#sigma,2#sigma}","p");
 
  m->Draw("ape");
  err_mb_hi->Draw("same");
  err_mb_lo->Draw("same");
  err_ht_hi->Draw("same");
  err_ht_lo->Draw("same");

  leg->Draw();

  if(isPP05){
    c->SaveAs("error_yield_pp.eps");
    c->SaveAs("error_yield_pp.root");
  }
  else{
    c->SaveAs("error_yield_dau.eps");
    c->SaveAs("error_yield_dau.root");
  }
}
