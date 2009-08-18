//d+Au yield:
double bemc_escale_dau_pion(double *x,double * /*par*/){
  double p[]={2.14037e-01,6.55108e-03};
  return 1.25*(p[0]+p[1]*x[0]);
}
double bemc_spread_dau_pion(double * /*x*/,double * /*par*/){
  return (double)0.035;
}
double bsmd_escale_dau_pion(double *x,double * /*par*/){
  double p[]={4.47017e-02,9.96379e-01,-7.68552e-01};
  return p[0]+p[1]*exp(p[2]*x[0]);
}
double bsmd_spread_dau_pion(double *x,double * /*par*/){
  double p[]={1.26779e-02,-1.55283e-01,-1.68008e-01};
  return p[0]+p[1]*exp(p[2]*x[0]);
}
double material_dau_pion(double * /*x*/,double * /*par*/){
  return (double)0.026;
}
double yield_extract_dau_pion(double *x,double * /*par*/){ 
  if(x[0]<4.) return (double)0.05;
  /*if(x[0]>=4.)*/ return (double)0.071;
}
double beambg_dau_pion(double *x,double * /*par*/){
  if(x[0]<5.) return (double)0.0;
  /*if(x[0]>=5.)*/ return (double)0.007*(x[0]-5.);
}
double error_norm_dau_pion(double * /*x*/,double * /*par*/){
  return (double)0.01;
}
double prescale_dau_pion(double *x,double * /*par*/){
  if(x[0]<4.) return (double)0.;
  /*if(x[0]>=4.)*/ return (double)0.05;
}
double total_sys_dau_pion(double *x,double *par){
  double ret=0.;
  ret=bemc_escale_dau_pion(x,par)*bemc_escale_dau_pion(x,par);
  ret+=bemc_spread_dau_pion(x,par)*bemc_spread_dau_pion(x,par);
  ret+=bsmd_escale_dau_pion(x,par)*bsmd_escale_dau_pion(x,par);
  ret+=bsmd_spread_dau_pion(x,par)*bsmd_spread_dau_pion(x,par);
  ret+=material_dau_pion(x,par)*material_dau_pion(x,par);
  ret+=yield_extract_dau_pion(x,par)*yield_extract_dau_pion(x,par);
  ret+=beambg_dau_pion(x,par)*beambg_dau_pion(x,par);
  ret+=error_norm_dau_pion(x,par)*error_norm_dau_pion(x,par);
  ret+=prescale_dau_pion(x,par)*prescale_dau_pion(x,par);
  return par[0]*sqrt(ret);
}
void set_sys_dau_pion(TGraphErrors *g){
  TF1 *sys=new TF1("sys",&total_sys_dau_pion,1.,15.,1);
  sys->SetParameter(0,1.);
  for(int i=0;i<g->GetN();i++){
    double x=0.;
    double y=0.;
    g->GetPoint(i,x,y);
    double rel_err=sys->Eval(x);
    g->SetPointError(i,0.,rel_err*y);
  }
}
void plotErrors_dau_pion(){
  
  TF1 *error_up=new TF1("error_up",&total_sys_dau_pion,1.,15.,1);
  error_up->SetParameter(0,1.);
  TF1 *error_down=new TF1("error_down",&total_sys_dau_pion,1.,15.,1);
  error_down->SetParameter(0,-1.);

  error_up->SetFillColor(5);
  error_down->SetFillColor(5);


  TCanvas *test=new TCanvas();
  error_up->Draw();
  error_up->SetMinimum(-1.*error_up->GetMaximum());
  error_down->Draw("same");
  test->SaveAs("systotal_dau_pions.eps");
  test->SaveAs("systotal_dau_pions.root");
}

//d+Au double ratio:
double bemc_escale_dau_ratio(double * /*x*/,double * /*par*/){
  return 0.03;
}
double bemc_spread_dau_ratio(double * /*x*/,double * /*par*/){
  return 0.01;
}
double bsmd_escale_dau_ratio(double * /*x*/,double * /*par*/){
  //return 0.06;
  return 0.12;
}
double bsmd_spread_dau_ratio(double * /*x*/,double * /*par*/){
  return 0.01;
}
double yield_extract_dau_ratio(double *x,double *par){
  return yield_extract_dau_pion(x,par);
}
double eta_over_pi_dau(double * /*x*/,double * /*par*/){
  return 0.02;
}
double fit_pion_dau(double * /*x*/,double * /*par*/){
  return 0.015;
}
//double beambg_dau_pion(double *x,double *par){
//  if(x[0]<5.) return (double)0.0;
//  if(x[0]>=5.) return 0.01+0.004*(x[0]-5.);
//}
double total_sys_dau_ratio(double *x,double *par){
  double ret=0.;
  ret=bemc_escale_dau_ratio(x,par)*bemc_escale_dau_ratio(x,par);
  ret+=bemc_spread_dau_ratio(x,par)*bemc_spread_dau_ratio(x,par);
  ret+=bsmd_escale_dau_ratio(x,par)*bsmd_escale_dau_ratio(x,par);
  ret+=bsmd_spread_dau_ratio(x,par)*bsmd_spread_dau_ratio(x,par);
  ret+=yield_extract_dau_ratio(x,par)*yield_extract_dau_ratio(x,par);
  ret+=eta_over_pi_dau(x,par)*eta_over_pi_dau(x,par);
  ret+=fit_pion_dau(x,par)*fit_pion_dau(x,par);
  ret+=beambg_dau_pion(x,par)*beambg_dau_pion(x,par);
  return par[0]*sqrt(ret);
}
void set_sys_dau_ratio(TGraphErrors *g){
  TF1 *sys=new TF1("sys",&total_sys_dau_ratio,1.,15.,1);
  sys->SetParameter(0,1.);
  for(int i=0;i<g->GetN();i++){
    double x=0.;
    double y=0.;
    g->GetPoint(i,x,y);
    double rel_err=sys->Eval(x);
    g->SetPointError(i,0.2,rel_err*y);
  }
}

