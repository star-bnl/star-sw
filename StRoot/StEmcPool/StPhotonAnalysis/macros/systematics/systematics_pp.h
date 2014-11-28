//p+p pion cross section:
double bemc_escale_pp_pion(double *x,double * /*par*/){
  double p[]={2.14037e-01,6.55108e-03};
  return p[0]+p[1]*x[0];
}
double bemc_spread_pp_pion(double * /*x*/,double * /*par*/){
  return (double)0.024;
}
double bsmd_escale_pp_pion(double *x,double * /*par*/){
  double p[]={4.47017e-02,9.96379e-01,-7.68552e-01};
  return p[0]+p[1]*exp(p[2]*x[0]);
}
double bsmd_spread_pp_pion(double *x,double * /*par*/){
  double p[]={1.26779e-02,-1.55283e-01,-1.68008e-01};
  return p[0]+p[1]*exp(p[2]*x[0]);
}
double material_pp_pion(double * /*x*/,double * /*par*/){
  return (double)0.018;
}
double yield_extract_pp_pion(double *x,double * /*par*/){ 
  if(x[0]<4.) return (double)0.05;
  /*if(x[0]>=4.) */return (double)0.071;
}
double error_xsec_pp_pion(double * /*x*/,double * /*par*/){
  return (double)sqrt(0.069*0.069+0.092*0.092);
}
double prescale_pp_pion(double *x,double * /*par*/){
  if(x[0]<4.) return (double)0.;
  /*if(x[0]>=4.) */return (double)0.05;
}
double total_sys_pp_pion(double *x,double *par){
  double ret=0.;
  ret=bemc_escale_pp_pion(x,par)*bemc_escale_pp_pion(x,par);
  ret+=bemc_spread_pp_pion(x,par)*bemc_spread_pp_pion(x,par);
  ret+=bsmd_escale_pp_pion(x,par)*bsmd_escale_pp_pion(x,par);
  ret+=bsmd_spread_pp_pion(x,par)*bsmd_spread_pp_pion(x,par);
  ret+=material_pp_pion(x,par)*material_pp_pion(x,par);
  ret+=yield_extract_pp_pion(x,par)*yield_extract_pp_pion(x,par);
  ret+=error_xsec_pp_pion(x,par)*error_xsec_pp_pion(x,par);
  ret+=prescale_pp_pion(x,par)*prescale_pp_pion(x,par);
  return par[0]*sqrt(ret);
}
void set_sys_pp_pion(TGraphErrors *g){
  TF1 *sys=new TF1("sys",&total_sys_pp_pion,1.,15.,1);
  sys->SetParameter(0,1.);
  for(int i=0;i<g->GetN();i++){
    double x=0.;
    double y=0.;
    g->GetPoint(i,x,y);
    double rel_err=sys->Eval(x);
    g->SetPointError(i,0.,rel_err*y);
  }
}
void plotErrors_pp_pion(){
  
  TF1 *error_up=new TF1("error_up",&total_sys_pp_pion,1.,15.,1);
  error_up->SetParameter(0,1.);
  TF1 *error_down=new TF1("error_down",&total_sys_pp_pion,1.,15.,1);
  error_down->SetParameter(0,-1.);

  error_up->SetFillColor(5);
  error_down->SetFillColor(5);


  TCanvas *test=new TCanvas();
  error_up->Draw();
  error_up->SetMinimum(-1.*error_up->GetMaximum());
  error_down->Draw("same");
  test->SaveAs("systotal_pp_pions.eps");
  test->SaveAs("systotal_pp_pions.root");
}

//p+p double ratio:
double bemc_escale_pp_ratio(double * /*x*/,double * /*par*/){
  return 0.03;
}
double bemc_spread_pp_ratio(double * /*x*/,double * /*par*/){
  return 0.01;
}
double bsmd_escale_pp_ratio(double * /*x*/,double * /*par*/){
  //return 0.06;
  return 0.12;
}
double bsmd_spread_pp_ratio(double * /*x*/,double * /*par*/){
  return 0.01;
}
double yield_extract_pp_ratio(double *x,double *par){
  return yield_extract_pp_pion(x,par);
}
double eta_over_pi_pp(double * /*x*/,double * /*par*/){
  return 0.02;
}
double fit_pion_pp(double * /*x*/,double * /*par*/){
  return 0.015;
}
double total_sys_pp_ratio(double *x,double *par){
  double ret=0.;
  ret=bemc_escale_pp_ratio(x,par)*bemc_escale_pp_ratio(x,par);
  ret+=bemc_spread_pp_ratio(x,par)*bemc_spread_pp_ratio(x,par);
  ret+=bsmd_escale_pp_ratio(x,par)*bsmd_escale_pp_ratio(x,par);
  ret+=bsmd_spread_pp_ratio(x,par)*bsmd_spread_pp_ratio(x,par);
  ret+=yield_extract_pp_ratio(x,par)*yield_extract_pp_ratio(x,par);
  ret+=eta_over_pi_pp(x,par)*eta_over_pi_pp(x,par);
  ret+=fit_pion_pp(x,par)*fit_pion_pp(x,par);
  return par[0]*sqrt(ret);
}
void set_sys_pp_ratio(TGraphErrors *g){
  TF1 *sys=new TF1("sys",&total_sys_pp_ratio,1.,15.,1);
  sys->SetParameter(0,1.);
  for(int i=0;i<g->GetN();i++){
    double x=0.;
    double y=0.;
    g->GetPoint(i,x,y);
    double rel_err=sys->Eval(x);
    g->SetPointError(i,0.2,rel_err*y);
//cout << "x=" << x << ", y=" << y << ", ey=" << (rel_err*y) << endl;
  }
}

