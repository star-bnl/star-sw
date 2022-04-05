//pion rda:
double bemc_escale_rda(double *x,double * /*par*/){
  double p[]={2.14037e-01,6.55108e-03};
  return 0.5*(p[0]+p[1]*x[0]);
}
double bemc_spread_rda(double * /*x*/,double * /*par*/){
  return (double)0.042;
}
double material_rda(double * /*x*/,double * /*par*/){
  return (double)0.032;
}
double yield_extract_rda(double *x,double * /*par*/){ 
  if(x[0]<4.) return (double)0.071;
  /*if(x[0]>=4.)*/ return (double)0.1;
}
double prescale_rda(double *x,double * /*par*/){
  if(x[0]<4.) return (double)0.;
  /*if(x[0]>=4.)*/ return (double)0.071;
}
double beambg_rda(double *x,double * /*par*/){
  if(x[0]<5.) return (double)0.0;
  /*if(x[0]>=5.)*/ return (double)0.007*(x[0]-5.);
}
double total_sys_rda(double *x,double *par){
  double ret=0.;
  ret=bemc_escale_rda(x,par)*bemc_escale_rda(x,par);
  ret+=bemc_spread_rda(x,par)*bemc_spread_rda(x,par);
  ret+=material_rda(x,par)*material_rda(x,par);
  ret+=yield_extract_rda(x,par)*yield_extract_rda(x,par);
  ret+=prescale_rda(x,par)*prescale_rda(x,par);
  ret+=beambg_rda(x,par)*beambg_rda(x,par);
  return par[0]*sqrt(ret);
}
void set_sys_rda(TGraphErrors *g){
  TF1 *sys=new TF1("sys",&total_sys_rda,1.,15.,1);
  sys->SetParameter(0,1.);
  for(int i=0;i<g->GetN();i++){
    double x=0.;
    double y=0.;
    g->GetPoint(i,x,y);
    double rel_err=sys->Eval(x);
    g->SetPointError(i,0.,rel_err*y);
  }
}

//normalization explicit:
double error_norm_rda(double * /*x*/,double * /*par*/){
  return (double)sqrt(0.069*0.069+0.092*0.092+0.01*0.01+0.053*0.053);//s_bbc,e_bbc,e_vert,<nbin>
}

