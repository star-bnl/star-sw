#include "StPDFs.h"
#include "TString.h"
#include "TMath.h"
#include "mstwpdf.h"

void StPDFs::init_polPDF_DSSV2009a(char *file)
{
  //  dssvini2009a_(iset);
  dssvinig_(file);
}
//DSSV
double StPDFs::get_polPDF_NLO_DSSV2009a(int flavor, double x, double Q2){

  double duv=0,ddv=0,dubar=0,ddbar=0,dstr=0,dglu=0;

  if (x>=1.0e-5&&x<=1.0&&Q2>=1.0&&Q2<=1.0e5) {
    //    dssvfit2009a_(&x,&Q2,&duv,&ddv,&dubar,&ddbar,&dstr,&dglu);
    dssvgupdate_(&x,&Q2,&duv,&ddv,&dubar,&ddbar,&dstr,&dglu);
    switch (flavor) {
    case  1: return (ddv+ddbar)/x; //dv + dsea quark
    case  2: return (duv+dubar)/x; //uv + usea quark
    case -1: return ddbar/x;  //dbar==dsea quark
    case -2: return dubar/x;  //ubar==usea quark
    case  3:
    case -3: return dstr/x;   //s==sbar quark
    case 21: return dglu/x;   //gluon
    case  4:
    case -4:
    case  5:
    case -5:
    case  6:
    case -6: return dstr/x;
    }
  }
  return 1000;
}
void StPDFs::init_unpolPDF_NLO(char *prefix, int iset)
{
  mstwpdf = new c_mstwpdf(Form("%s.%2.2d.dat", prefix, iset));
}

double StPDFs::get_unpolPDF_NLO(int flavor, double x, double Q2){
/*
  Double_t pdf=0.0;
  Int_t iset=1;//NLO MSbar scheme
  Int_t er=0;
  Int_t fl=10;

  if (flavor==1) fl=2;
  if (flavor==2) fl=1;
  if (flavor==-1) fl=-2;
  if (flavor==-2) fl=-1;
  if (flavor==21) fl=0;
  if (flavor==3) fl=3;
  if (flavor==-3) fl=-3;
  if (flavor==4) fl=4;
  if (flavor==-4) fl=-4;
  if (flavor==5) fl=5;
  if (flavor==-5) fl=-5;

  double Q=TMath::Sqrt(Q2);
  pdf=ctq5pd_(&iset,&fl,&x,&Q,&er);
  if (er!=0) pdf=0.0;

  return pdf;
*/
  double Q = TMath::Sqrt(Q2);
  int fl;
  if(flavor == 21) fl = 0;
  else fl = flavor;
  if(fl >= -6 && fl <= 6){
    return mstwpdf->parton(fl, x, Q);
  }else
    return 1000;
}
