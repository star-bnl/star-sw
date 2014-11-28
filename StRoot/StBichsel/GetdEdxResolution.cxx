#include "Bichsel.h"
#include "TMath.h"
#include "tpcCorrection.h"
struct Sigma_t {
  tpcCorrection_st params;
  Int_t utime, date, time;
};
/*mysql> select 
  type,nrows,idx,npar,OffSet,min,max,a,beginTime,entryTime  from TpcLengthCorrectionB where idx=2 and deactive=0 order by beginTime; */
static Sigma_t Sigmas[] = {         
  {{0,10,2,4,0, 2.30, 4.79, {1.82545,-1.13077,0.249432,-0.0189606,0,0,0,0,0,0}               }, 994003200,     20010701,  120000},// 0 run II          20040717 221107
  {{0, 6,2,3,0, 2.30, 4.70, {0.621603,-0.228409,0.0236501,0,0,0,0,0,0,0}                     },1001304004,     20010924,       4},// 1 run II AuAu19,  20071105 155039
  {{0,13,2,4,0, 0.00, 0.00, {0.0370811,0.142838,-0.0513699,0.00477384,0,0,0,0,0,0 }          },1041829200,     20030106,       0},// 2 run III         20040717 221152
  {{0, 6,2,3,0, 0.00, 0.00, {0.399909,-0.113657,0.0089704,0,0,0,0,0,0,0}                     },1073192401,     20040104,       1},// 3 run IV          20050214 173752
  {{0, 6,2,3,0, 0.00, 0.00, {0.399909,-0.113657,0.0089704,0,0,0,0,0,0,0}                     },1075957201,     20040205,       1},// 4 run IV b        20050214 173813
  {{0, 6,2,3,0, 0.00, 0.00, {0.399909,-0.113657,0.0089704,0,0,0,0,0,0,0}                     },1076994001,     20040217,       1},// 5 run IV c        20050214 173841
  {{0, 6,2,3,0, 0.00, 0.00, {0.399909,-0.113657,0.0089704,0,0,0,0,0,0,0}                     },1080104401,     20040324,       1},// 6 run VI d        20050214 173903
  {{0, 6,2,5,0, 2.30, 4.80, {1.27303,-1.25341,0.532478,-0.102658,0.00733247,0,0,0,0,0}       },1105498801,     20050111,  220001},// 7 run V           20050513 203842
  {{0, 6,2,4,0, 2.30, 4.70, {-0.314706,0.404681,-0.116752,0.0101701,0,0,0,0,0,0}             },1112508000,     20050403,   10000},// 8 run V b         20050729 201656
  {{0, 6,2,6,0, 2.30, 4.79, {2.7451,-3.64943,1.98889,-0.525204,0.0666543,-0.00324793,0,0,0,0}},1141837080,     20060308,  115800},// 9 run VI          20060820 214522
  {{0, 6,2,6,0, 0.00, 0.00, {2.7451,-3.64943,1.98889,-0.525204,0.0666543,-0.00324793,0,0,0,0}},1144314000,     20060406,   50000},//10 run VI b        20060808 160055
  {{0, 6,2,5,0, 0.00, 0.00, {1.11775,-1.24347,0.586667,-0.121644,0.00915544,0,0,0,0,0}       },1147287961,     20060510,  150601},//11 run VI c        20060808 160310
  {{0, 6,2,4,0, 2.30, 4.70, {-0.0748715,0.243322,-0.0793629,0.00726962,0,0,0,0,0,0 }         },1174449642,     20070321,      42} //12 run VII         20071008 205348
};
static Int_t N = sizeof(Sigmas)/sizeof(Sigma_t);
//________________________________________________________________________________
Double_t Bichsel::GetdEdxResolution(Int_t k, Double_t TrackLengthInTPC) {
  if (TrackLengthInTPC <= 0.0 || k < 0) return -999;
  if (k >= N) {
    Int_t uc = k;
    for (k = N - 1; k > 0; k--) {
      if (uc >= Sigmas[k].utime) break;
    }
  }
  if (k >= N) return -999;
  Double_t X = TMath::Log(TrackLengthInTPC);
  return CalcCorrection(&Sigmas[k].params, X);
}
//________________________________________________________________________________
Double_t Bichsel::GetdEdxResolution(Double_t *x, Double_t *p) {
  Int_t k = (Int_t) p[0];
  return GetdEdxResolution(k,x[0]);
}
//________________________________________________________________________________
Double_t Bichsel::CalcCorrection(const tpcCorrection_st *cor,const Double_t x) {
  Int_t N = TMath::Abs(cor->npar);
  Double_t X = x;
  if (cor->npar  < 0) X = TMath::Exp(x);
  if (N > 0) {
    if (cor->min < cor->max) {
      if (X < cor->min) X = cor->min;
      if (X > cor->max) X = cor->max;
    }
    return SumSeries(X,N,&cor->a[0]);
  }
  else return 0;
}
//________________________________________________________________________________
Double_t Bichsel::SumSeries(const Double_t &X,const Int_t &N,const Double_t *params) {
  Double_t Sum = 0;
  if (N > 0) {
    Sum = params[N-1];
    for (int n = N-2; n>=0; n--) Sum = X*Sum + params[n];
  }
  return Sum;
}
