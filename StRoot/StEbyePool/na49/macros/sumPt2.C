///////////////////////////////////////////////////////////////////////////////
//
// $Id: sumPt2.C,v 1.2 2001/11/06 18:02:53 posk Exp $
//
// Author:       Glenn Cooper and Art Poskanzer, April 00 and Nov 01
// Description:  Calculates sum of pt square of all the particles
//               Uses dNdydPt.C
//               Ads 3/2 sum of charged pions +
//               2 sum of charged kaons +
//               fraction of protons which are participants
//
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: sumPt2.C,v $
// Revision 1.2  2001/11/06 18:02:53  posk
// 40 GeV compatability.
//
//
///////////////////////////////////////////////////////////////////////////////

Double_t NPart[6] = { 366, 309, 242, 178, 132, 85 };
FILE *f = fopen("sumPt2.txt","w");

void sumPt2() {
  gROOT->LoadMacro("dNdydPt.C");
  //SetParameters(158);
  SetParameters(40);
  for (int n = 0; n < 6; n++) {
    Int_t cent = n + 1;
    TF1* fdndy = (TF1*)gROOT->FindObject("fdndy");
    if(!fdndy) fdndy = new TF1("fdndy",(void*)fdNdy,-2.8,2.8,2);
    TF1* fsumPt2 = (TF1*)gROOT->FindObject("fsumPt2");
    if(!fsumPt2) fsumPt2 = new TF1("fsumPt2",(void*)fSumPt2,-2.8,2.8,2);
    fdndy->SetParameter(1,cent);
    fsumPt2->SetParameter(1,cent);
    printf("Sum of pt^2 for Cent %d:\n", cent);
    fprintf(f,"Sum of pt^2 for Cent %d:\n", cent);
    Double_t N[6];
    Double_t SumPt2[6];
    for(int ipid=0; ipid<6; ipid++) {
      fdndy->SetParameter(0,ipid);
      fsumPt2->SetParameter(0,ipid);
      N[ipid] = fdndy->Integral(-YMax[ipid],YMax[ipid]);
      SumPt2[ipid] = fsumPt2->Integral(-YMax[ipid],YMax[ipid]);
      printf("  %4s:  %.2f\n", PidName[ipid], SumPt2[ipid]);
      fprintf(f,"  %4s:  %.2f\n", PidName[ipid], SumPt2[ipid]);
    }
    Double_t all = 3.0*(SumPt2[kPionPlus]+SumPt2[kPionMinus])/2 + 
	   2.0*(SumPt2[kKaonPlus]+SumPt2[kKaonMinus]) +
	   NPart[cent-1]*SumPt2[kProton]/N[kProton];
    printf("   All:  %.2f\n", all);
    fprintf(f,"   All:  %.2f\n", all);
    printf("\n");    
    fprintf(f,"\n");
  }
}

Double_t fdNdy(Double_t *x, Double_t *par) {
  // x[0] - y
  // par[0] - ipid
  // par[1] - cent
  return dNdy(x[0], par[0]+0.5, par[1]+0.5);
}

Double_t MeanPt2(Double_t y, Int_t ipid, Int_t cent) {
  double m = Mass[ipid];
  double t1 = T1(y, ipid, cent);
  double t2 = T2(y, ipid, cent);
  double xi2 = Xi2(y, ipid, cent);
  return (1-xi2)*MeanPt2Term(m,t1) + xi2*MeanPt2Term(m,t2);
}

Double_t MeanPt2Term(Double_t mass, Double_t T) {
  double tm = T/mass;
  return 2*mass*T*(1+3*tm+3*tm*tm)/(1+tm);
}

Double_t SumPt2(Double_t y, Int_t ipid, Int_t cent) {
  return dNdy(y,ipid,cent)*MeanPt2(y,ipid,cent);
}

Double_t fSumPt2(Double_t *x, Double_t *par) {
  // x[0] - y
  // par[0] - ipid
  // par[1] - cent
  return SumPt2(x[0], par[0]+0.5, par[1]+0.5);

}

