/***************************************************************************
 *
 * $Id: TH1Helper.cxx,v 1.3 2016/01/22 17:45:19 smirnovd Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "TH1Helper.h"
#include "TMath.h"

//ClassImp(TH1Helper)
//______________________________________________________________________________
TH1Helper::TH1Helper(const TH1 *h, int binMin, int binMax)
{
  fNonZeros=0;
  if(h==0) return;
  Set(h,binMin,binMax);
}
//______________________________________________________________________________
TH1Helper::TH1Helper(const TH1 *h, double xMin, double xMax)
{
   Set(h,xMin,xMax);
}
//______________________________________________________________________________
void TH1Helper::Set(const TH1 *h, double xMin, double xMax)
{
   fH1 = h;
   fBMin = 0;
   fBMax = 0;
   fXMin = xMin;
   fXMax = xMax;
   Build();	
}
//______________________________________________________________________________
void TH1Helper::Set(const TH1 *h, int binMin, int binMax)
{
   fH1 = h;
   fBMin = binMin;
   fBMax = binMax;
   fXMin = 0;
   fXMax = 0;
   Build();	
}
//______________________________________________________________________________
TH1Helper::~TH1Helper()
{
}
//______________________________________________________________________________
void TH1Helper::Build()
{
  const TAxis *ax = fH1->GetXaxis();
  int nbins = ax->GetNbins();
  if (fBMax) { //1st constructor
    if (fBMin < 1) fBMin = 1;
    if (fBMax > nbins) fBMax = nbins;
    fXMin = ax->GetBinLowEdge(fBMin);
    fXMax = ax->GetBinUpEdge (fBMax);
  } else     { //2nd constructor

    fBMin = ax->FindFixBin(fXMin);
    if (fBMin < 1) fBMin = 1;
    fBMax = ax->FindFixBin(fXMax);
    if (fBMax > nbins) fBMax = nbins;
    if (fXMin < ax->GetBinLowEdge(fBMin)) fXMin = ax->GetBinLowEdge(fBMin);
    if (fXMax > ax->GetBinUpEdge (fBMax)) fXMax = ax->GetBinUpEdge (fBMax);
  }
  memset(fMom,0,sizeof(fMom));
  fNonZeros = 0;
  for (int i=fBMin; i<=fBMax; i++) if(fH1->GetBinContent(i)) fNonZeros++; 

  fMom[0] = 9.e-99;

}   

//______________________________________________________________________________
void TH1Helper::Aver()
{
  if (!fNonZeros) return;
  if (fMom[2])    return;
  const TAxis *ax = fH1->GetXaxis();
  double ovl[2][6];
  double h,error,content,part,center,low,upp;

//   first bin overlap
  error = fH1->GetBinError  (fBMin); 
  content = fH1->GetBinContent(fBMin);   
  h   = ax ->GetBinWidth  (fBMin);
  low = fXMin;
  upp = ax->GetBinUpEdge(fBMin);
  if (upp>fXMax) upp = fXMax;
  ovl[0][0] = 0.5*(low + upp); 				//partial Center
  ovl[0][1] = (upp-low);        			//partial width
  part = ovl[0][1]/h;
  ovl[0][2] = content*part; 				//partial content
  ovl[0][3] = error*TMath::Sqrt(part);                         //partial error
  ovl[0][4] = low;                                    	//low edge
  ovl[0][5] = upp;                  			//upp edge


//   last bin overlap
  error = fH1->GetBinError  (fBMax); 
  content = fH1->GetBinContent(fBMax);   
  h   = ax ->GetBinWidth  (fBMax);
  low = ax->GetBinLowEdge (fBMax);
  if (low<ovl[0][5]) low = ovl[0][5];
  upp = fXMax;
  part = (upp-low)/h; 
  ovl[1][0] = 0.5*(low+upp); 				//partial Center
  ovl[1][1] = (upp-low);      				//partial width
  ovl[1][2] = content*part; 				//partial content
  ovl[1][3] = error*TMath::Sqrt(part);                         //partial error
  ovl[1][4] = low;                 //low edge
  ovl[1][5] = upp;                 			//upp edge
  double wtot=0,wt,fun;
  for (int iter=0;iter <=2;iter++) {
    for (int ibin = fBMin; ibin <= fBMax; ibin++) {
      int jk = -1;
      if (ibin == fBMin) jk=0;
      if (ibin == fBMax) jk=1;

      if (jk>=0) {
	center  = ovl[jk][0];
	h       = ovl[jk][1];
	content = ovl[jk][2];
	error   = ovl[jk][3];
	low     = ovl[jk][4];
	upp     = ovl[jk][5];
      } else {
	center = ax->GetBinCenter(ibin);
	h      = ax ->GetBinWidth(ibin);
	content = fH1->GetBinContent(ibin);   
	error   = fH1->GetBinError  (ibin); 
	low     = ax->GetBinLowEdge (ibin);
	upp     = ax->GetBinUpEdge  (ibin);

      }
      if (!content) continue;
      wt = content;
      if ( iter==0 ) {
	wtot +=wt;
	fMom[0] += content;
	fun  = center;
	fMom[1] += fun*content;
	continue;
      }
      if ( iter==1 ) {
	fun  = h*h/12 + TMath::Power(center-fMom[1],2);
	fMom[2] += fun*content;
	continue;
      } 
      if ( iter==2 ) {
	fun = 0;
	for (int i=0;i<3;i++){fun += TMath::Power(TMath::Power(low-fMom[1]+i*h/2,2)-fMom[2],2);}
	fun /=3;
	fMom[4] += fun*content;
	continue;
      }

    }// end bin loop
    if (!fNonZeros) fMom[0] = 9.e-99;
    fMom[(1<<iter)] /=fMom[0];
  }//end iter loop

}
double TH1Helper::GetMean    () {Aver();return fMom[1];              }
double TH1Helper::GetMeanErr () {Aver();return TMath::Sqrt(fMom[2]/fMom[0]);}
double TH1Helper::GetRMS     () {Aver();return TMath::Sqrt(fMom[2]);        }
double TH1Helper::GetRMSErr  () {Aver();return TMath::Sqrt(fMom[4]/fMom[0]);}
int    TH1Helper::GetNonZeros() const { return fNonZeros;            }
double TH1Helper::GetIntegral() {Aver();return fMom[0];              }
double TH1Helper::GetIntegErr() {Aver();return TMath::Sqrt(fMom[0]);        }
