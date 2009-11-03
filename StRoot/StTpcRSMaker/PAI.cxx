// $Id: PAI.cxx,v 1.3 2009/11/03 22:38:53 fisyak Exp $
// This is wrapper for Pavel Nevski version of Photo Aborption Model 
// which can be used an alternative to Bichsel model which  we have  only for P10 mixture.
#include "Stiostream.h"
#include <assert.h>
#include "PAI.h"
#include "StarCallf77.h" 
#include "TMath.h"
#include "TRandom.h"
#include "TError.h"
#include "TH2.h"
#include "TString.h"
//#include "TGraph2D.h"
#include "TGraph.h"
#include "StMessMgr.h" 
#define    xgasini	 F77_NAME(xgasini,XGASINI)
#define    xgastab	 F77_NAME(xgastab,XGASTAB)
#define    xfintera 	 F77_NAME(xfintera,XFINTERA)
PAI* PAI::fgInstance = 0;
//#define DEBUG
//____________________________________________________________________________ 
extern "C" 
{
  void type_of_call xgasini(Int_t &lun);
  void type_of_call xgastab(Float_t &g, Float_t &mdNdX, Int_t &mNoInTable, Float_t *mEnergy, Float_t *mdNdE);
  Float_t type_of_call xfintera(Float_t &x, Float_t *A, Float_t *F, Int_t &N);
}
//____________________________________________________________________________ 
Float_t type_of_call xfintera(Float_t &x, Float_t *A, Float_t *F, Int_t &N) {
  Int_t bin = TMath::BinarySearch(N,A,x);
  if (bin < 0) bin = 0;
  if (bin > N - 3) bin = N-3;
  Double_t xlow = A[bin];
  Double_t xup  = A[bin+1];
  Double_t dx   = xup - xlow;
  if (dx < 1.e-5) {
    LOG_WARN << "bin\t" << bin << "\tN\t" << N << "\tx\t" << x << "\txlow\t" << xlow << "\txup\t" << xup << endm;
  }
  Double_t ylow = F[bin];
  Double_t yup  = F[bin+1];
  Float_t y    = ((xup*ylow-xlow*yup) + x*(yup-ylow))/dx;
#ifdef DEBUG
  if (y > 20) {
    LOG_DEBUG << "bin\t" << bin << "\tN\t" << N << "\tx\t" << x 
	      << "\txlow\t" << xlow << "\txup\t" << xup << "\ty\t" << y << endm;
  }
#endif
  return y; 
} 
//____________________________________________________________________________ 
PAI *PAI::Instance(Int_t NoBetaGammas, Int_t NoEntries, Double_t BetaGammaLog10Min, Double_t BetaGammaLog10Max) {
  if (fgInstance) return fgInstance;
  fgInstance = new PAI(NoBetaGammas, NoEntries, BetaGammaLog10Min, BetaGammaLog10Max); 
  return fgInstance;
}
//____________________________________________________________________________ 
PAI::PAI(Int_t NoBetaGammas, Int_t NoEntries, Double_t BetaGammaLog10Min, Double_t BetaGammaLog10Max) : 
  mNoBetaGammas(NoBetaGammas), mNoEntries(NoEntries), mNoInTable(0), 
  mBetaGammaLog10Min(BetaGammaLog10Min), mBetaGammaLog10Max(BetaGammaLog10Max), 
  mDeltaBetaGammaLog10(0),mEnergy(0), mdNdE(0), mdNdX(0), mGraphs(0) {
  Int_t lun = 6;
  xGasIni(lun);
  mEnergy = new Float_t [mNoEntries];
  mdNdE   = new Float_t [mNoBetaGammas*mNoEntries];
  mdNdX   = new Float_t [mNoBetaGammas];
  mDeltaBetaGammaLog10 = ( mBetaGammaLog10Max - mBetaGammaLog10Min )/ (mNoBetaGammas - 1);
  for (Int_t jg = 0; jg < mNoBetaGammas; jg++) {
    Float_t BetaGammaLog10 = mBetaGammaLog10Min + mDeltaBetaGammaLog10 * jg;
    Float_t BetaGamma  = TMath::Power(10.,BetaGammaLog10);
    Float_t gamma = TMath::Sqrt(BetaGamma*BetaGamma + 1);
    xGasTab(gamma,mdNdX[jg],mNoInTable,&mEnergy[0],&mdNdE[jg*mNoEntries]);
#ifdef DEBUG
    if (! mGraphs) {mGraphs = new TGraph*[ mNoBetaGammas ]; memset (mGraphs, 0, mNoBetaGammas*sizeof(TGraph *));}
    if (mGraphs[jg]) delete mGraphs[jg];
    mGraphs[jg] = new TGraph(mNoInTable);
    Int_t N = 0;
    LOG_DEBUG << "jg\t" << jg << "\tBetaGammaLog10\t" << BetaGammaLog10 
	 << "\tBetaGamma\t" << BetaGamma << "\tgamma\t" << gamma 
	 << "\tmdNdX\t" << mdNdX[jg] 
	 << "\tmNoInTable\t" << mNoInTable << endm;
    for (int i = 0; i < mNoInTable; i++) {
      LOG_DEBUG << "\t" << i << "\tmEnergy\t" << mEnergy[i] << "\tmdNdE\t" << mdNdE[jg*mNoEntries +i] << endm;
    }
    for (int i = 0; i < mNoInTable; i++) {
      //      Double_t x = BetaGammaLog10;
      Double_t y = TMath::Exp(mEnergy[i]);
      Double_t z = mdNdE[jg*mNoEntries+i]/y;
      mGraphs[jg]->SetPoint(N++,y,z);
    }
#endif
  }
}
//____________________________________________________________________________ 
PAI::~PAI() {
  fgInstance = 0;
  delete [] mEnergy;
  delete [] mdNdE;
  delete [] mdNdX;
#ifdef DEBUG
  for (Int_t i = 0; i < mNoBetaGammas; i++) SafeDelete(mGraphs[i]);
  delete [] mGraphs;
#endif
}
//____________________________________________________________________________ 
void PAI::xGasIni(Int_t &lun) {xgasini(lun);}
//____________________________________________________________________________ 
void PAI::xGasTab(Float_t &g, Float_t &mdNdX, Int_t &mNoInTable, Float_t *mEnergy, Float_t *mdNdE) { 
  xgastab(g,mdNdX, mNoInTable, mEnergy, mdNdE);
}
//____________________________________________________________________________ 
Float_t PAI::xFintera(Float_t &x, Float_t *A, Float_t *F, Int_t &N) {
  return xfintera(x, A, F, N); 
}
//____________________________________________________________________________ 
void PAI::xNext(Float_t BetaGamma, Float_t &dX, Float_t &dE) {

  Float_t BetaGammaL = TMath::Log10(BetaGamma);
  Int_t jg = (Int_t) ((BetaGammaL - mBetaGammaLog10Min)/mDeltaBetaGammaLog10);
  if (jg < 0) jg = 0;
  if (jg >= mNoBetaGammas) jg = mNoBetaGammas-1;
  dX = - TMath::Log(gRandom->Rndm())/mdNdX[jg];
  Float_t random = gRandom->Rndm()*mdNdX[jg];
  dE = TMath::Exp(xFintera(random,&mdNdE[mNoEntries*jg],mEnergy,mNoInTable));
#ifdef DEBUG
  LOG_DEBUG << "BetaGamma\t" << BetaGamma << "\trandom\t" << random << "\tdX\t" << dX << "\tdE\t" << dE << endm;
#endif  
} 
//________________________________________________________________________________
void PAI::xGenerate(Int_t NsSteps, Int_t Nevents) {
  Double_t *step = new Double_t [NsSteps];
  for (int i = 0; i < NsSteps; i++) step[i] = TMath::Power(2.,(i - 2.)/3.);
  for (int jg = 0; jg < mNoBetaGammas; jg++) {
    Double_t betaGammaLog10 = mBetaGammaLog10Min + mDeltaBetaGammaLog10*jg;
    Double_t betaGamma = TMath::Power(10., betaGammaLog10);
    TH1D **hist = new TH1D * [NsSteps];
    for (int i = 0; i < NsSteps; i++) {
      hist[i] = new TH1D(Form("dNdE_g%i_s%i",jg,i),
			 Form("log10(dE/dx (keV/cm)) distribution for beta*gamma=%f and dX=%f",
			      betaGamma,step[i]),500, -0.5, 4.5);
      LOG_INFO << "Generate Histograms\t" << hist[i]->GetName() << "\t" << hist[i]->GetTitle() << endm;
    }
    Float_t  dX, dE;
    for (int iev = 0; iev < Nevents; iev++) {
      Double_t x = 0;
      Double_t E = 0;
      Int_t js = 0;
      while (js < NsSteps) {
	xNext(betaGamma, dX, dE); 
	//	if (dE > 500.) continue;
	x += dX;
	E += dE;
	for (int j = js; j < NsSteps; j++) {
	  if (x > step[j]) {
	    js++;
	    Double_t dEdx = 1.e-3*E/step[j];
	    if (dEdx > 0) hist[j]->Fill(TMath::Log10(dEdx));
	  }
	}
      }
    }
  }
}
// $Log: PAI.cxx,v $
// Revision 1.3  2009/11/03 22:38:53  fisyak
// Freeze version rcf9108.J
//
// Revision 1.2  2009/10/30 21:12:00  fisyak
// Freeze version rcf9108.F, Clean up
//
