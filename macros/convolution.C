/////////////////////////////////////////////////////////////////////////
//
// 'ADDITION AND CONVOLUTION' RooFit tutorial macro #208
// 
// One-dimensional numeric convolution
// (require ROOT to be compiled with --enable-fftw3)
// 
// pdf = landau(t) (x) gauss(t)
// 
//
// 07/2008 - Wouter Verkerke 
//
/////////////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "RooGlobalFunc.h"
#else
// Refer to a class implemented in libRooFit to force its loading
// via the autoloader.
class Roo2DKeysPdf;
#endif
#include "RooRealVar.h"
#include "RooDataSet.h"
#include "RooGaussian.h"
#include "RooFFTConvPdf.h"
#include "RooPlot.h"
#include "TCanvas.h"
#include "TAxis.h"
#include "TH1.h"
#include "TF1.h"
#include "TMath.h"
#include "TDirectory.h"
#ifndef __CINT__
#include "RooCFunction1Binding.h" 
#include "RooCFunction3Binding.h"
#endif
#include "RooTFnBinding.h" 
#include "RooDataHist.h"
using namespace RooFit ;
#ifndef ROO_LANDAUZ
#define ROO_LANDAUZ

#include "RooAbsPdf.h"
#include "RooRealProxy.h"
#include "TMath.h"
#include "RooFit.h"
#include "RooRandom.h"
Double_t landauZ(Double_t *x, Double_t *par) {
  Double_t xd = x[0];
  Double_t meand = par[0];
  Double_t sigmad = par[1];
  Double_t mpshift  = -0.22278298; // 1.0844535734 + 0.61417;       // LandauZ maximum location
  // MP shift correction
  Double_t mpc = TMath::Exp(meand) - mpshift * sigmad; 
  Double_t xx = TMath::Exp(xd);
  return xx*TMath::Landau(xx,mpc,sigmad);
}
class RooRealVar;

class RooLandauZ : public RooAbsPdf {
public:
  RooLandauZ() {} ;
  RooLandauZ(const char *name, const char *title, RooAbsReal& _x, RooAbsReal& _mean, RooAbsReal& _sigma);
  RooLandauZ(const RooLandauZ& other, const char* name=0);
  virtual TObject* clone(const char* newname) const { return new RooLandauZ(*this,newname); }
  inline virtual ~RooLandauZ() { }

  Int_t getGenerator(const RooArgSet& directVars, RooArgSet &generateVars, Bool_t staticInitOK=kTRUE) const;
  void generateEvent(Int_t code);
  
protected:
  
  RooRealProxy x ;
  RooRealProxy mean ;
  RooRealProxy sigma ;
  
  Double_t evaluate() const ;
  
private:
  
  ClassDef(RooLandauZ,1) // LandauZ Distribution PDF
};

#endif

using namespace std;

ClassImp(RooLandauZ)


//_____________________________________________________________________________
RooLandauZ::RooLandauZ(const char *name, const char *title, RooAbsReal& _x, RooAbsReal& _mean, RooAbsReal& _sigma) :
  RooAbsPdf(name,title),
  x("x","Dependent",this,_x),
  mean("mean","Mean",this,_mean),
  sigma("sigma","Width",this,_sigma)
{
}
 

//_____________________________________________________________________________
RooLandauZ::RooLandauZ(const RooLandauZ& other, const char* name) : 
  RooAbsPdf(other,name),
  x("x",this,other.x),
  mean("mean",this,other.mean),
  sigma("sigma",this,other.sigma)
{
} 


//_____________________________________________________________________________
Double_t RooLandauZ::evaluate() const
{
  //  return TMath::Landau(x, mean, sigma);
  const Double_t par[2] = {mean, sigma};
  Double_t xd = x;
  return landauZ(&xd,(Double_t *) par);
  //  return xx*TMath::Landau(xx,mpc,sigmad);
  //  return xx*TMath::Landau(xx,mpc,sigmad) / sigmad;

}


//_____________________________________________________________________________
Int_t RooLandauZ::getGenerator(const RooArgSet& directVars, RooArgSet &generateVars, Bool_t /*staticInitOK*/) const
{
  if (matchArgs(directVars,generateVars,x)) return 1 ;  
  return 0 ;
}


//_____________________________________________________________________________
void RooLandauZ::generateEvent(Int_t code)
{
  assert(code==1) ;
  Double_t xgen ;
  Double_t mpshift  = -0.22278298; // 1.0844535734 + 0.61417;       // Landau maximum location
  // MP shift correction
  Double_t meand = mean;
  Double_t sigmad = sigma;
  Double_t mpc = TMath::Exp(meand) - mpshift * sigmad; 
  while(1) {    
    xgen = RooRandom::randomGenerator()->Landau(mpc,sigma);
    if (xgen <= 0) continue;
    Double_t xgl = TMath::Log(xgen) ;
    if (xgl<x.max() && xgl>x.min()) {
      x = xgl;
      break;
    }
  }
  return;
}

//________________________________________________________________________________

void convolution(const Char_t *hName = "f39_27") {
  // S e t u p   c o m p o n e n t   p d f s 
  // ---------------------------------------

  // Construct observable
  RooRealVar t("t","t",-2,6) ;

  // Construct landauz(t,ml,sl) ;
  RooRealVar ml("ml","log mean landauz",0.0,-20,20) ;
  RooRealVar sl("sl","sigma landauz",0.10,0.01,10) ;
  RooLandauZ landauz("lx","lx",t,ml,sl) ;
  // Construct gauss(t,mg,sg)
  RooRealVar mg("mg","mg",0) ;
  RooRealVar sg("sg","sg",0.25,0.01,10) ;
  RooGaussian gauss("gauss","gauss",t,mg,sg) ;

  // C o n s t r u c t   c o n v o l u t i o n   p d f 
  // ---------------------------------------

  // Set #bins to be used for FFT sampling to 10000
  t.setBins(10000,"cache") ; 
#if 1
  // Construct landauz (x) gauss
 
  RooFFTConvPdf lxg("lxg","landauZ (X) gauss",t,landauz,gauss) ;
#else
  RooLandauZ &lxg = *&landauz;
#endif


  // S a m p l e ,   f i t   a n d   p l o t   c o n v o l u t e d   p d f 
  // ----------------------------------------------------------------------

  // Sample 1000 events in x from gxlx
#if 0
  RooDataSet* data = lxg.generate(t,10000) ;
#else
  TH1 *hist = (TH1 *) gDirectory->Get(hName);
  if (! hist) return;
  RooDataHist* data = new RooDataHist("data","data",t,Import(*hist));
#endif

  // Fit gxlx to data
  lxg.fitTo(*data) ;

  // Plot data, landauz pdf, landauz (X) gauss pdf
  RooPlot* frame = t.frame(Title("landauz (x) gauss convolution")) ;
  data->plotOn(frame) ;
  lxg.plotOn(frame) ;
  landauz.plotOn(frame,LineStyle(kDashed)) ;


  // Draw frame on canvas
  new TCanvas("rf208_convolution","rf208_convolution",600,600) ;
  gPad->SetLeftMargin(0.15) ; frame->GetYaxis()->SetTitleOffset(1.4) ; frame->Draw() ;

}



