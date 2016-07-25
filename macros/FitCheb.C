/////////////////////////////////////////////////////////////////////////
//
// 'ADDITION AND CONVOLUTION' RooFit tutorial macro #201
// 
// Composite p.d.f with signal and background component
//
// pdf = f_bkg * bkg(x,a0,a1) + (1-fbkg) * (f_sig1 * sig1(x,m,s1 + (1-f_sig1) * sig2(x,m,s2)))
//
//
// 07/2008 - Wouter Verkerke 
//
/////////////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "RooGlobalFunc.h"
#endif
#include "RooRealVar.h"
#include "RooDataSet.h"
#include "RooGaussian.h"
#include "RooChebychev.h"
#include "RooAddPdf.h"
#include "TCanvas.h"
#include "TAxis.h"
#include "RooPlot.h"
#include "TH1.h"
using namespace RooFit ;


void FitCheb(TH1 *hh)
{
  if (! hh) return;
  // Declare observable x
  RooRealVar x("x","x",-0.5,0.5) ;
  ////////////////////////////////////////////////////////
  // I m p o r t i n g   R O O T   h i s t o g r a m s  //
  ////////////////////////////////////////////////////////
  // Create a binned dataset that imports contents of TH1 and associates its contents to observable 'x'
  RooDataHist dh("dh","dh",x,Import(*hh)) ;

  // P l o t   a n d   f i t   a   R o o D a t a H i s t
  // ---------------------------------------------------

  // Make plot of binned dataset showing Poisson error bars (RooFit default)
  RooPlot* frame = x.frame(Title("Imported TH1 with Poisson error bars")) ;
  dh.plotOn(frame) ; 

  // ---------------------------------------------------------
  
  // S e t u p   c o m p o n e n t   p d f s 
  // ---------------------------------------


  // Build Chebychev polynomial p.d.f.  
  RooRealVar a0("a0","a0", 0.1,-1e10,1e10) ;
  RooRealVar a1("a1","a1", 0.1,-1e10,1e10) ;
  RooChebychev bkg("bkg","Background",x,RooArgSet(a0,a1)) ;

  //  RooAddPdf  model("model","a",RooArgList(bkg)) ;

  bkg.fitTo(dh);
  bkg.plotOn(frame) ;
   // Print structure of composite p.d.f.
  bkg.Print("t") ;

}

