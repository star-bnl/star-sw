//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 28 July 2007
//

// ROOT
#include "TFile.h"
#include "TH1.h"
#include "TF1.h"
#include "TCanvas.h"

// STAR
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

// Local
#include "StGammaEvent.h"
#include "StGammaCandidate.h"
#include "StGammaFitterResult.h"
#include "StGammaFitter.h"

ClassImp(StGammaFitter);

StGammaFitter* StGammaFitter::mInstance = 0;
TH1* StGammaFitter::hU = 0;
TH1* StGammaFitter::hV = 0;
TF1* StGammaFitter::fFit[2];
TF1* StGammaFitter::fResidualCut = 0;
int StGammaFitter::mNdf = 0;
TCanvas* StGammaFitter::mCanvas = 0;
TF1* StGammaFitter::mShowerShapes[3];

StGammaFitter::StGammaFitter()
{
  hU = new TH1F("hU", "hU", 288, 0, 288);
  hV = new TH1F("hV", "hV", 288, 0, 288);

  fResidualCut = new TF1("fResidualCut", "pol2", 0, 200);
  fResidualCut->SetParameters(100, 0, 0.05);

  // Initialize shower shapes with fits from Ilya based on his gamma-jet analysis for
  // different preshower conditions.
  // See http://www.star.bnl.gov/protected/spin/seluzhen/gammaJet/2008.05.27/showerShapes_comparison.html
  static const char* formula[] = {
    "[0]*(0.669864*exp(-0.5*sq((x-[1])/0.574864))+0.272997*exp(-0.5*sq((x-[1])/-1.84608))+0.0585682*exp(-0.5*sq((x-[1])/5.49802)))", // pre1=0, pre2=0
    "[0]*(0.0694729*exp(-0.5*sq((x-[1])/5.65413))+0.615724*exp(-0.5*sq((x-[1])/0.590723))+0.314777*exp(-0.5*sq((x-[1])/2.00192)))", // pre1=0, pre2>0
    "[0]*(0.0955638*exp(-0.5*sq((x-[1])/5.59675))+0.558661*exp(-0.5*sq((x-[1])/0.567596))+0.345896*exp(-0.5*sq((x-[1])/1.9914)))" // pre1>0, pre2>0
  };

  for (int i = 0; i < 3; ++i) {
    const char* name = Form("mShowerShapes%d", i);
    mShowerShapes[i] = new TF1(name, formula[i], 0, 288);
  }
}

StGammaFitter::~StGammaFitter()
{
  // Never called???
  delete hU;
  delete hV;
  delete fResidualCut;
}

StGammaFitter* StGammaFitter::instance()
{
  if (!mInstance) mInstance = new StGammaFitter;
  return mInstance;
}

int StGammaFitter::fit(StGammaCandidate* candidate, StGammaFitterResult* fits, Int_t plane)
{
  // Require at least 5 strips in each SMD plane
  if (candidate->numberOfSmdu() < 5) return 9;
  if (candidate->numberOfSmdv() < 5) return 9;

  // Clear fit results
  //memset(fits, 0, 1 * sizeof(StGammaFitterResult));

  TF1* fit = 0;

  if (candidate->pre1Energy() == 0 && candidate->pre2Energy() == 0)
    fit = mShowerShapes[0];
  else if (candidate->pre1Energy() == 0 && candidate->pre2Energy() > 0)
    fit = mShowerShapes[1];
  else if (candidate->pre1Energy() > 0 && candidate->pre2Energy() > 0)
    fit = mShowerShapes[2];
  else
    return 9;

  // Loop over planes
  //  for (int plane = 0; plane < 2; ++plane) {
  {
    static TH1* hStrips = new TH1F("hStrips", "hStrips", 288, 0, 288);
    hStrips->Reset();

    switch (plane) {
    case 0:
      for (int i = 0; i < candidate->numberOfSmdu(); ++i) {
	StGammaStrip* strip = candidate->smdu(i);
	hStrips->Fill(strip->index, strip->energy);
      }
      break;
    case 1:
      for (int i = 0; i < candidate->numberOfSmdv(); ++i) {
	StGammaStrip* strip = candidate->smdv(i);
	hStrips->Fill(strip->index, strip->energy);
      }
      break;
    }

    // Find maximum strip
    int mean = hStrips->GetMaximumBin();

    // Integrate yield from +/- 2 strips of max strip
    int bin1 = max(1,   mean - 2);
    int bin2 = min(288, mean + 2);

    // Fit to Ilya's shower shape
    float yield = hStrips->Integral(bin1, bin2);
    fit->SetParameters(yield, hStrips->GetBinLowEdge(mean));
    hStrips->Fit(fit, "WWQ", "", hStrips->GetBinLowEdge(bin1), hStrips->GetBinLowEdge(bin2));

    // Save fit results
    fits[plane].yield = yield;
    fits[plane].yieldError = 0;
    fits[plane].centroid = fit->GetParameter(1);
    fits[plane].centroidError = fit->GetParError(1);
    fits[plane].residual = residual(hStrips, fit);
    fits[plane].mean = hStrips->GetMean();
    fits[plane].rms = hStrips->GetRMS();
    fits[plane].chiSquare = fit->GetChisquare();
    fits[plane].ndf = fit->GetNDF();
    fits[plane].prob = fit->GetProb();
  }

  return 0;
}

void StGammaFitter::estimateYieldMean(TH1* h1, float& yield, float& mean)
{
  int bin = h1->GetMaximumBin();
  int xmin = max(bin - 2, 1);
  int xmax = min(bin + 2, 288);
  yield = h1->Integral(xmin, xmax);
  mean = h1->GetBinCenter(bin);
}

float StGammaFitter::residual(TH1* h1, TF1* f1)
{
  int mean = h1->FindBin(f1->GetParameter(1));

  int leftMin = 1;
  int leftMax = max(mean - 3, 1);
  float leftResidual = 0;

  for (int bin = leftMin; bin <= leftMax; ++bin) {
    float data = h1->GetBinContent(bin);
    float x = h1->GetBinCenter(bin);
    float fit = f1->Eval(x);
    leftResidual += data - fit;
  }

  int rightMin = min(mean + 3, 288);
  int rightMax = 288;
  float rightResidual = 0;

  for (int bin = rightMin; bin <= rightMax; ++bin) {
    float data = h1->GetBinContent(bin);
    float x = h1->GetBinCenter(bin);
    float fit = f1->Eval(x);
    rightResidual += data - fit;
  }

  return max(leftResidual, rightResidual);
}

double StGammaFitter::distanceToQuadraticCut(double x, double y)
{
  double a = fResidualCut->GetParameter(0);
  double b = fResidualCut->GetParameter(2);
  double coef[] = { -x, 2*b*(a-y)+1, 0, 2*b*b };
  double r1, r2, r3;
  double x1 = 0;

  if (TMath::RootsCubic(coef, r1, r2, r3)) {
    // 1 real root r1 and 2 complex conjugates r2+i*r3 and r2-i*r3
    // Pick real root
    x1 = r1;
  }
  else {
    // 3 real roots r1, r2, r3
    // Pick positive root
    if (r1 >= 0)
      x1 = r1;
    else if (r2 >= 0)
      x1 = r2;
    else if (r3 >= 0)
      x1 = r3;
    else {
      LOG_ERROR << "Can't find positive root" << endm;
    }
  }

  double y1 = fResidualCut->Eval(x1);
  double d = hypot(x - x1, y - y1);

  return (y > fResidualCut->Eval(x)) ? d : -d;
}

float StGammaFitter::GetMaximum(TH1* h1, float xmin, float xmax)
{
  int bin1 = h1->FindBin(xmin);
  int bin2 = h1->FindBin(xmax);

  float ymax = 0;

  for (int bin = bin1; bin <= bin2; ++bin) {
    float y = h1->GetBinContent(bin);
    if (y > ymax) ymax = y;
  }

  return ymax;
}
