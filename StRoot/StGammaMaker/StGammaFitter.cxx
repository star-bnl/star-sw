//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 28 July 2007
//

#include "TVirtualFitter.h"
#include "TH1.h"
#include "TF1.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StGammaCandidate.h"
#include "StGammaFitterResult.h"
#include "StGammaFitter.h"

ClassImp(StGammaFitter);

StGammaFitter* StGammaFitter::mInstance = 0;
const char* StGammaFitter::mFormula = "[0]*(0.69*exp(-0.5*((x-[1])/0.87)**2)/(sqrt(2*pi)*0.87)+0.31*exp(-0.5*((x-[1])/3.3)**2)/(sqrt(2*pi)*3.3))";

TVirtualFitter* StGammaFitter::mMinuit = 0;
TH1* StGammaFitter::hU = 0;
TH1* StGammaFitter::hV = 0;
TF1* StGammaFitter::fU = 0;
TF1* StGammaFitter::fV = 0;
TF1* StGammaFitter::fResidualCut = 0;

StGammaFitter::StGammaFitter()
{
  mMinuit = TVirtualFitter::Fitter(0, 3);
  mMinuit->SetFCN(fcn);

  hU = new TH1F("hU", "hU", 288, 0, 288);
  hV = new TH1F("hV", "hV", 288, 0, 288);

  fU = new TF1("fU", mFormula, 0, 288);
  fV = new TF1("fV", mFormula, 0, 288);

  fResidualCut = new TF1("fResidualCut", "pol2", 0, 200);
  fResidualCut->SetParameters(100, 0, 0.05);
}

StGammaFitter::~StGammaFitter()
{
  delete hU;
  delete hV;
  delete fU;
  delete fV;
  delete fResidualCut;
}

StGammaFitter* StGammaFitter::instance()
{
  if (!mInstance) mInstance = new StGammaFitter;
  return mInstance;
}

int StGammaFitter::fitSector(StGammaCandidate* candidate, StGammaFitterResult* fit)
{
  // Require at least 2 hits in each plane
  if (candidate->numberOfSmdu() < 2 || candidate->numberOfSmdv() < 2)
    return 9;			// reserved status code

  // Get EEMC hits
  hU->Reset();

  for (int i = 0; i < candidate->numberOfSmdu(); ++i) {
    StGammaStrip* strip = candidate->smdu(i);
    hU->Fill(strip->index, 1000 * strip->energy); // MeV
  }

  hV->Reset();

  for (int i = 0; i < candidate->numberOfSmdv(); ++i) {
    StGammaStrip* strip = candidate->smdv(i);
    hV->Fill(strip->index, 1000 * strip->energy); // MeV
  }  

#if 0
  // Get seed tower
  StGammaTower* tower = candidate->mytower(0);
  // Conversion tower id -> sector, subsector, etabin
  int sector, subsector, etabin, phibin;
  getSectorSubEtaPhiFromTowerId(tower->id, sector, subsector, etabin, phibin);

  // Get tower at cluster position
  EEmcGeomSimple::Instance().getTower(candidate->position(), sector, subsector, etabin);

  // Get range of SMD strips under the seed tower
  int uMin, uMax, vMin, vMax;
  EEmcSmdMap::instance()->getRangeU(sector, subsector, etabin, uMin, uMax);
  EEmcSmdMap::instance()->getRangeV(sector, subsector, etabin, vMin, vMax);

  hU->SetAxisRange(uMin, uMax);
  hV->SetAxisRange(vMin, vMax);
#endif

  float uyield = 0;
  float vyield = 0;
  float umean = 0;
  float vmean = 0;

  estimateYieldMean(hU, uyield, umean);
  estimateYieldMean(hV, vyield, vmean);

  float yield = 0.5 * (uyield + vyield);

  mMinuit->SetParameter(0, "yield", yield, 0.1, 0, 1000);
  mMinuit->SetParameter(1, "umean", umean, 0.1, umean-5, umean+5);
  mMinuit->SetParameter(2, "vmean", vmean, 0.1, vmean-5, vmean+5);

  mMinuit->ExecuteCommand("CALL FCN", 0, 0);
  int status = mMinuit->ExecuteCommand("MINImize", 0, 0);

  //
  //  Returns the status of the execution:
  //    = 0: command executed normally
  //      1: command is blank, ignored
  //      2: command line unreadable, ignored
  //      3: unknown command, ignored
  //      4: abnormal termination (e.g., MIGRAD not converged)
  //      5: command is a request to read PARAMETER definitions
  //      6: 'SET INPUT' command
  //      7: 'SET TITLE' command
  //      8: 'SET COVAR' command
  //      9: reserved
  //     10: END command
  //     11: EXIT or STOP command
  //     12: RETURN command
  //

  mMinuit->PrintResults(1, 0);

  double par[] = {
    mMinuit->GetParameter(0),
    mMinuit->GetParameter(1),
    mMinuit->GetParameter(2)
  };

  int npar;
  double chiSquare;
  fcn(npar, 0, chiSquare, par, 0);

  float ures = residual(hU, fU);
  float vres = residual(hV, fV);
  float totalYield = 2 * mMinuit->GetParameter(0);
  float likelihood = distanceToQuadraticCut(ures + vres, totalYield);

  fit->setYield(mMinuit->GetParameter(0), mMinuit->GetParError(0));
  fit->setUmean(mMinuit->GetParameter(1), mMinuit->GetParError(1));
  fit->setVmean(mMinuit->GetParameter(2), mMinuit->GetParError(2));
  fit->setUresidual(ures);
  fit->setVresidual(vres);
  fit->setUsigma(hU->GetRMS());
  fit->setVsigma(hV->GetRMS());
  fit->setChiSquare(chiSquare);
  fit->setNdf(npar);
  fit->setLikelihood(likelihood);

  return status;
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

  int leftMin = max(mean - 40, 1);
  int leftMax = max(mean -  2, 1);

  int rightMin = min(mean +  2, 288);
  int rightMax = min(mean + 40, 288);

  float leftResidual = 0;
  for (int bin = leftMin; bin <= leftMax; ++bin) {
    float data = h1->GetBinContent(bin);
    float x = h1->GetBinCenter(bin);
    float fit = f1->Eval(x);
    leftResidual += data - fit;
  }

  float rightResidual = 0;
  for (int bin = rightMin; bin <= rightMax; ++bin) {
    float data = h1->GetBinContent(bin);
    float x = h1->GetBinCenter(bin);
    float fit = f1->Eval(x);
    rightResidual += data - fit;
  }

  return max(leftResidual, rightResidual);
}

void StGammaFitter::getSectorSubEtaPhiFromTowerId(int id, int& sector, int& subsector, int& etabin, int& phibin)
{
  phibin = id / 12;
  etabin = id % 12;
  sector = phibin / 5;
  subsector = phibin % 5;
}

void StGammaFitter::fcn(int& npar, double* gin, double& f, double* par, int iflag)
{
  npar = 0;
  f = 0;

  fU->SetParameters(par[0], par[1]);
  fV->SetParameters(par[0], par[2]);

  int umean = hU->FindBin(par[1]);
  int umin = max(umean - 40, 1);
  int umax = min(umean + 40, 288);

  for (int bin = umin; bin <= umax; ++bin) {
    double y = hU->GetBinContent(bin);
    if (!y) continue;
    ++npar;
    double dy = hU->GetBinError(bin);
    double x  = hU->GetBinCenter(bin);
    double fx = fU->Eval(x);
    double chi = (y - fx) / dy;
    f += chi * chi;
  }

  int vmean = hV->FindBin(par[2]);
  int vmin = max(vmean - 40, 1);
  int vmax = min(vmean + 40, 288);

  for (int bin = vmin; bin <= vmax; ++bin) {
    double y = hV->GetBinContent(bin);
    if (!y) continue;
    ++npar;
    double dy = hV->GetBinError(bin);
    double x  = hV->GetBinCenter(bin);
    double fx = fV->Eval(x);
    double chi = (y - fx) / dy;
    f += chi * chi;
  }

  npar -= 3;
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
