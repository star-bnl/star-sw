///
/// \author Pibero Djawotho <pibero@indiana.edu>
/// \author Indiana University
/// \date 7 July 2007
///

#include "TH1.h"
#include "TF1.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/EEmcSmdMap/EEmcSmdMap.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

#include "StGammaCandidate.h"
#include "StGammaFitterResult.h"
#include "StGammaFitter.h"

ClassImp(StGammaFitter);

StGammaFitter* StGammaFitter::mInstance = 0;

StGammaFitter::~StGammaFitter()
{
  if (mInstance) {
    delete mInstance;
    mInstance = 0;
  }
}

StGammaFitter* StGammaFitter::instance()
{
  if (!mInstance) mInstance = new StGammaFitter;
  return mInstance;
}

bool StGammaFitter::fitSector(StGammaCandidate* candidate, StGammaFitterResult* u, StGammaFitterResult* v)
{
  if (candidate->detectorId() != StGammaCandidate::kEEmc) return false;

  // Fit SMD u- and v-plane
  TH1F hU("hU", "hU", 288, 0, 288);
  TH1F hV("hV", "hV", 288, 0, 288);

  hU.SetFillColor(kBlue);
  hV.SetFillColor(kRed);

  for (int i = 0; i < candidate->numberOfSmdu(); ++i) {
    StGammaStrip* strip = candidate->smdu(i);
    hU.SetBinContent(strip->index + 1, 1000 * strip->energy); // MeV
  }

  for (int i = 0; i < candidate->numberOfSmdv(); ++i) {
    StGammaStrip* strip = candidate->smdv(i);
    hV.SetBinContent(strip->index + 1, 1000 * strip->energy); // MeV
  }

  u->setSigma(hU.GetRMS());
  v->setSigma(hV.GetRMS());

  // Fit formula
  const char* formula = "[0]*(0.69*exp(-0.5*((x-[1])/0.87)**2)/(sqrt(2*pi)*0.87)+0.31*exp(-0.5*((x-[1])/3.3)**2)/(sqrt(2*pi)*3.3))";

  TF1 fU("fU", formula, 0, 288);
  TF1 fV("fV", formula, 0, 288);

  fU.SetParNames("yield", "mean");
  fV.SetParNames("yield", "mean");

#if 0
  // Get seed tower
  StGammaTower* tower = candidate->mytower(0);
  // Conversion tower id -> setcor, subsector, etabin
  int sector, subsector, etabin, phibin;
  getSectorSubEtaPhiFromTowerId(tower->id, sector, subsector, etabin, phibin);
#endif

  // Get tower at cluster position
  int sector, subsector, etabin;
  EEmcGeomSimple::Instance().getTower(candidate->position(), sector, subsector, etabin);

  // Get range of SMD strips under the seed tower
  EEmcSmdMap* map = EEmcSmdMap::instance();
  int uMin, uMax, vMin, vMax;
  map->getRangeU(sector, subsector, etabin, uMin, uMax);
  map->getRangeV(sector, subsector, etabin, vMin, vMax);

  hU.SetAxisRange(uMin, uMax);
  hV.SetAxisRange(vMin, vMax);

  if (!fitSector(hU, fU)) return false;
  if (!fitSector(hV, fV)) return false;

  u->setYield(fU.GetParameter("yield"));
  v->setYield(fV.GetParameter("yield"));
  u->setMean(fU.GetParameter("mean") - 1);
  v->setMean(fV.GetParameter("mean") - 1);
  u->setChi2(fU.GetChisquare());
  v->setChi2(fV.GetChisquare());
  u->setNdf(fU.GetNDF());
  v->setNdf(fV.GetNDF());
  u->setResidual(residual(hU, fU));
  v->setResidual(residual(hV, fV));

  return true;
}

bool StGammaFitter::fitSector(TH1& h1, TF1& f1)
{
  int mean = h1.GetMaximumBin();
  int xmin = max(mean - 2, 1);
  int xmax = min(mean + 2, 288);
  float yield = h1.Integral(xmin, xmax);
  f1.SetParameters(yield, mean);
  xmax = min(xmax + 1, 288);
  h1.Fit(&f1, "QRO", "", xmin, xmax);
  return true;
}

float StGammaFitter::residual(TH1& h1, TF1& f1)
{
  int mean = (int)(f1.GetParameter("mean") + 0.5);

  int leftMin = max(mean - 40, 1);
  int leftMax = max(mean -  2, 1);

  int rightMin = min(mean +  2, 288);
  int rightMax = min(mean + 40, 288);

  float leftResidual = 0;
  for (int bin = leftMin; bin <= leftMax; ++bin) {
    float data = h1.GetBinContent(bin);
    float fit = f1.Eval(bin);
    leftResidual += data - fit;
  }

  float rightResidual = 0;
  for (int bin = rightMin; bin <= rightMax; ++bin) {
    float data = h1.GetBinContent(bin);
    float fit = f1.Eval(bin);
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
