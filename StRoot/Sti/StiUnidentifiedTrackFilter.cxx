#include "StiTrack.h"
#include "StiUnidentifiedTrackFilter.h"

StiUnidentifiedTrackFilter::StiUnidentifiedTrackFilter()
{
  setDefaults();
}


void StiUnidentifiedTrackFilter::setDefaults()
{
  /** 
   * Set default values of this filters
   */ 
  usePhi = false;
  useEta = false;
  usePt  = false;
  useTpcDedx = false;
  useSvtDedx = false;
  useDca  = false;
  usePtsCount = false;
  useTpcPtsCount = false;
  useSvtPtsCount = false;
  useChi2          = false;
  useCharged       = true;
  useFitToTotalPtsRatio = false;

  setEtaRange(-0.5, 0.5);
  setPtRange(0.1,1.5);
  setPointCountRange(15, 50);
  //setTpcPointCountRange(5, 50);
  //setSvtPointCountRange(5, 50);
  setTpcDedxRange(0.25,50.);
  setDcaRange(0.,3.);            
}

bool StiUnidentifiedTrackFilter::accept(StiTrack * t)
{
  if (t)
    {
      analyzedTrackCount++;
      float phi = t->getPhi(); 
      float eta = t->getPseudoRapidity();
      float pt  = t->getPt();
      float dca = t->getDca();
      //float tpcDedx = 0.;//1.e6*t->getTpcDedx();
      //float svtDedx = 0.;//1.e6*t->getSvtDedx();
      float chi2 = t->getChi2();
      int   pts    = t->getPointCount();
      //int   tpcPts = 0;//t->getTpcPointCount();
      //int   svtPts = 0;//t->getSvtPointCount();
      int   fitPts = t->getFitPointCount();
      float ptsRatio = float(fitPts)/float(pts);
      if ( 
	  ((!usePhi)           || (phi>=minPhi          && phi<maxPhi) ) &&
	  ((!useEta)           || (eta>=minEta          && eta<maxEta) ) &&
	  ((!usePt)            || (pt>=minPt            && pt<maxPt  ) ) && 
	  ((!useDca)           || (dca>=minDca          && dca<maxDca) ) &&
	  // ((!useTpcDedx)       || (tpcDedx>=minTpcDedx     && tpcDedx<maxTpcDedx      ) ) &&
	  // ((!useSvtDedx)       || (svtDedx>=minSvtDedx     && svtDedx<maxSvtDedx      ) ) &&
	  ((!usePtsCount)      || (pts>=minPtsCount         &&  pts<maxPtsCount) )        &&
	  //((!useTpcPtsCount)   || (tpcPts>=minTpcPtsCount   &&  tpcPts<maxTpcPtsCount)   ) &&
	  //((!useSvtPtsCount)   || (svtPts>=minSvtPtsCount   &&  svtPts<maxSvtPtsCount)   ) &&
	  ((!useFitPtsCount)   || (fitPts>=minFitPtsCount   &&  fitPts<maxFitPtsCount)   ) &&
	  ((!useFitToTotalPtsRatio)|| (ptsRatio>=minFitToTotalPtsRatio &&  ptsRatio<maxFitToTotalPtsRatio)) &&
	  ((!useChi2)                || (chi2>=minChi2              &&  chi2<maxChi2)              ) &&
	  ((!useCharged)             || (t->getCharge()!=0)) 
	  )
	{
	  acceptedTrackCount++;
	  return true;
	}
      else
	{
	  return false;	
	}
    }
  else
    return false;
}


void StiUnidentifiedTrackFilter::setEtaRange(double min, double max)
{
  if (min<max)
    {
      useEta = true;
      minEta = min;
      maxEta = max;
    }
  else
    useEta = false;
}

void StiUnidentifiedTrackFilter::setPtRange(double min, double max)
{
  if (min<max)
    {
      usePt = true;
      minPt = min;
      maxPt = max;
    }
  else
    usePt = false;
}

void StiUnidentifiedTrackFilter::setPhiRange(double min, double max)
{
  if (min<max)
    {
      usePhi = true;
      minPhi = min;
      maxPhi = max;
    }
  else
    usePhi = false;
}

void StiUnidentifiedTrackFilter::setTpcDedxRange(double min, double max)
{
  if (min<max)
    {
      useTpcDedx = true;
      minTpcDedx = min;
      maxTpcDedx = max;
    }
  else
    useTpcDedx = false;
}

void StiUnidentifiedTrackFilter::setSvtDedxRange(double min, double max)
{
  if (min<max)
    {
      useSvtDedx = true;
      minSvtDedx = min;
      maxSvtDedx = max;
    }
  else
    useSvtDedx = false;
}

void StiUnidentifiedTrackFilter::setDcaRange(double min, double max)
{
  if (min<max)
    {
      useDca = true;
      minDca = min;
      maxDca = max;
    }
  else
    useDca = false;
}


void StiUnidentifiedTrackFilter::setPointCountRange(int min, int max)
{
  if (min<max)
    {
      usePtsCount = true;
      minPtsCount = min;
      maxPtsCount = max;
    }
  else
    usePtsCount = false;
}

void StiUnidentifiedTrackFilter::setTpcPointCountRange(int min, int max)
{
  if (min<max)
    {
      useTpcPtsCount = true;
      minTpcPtsCount = min;
      maxTpcPtsCount = max;
    }
  else
    useTpcPtsCount = false;
}
void StiUnidentifiedTrackFilter::setSvtPointCountRange(int min, int max)
{
  if (min<max)
    {
      useSvtPtsCount = true;
      minSvtPtsCount = min;
      maxSvtPtsCount = max;
    }
  else
    useSvtPtsCount = false;
}

void StiUnidentifiedTrackFilter::setFitPointCountRange(int min, int max)
{
  if (min<max)
    {
      useFitPtsCount = true;
      minFitPtsCount = min;
      maxFitPtsCount = max;
    }
  else
    useFitPtsCount = false;
}

void StiUnidentifiedTrackFilter::setFitToTotalPointRatioRange(float min, float max)
{
  if (min<max)
    {
      useFitToTotalPtsRatio = true;
      minFitToTotalPtsRatio = min;
      maxFitToTotalPtsRatio = max;
    }
  else
    useFitToTotalPtsRatio = false;
}


void StiUnidentifiedTrackFilter::setChi2Range(double min, double max)
{
  if (min<max)
    {
      useChi2 = true;
      minChi2 = min;
      maxChi2 = max;
    }
  else
    useChi2 = false;
}

