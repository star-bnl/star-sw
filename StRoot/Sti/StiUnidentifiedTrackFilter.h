#ifndef StiUnidentifiedTrackFilter_H
#define StiUnidentifiedTrackFilter_H 1

#include <iostream.h>
#include <stdlib.h>

#include "StiTrackFilter.h"

class StiTrack;

/**
 * StiUnidentified track filter. This filter shall be used to 
 * permit filtering of tracks based on their pseudo-rapidity and other 
 * features that do not require the track particle species to be identified.
 */
class StiUnidentifiedTrackFilter : public StiTrackFilter
{
 public:

  StiUnidentifiedTrackFilter();
  
  virtual void setDefaults();
  virtual bool accept(StiTrack * track);

  void setPhiRange(double min, double max);
  void setEtaRange(double min, double max);
  void setPtRange(double min, double max);
  void setPointCountRange(int min, int max);
  void setFitPointCountRange(int min, int max);
  void setTpcPointCountRange(int min, int max);
  void setSvtPointCountRange(int min, int max);
  void setFitToTotalPointRatioRange(float min, float max);
  void setTpcDedxRange(double min, double max);
  void setSvtDedxRange(double min, double max);
  void setDcaRange(double min, double max);
  void setChi2Range(double min, double max);

 protected:
  
  bool   usePhi, useEta, usePt, useTpcDedx, useSvtDedx;
  bool   useDca, usePtsCount, useTpcPtsCount, useSvtPtsCount, useFitPtsCount;
  bool   useChi2, useCharged, useFitToTotalPtsRatio;

  float minPhi,           maxPhi;
  float minEta,           maxEta;
  float minPt,            maxPt;
  float minTpcDedx,       maxTpcDedx;
  float minSvtDedx,       maxSvtDedx;
  float minDca,           maxDca;
  int    minPtsCount,      maxPtsCount;
  int    minTpcPtsCount,        maxTpcPtsCount;
  int    minSvtPtsCount,        maxSvtPtsCount;
  int    minFitPtsCount,        maxFitPtsCount;
  float  minFitToTotalPtsRatio, maxFitToTotalPtsRatio;
  float  minChi2,               maxChi2;

ClassDef(StiUnidentifiedTrackFilter, 1)

};

#endif
