#ifndef StiUnidentifiedTrackFilter_H
#define StiUnidentifiedTrackFilter_H 1

#include "StiFilter.h"
#include "StiTrackFilter.h"


class StiTrack;

/**
 * StiUnidentifiedTrackFilter. Instances of this class are track filter
 * enabling filtering of tracks based on their pseudo-rapidity, pt,  and other 
 * features that do not require the track particle species to be identified.
 *
 * kChi2     : Chisquare of the track
 * kPhi      : Azimuthal angle of the track 
 * kPt       : Transverse Momentum of the track (GeV/c)
 * kPseudoRapidity : Pseudo-rapidity of the track
 * kRapidity       : Rapidity
 * kNPts           : Number of nodes (with hits) on the track
 * kNFitPts        : Number of nodes (with hits) used in the fit of the track
 * kNGaps          : Number of active layers without hits on the track
 * kFitToTotalPts  : Ratio of fit to total points on the track
 * kPrimaryDca     : Distance to closest approach (DCA) to primary vertex
 * kNTpcPts        : Number of TPC hits
 * kNSvtPts        : Number of SVT hits
 * kTpcDedx        : Value of TPC truncated Dedx Mean
 * kSvtDedx        : Value of SVT truncated Dedx Mean
 * kTrackType      : Type of Track
 *                   0  - All types selected - same as not using this filter
 *                   1  - Primary tracks only - track which include primary vertex
 *                   2  - Secondary tracks only - tracks which do not primary vertex
 * kCharged        : Charge of track
 *                   -1 - Negative only
 *                    0 - Neutral only
 *                    1 - Positive only
 *                    2 - Negative or Positive only
 *                   other - All accepted - same as not using this filter
 */

class StiUnidentifiedTrackFilter : public StiTrackFilter
{
 public:
	
	enum StiTrackFilterIdentifier {kChi2=0,
																 kPhi,
																 kPt,
																 kPseudoRapidity,
																 kRapidity,
																 kNPts,
																 kNFitPts,
																 kNGaps,
																 kFitToTotalPts,
																 kPrimaryDca,
																 kNTpcPts,
																 kNSvtPts,
																 kTpcDedx,
																 kSvtDedx,
																 kTrackType,
																 kCharged};
  StiUnidentifiedTrackFilter();
  virtual ~StiUnidentifiedTrackFilter()
    {}
  virtual void setDefaults();
  virtual bool accept(StiTrack * track);

protected:

  StiFilterVec filters;
};


#endif


//////////////
/*

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

};

#endif
*/
