#ifndef StiSimpleTrackFilter_H
#define StiSimpleTrackFilter_H 1

#include <string>
using std::string;
#include "StiTrackFilter.h"

class StiTrack;

/**
 * StiSimpleTrackFilter. Instances of this class are track filter
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

class StiSimpleTrackFilter : public StiTrackFilter
{
 public:
	
  enum StiTrackFilterIdentifier {kChi2=0,
				 kPhi,
				 kPt,
				 kP,
				 kPseudoRap,
				 kNPts,
				 kNGaps,
				 kNToNmaxPts,
				 kNTpcPts,
				 kNSvtPts,
				 kTpcDedx,
				 kSvtDedx};
  StiSimpleTrackFilter();
  virtual ~StiSimpleTrackFilter();
  void setDefaults();
  bool accept(StiTrack * track) const;
  void set(int id, const char * name, double minimum=1, double maximum=0, bool use=false);
  void set(int id, double minimum, double maximum, bool use=true);
  
 protected:
  
  bool   used[100];
  double low[100];
  double hi[100];
  string * names[100];
};


#endif
