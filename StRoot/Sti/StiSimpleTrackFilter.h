#ifndef StiSimpleTrackFilter_H
#define StiSimpleTrackFilter_H 1
#include "StiTrackFilter.h"
#include "Parameters.h"
#include "StiObjectFactoryInterface.h"

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

class StiSimpleTrackFilter : public StiTrackFilter, public Parameters
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
  StiSimpleTrackFilter(const string & name, const string & description);
  virtual ~StiSimpleTrackFilter();
  virtual void initialize();
  bool accept(StiTrack * track) const;
};

/*! StiSimpleTrackFilter factory
 */
class StiSimpleTrackFilterFactory : public StiObjectFactoryInterface<StiTrackFilter>
{
public:
  ///This is the only constructor available.
  StiSimpleTrackFilterFactory(const string& newName, 
			      int original=-1, int 
			      incremental=-1, 
			      int maxInc=-1);
  ///Default destructor.
  virtual ~StiSimpleTrackFilterFactory();

 protected:
  ///Return a pointer to a new StiSimpleTrackFilter object on the heap.
  virtual void * makeNewObject() const;
  
 private:
  StiSimpleTrackFilterFactory(); //Not implemented
};

#endif
