// StSsdEvent.hh
// Ludovic Gaudichet

#ifndef  STAR_StSsdEvent_hh
#define  STAR_StSsdEvent_hh

# include "StSsdObjects.hh"

class StSsdEvent
{
public :
  StSsdEvent();
  ~StSsdEvent();

  int addTrack(track newOne);
  track *getTrack(int trackNumber);
  const int numberOfTracks() const;
  double processTracks();
  globalPoint getVertex();

  double processCosmics();
  int setCosmicChi2(int ichi2, int track, int hitnumber, globalPoint &p);

  // ************** for derivatives : ***
  int trackHitNumber(int track, int wafer);
  int pointID(int track, int hitNumber);
  int setChi2(int ichi2, int track, int hitnumber, globalPoint &p);
  double chi2DiffPerHit(int itrack);
  //*************************************

  const int isTrackSelected(int itrack) const;
  
  double chi2ab_Np( globalPoint &aa, globalPoint &bb, globalPoint *pp0, int N,
		    globalPoint &p, int i_p);
protected :

  globalPoint mVertex;
  track **mTracks;
  int maxNumTracks;
  int mNumTracks;
  double mTotalChi2;
};


const inline int StSsdEvent::numberOfTracks() const
{ return mNumTracks; };

const inline int StSsdEvent::isTrackSelected(int itrack) const
{ return mTracks[itrack]->flag; };

#endif
