#ifndef StiCAKalmanTrackFinder_H_INCLUDED
#define StiCAKalmanTrackFinder_H_INCLUDED

#include "Sti/StiKalmanTrackFinder.h"

class StiCATpcTrackerInterface;
class StiCAKalmanTrack;


class StiCAKalmanTrackFinder : public StiKalmanTrackFinder
{
public:
  StiCAKalmanTrackFinder() : StiKalmanTrackFinder() {}

  /// Find all tracks of the currently loaded event
  virtual void findTpcTracks(StiCATpcTrackerInterface &caTrackerInt); 
  virtual void findTracks(); 
  virtual Int_t Fit(StiKalmanTrack *track, Double_t rMin=0);
  static void PrintFitStatus(const int status, const StiCAKalmanTrack* track); // print message according to the status value



};

#endif
