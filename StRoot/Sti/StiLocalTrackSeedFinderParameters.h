#ifndef StiLocalTrackSeedFinderParameters_H
#define StiLocalTrackSeedFinderParameters_H
#include "Sti/Base/EditableParameters.h"

class StiLocalTrackSeedFinderParameters : public EditableParameters
{
public: 
  StiLocalTrackSeedFinderParameters();
  StiLocalTrackSeedFinderParameters(const StiLocalTrackSeedFinderParameters & pars);
  ~StiLocalTrackSeedFinderParameters();
  const StiLocalTrackSeedFinderParameters & operator=(const StiLocalTrackSeedFinderParameters & p);
  void   initialize();
  void loadDS(TDataSet&);
  void loadFS(ifstream& inFile);
  friend class StiLocalTrackSeedFinder;
  friend  ostream& operator<<(ostream& os, const StiLocalTrackSeedFinderParameters& par);
 protected:

  //define search window in the next layer when connecting two points
  double _deltaY;
  double _deltaZ;
  //define the number of points to connect
  int _seedLength;
  //define search window in the next layer when extending a coonection of points
  double _extrapDeltaY;
  double _extrapDeltaZ;
  //Define the max number we can skip
  int _maxSkipped;
  //define the Min/Max number of points to extrapolate
  int _extrapMinLength;
  int _extrapMaxLength;
  //Use the origin to calculate helix?
  bool _useOrigin;  
  bool _doHelixFit; //true-> fit, false-> calculate
};

#endif
