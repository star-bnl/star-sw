#ifndef RadLengthPlots_H_INCLUDED
#define RadLengthPlots_H_INCLUDED

#include "Sti/StiHistograms.h"

class RadLengthPlots : public StiHistograms
{
 public:
  RadLengthPlots();
  RadLengthPlots(const string & name, const string & description);
  ~RadLengthPlots();
  void initialize();
  void fill(StiTrackContainer* mTrackStore);
 protected:
  TH2D * _radLengthVsPhi;
  TH2D * _radLengthVsEta;
  TProfile * _radLengthVsPhiProf;
  TProfile * _radLengthVsEtaProf;
};

#endif

