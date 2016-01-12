// Track generation using differential cross-sections computed by H. Bichsel
// References:
//   - H. Bichsel, Rev. Mod. Phys. 60 (1988), 663-699
//   - https://faculty.washington.edu/hbichsel/

#ifndef G_TRACK_BICHSEL_H
#define G_TRACK_BICHSEL_H

#include "Track.hh"

namespace Garfield {

class TrackBichsel : public Track {

 public:
  // Constructor
  TrackBichsel();
  // Destructor
  ~TrackBichsel() {}

  bool NewTrack(const double x0, const double y0, const double z0,
                const double t0, const double dx0, const double dy0,
                const double dz0);
  bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                  int& n, double& e, double& extra);

  double GetClusterDensity();
  double GetStoppingPower();

  void SetDataFile(const std::string& filename) { m_datafile = filename; }

 private:
  // Particle speed and rel. momentum
  double m_bg;
  double m_speed;

  // Particle position and direction
  double m_x, m_y, m_z, m_t;
  double m_dx, m_dy, m_dz;

  // Inverse mean free path
  double m_imfp;

  std::string m_datafile;
  // Table of cumulative distribution functions
  std::vector<std::vector<double> > m_cdf;
  int m_iCdf;
  int m_nCdfEntries;

  bool m_isInitialised;
  bool m_isInMedium;

  double GetInverseMeanFreePath(const double bg);
  bool LoadCrossSectionTable(const std::string& filename);
  void SelectCrossSectionTable();
};
}

#endif
