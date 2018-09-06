#ifndef G_TRACK_BICHSEL_H
#define G_TRACK_BICHSEL_H

#include "Track.hh"

namespace Garfield {

/// Generate tracks using differential cross-sections 
/// for silicon computed by Hans Bichsel.
/// References:
///   - H. Bichsel, Rev. Mod. Phys. 60 (1988), 663-699
///   - https://faculty.washington.edu/hbichsel/

class TrackBichsel : public Track {

 public:
  /// Constructor
  TrackBichsel();
  /// Destructor
  virtual ~TrackBichsel() {}

  virtual bool NewTrack(const double x0, const double y0, const double z0,
                        const double t0, const double dx0, const double dy0,
                        const double dz0);
  virtual bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                          int& n, double& e, double& extra);

  virtual double GetClusterDensity();
  virtual double GetStoppingPower();

  void SetDataFile(const std::string& filename) { m_datafile = filename; }

 private:
  /// Particle rel. momentum.
  double m_bg = 3.16228;
  /// Particle speed
  double m_speed;

  // Particle position and direction
  double m_x = 0., m_y = 0., m_z = 0., m_t = 0.;
  double m_dx = 0., m_dy = 0., m_dz = 1.;

  /// Inverse mean free path
  double m_imfp = 4.05090e4;

  std::string m_datafile = "SiM0invw.inv";
  /// Table of cumulative distribution functions
  std::vector<std::vector<double> > m_cdf;
  int m_iCdf = 2;
  int m_nCdfEntries = -1;

  bool m_isInitialised = false;
  bool m_isInMedium = false;

  double GetInverseMeanFreePath(const double bg);
  bool LoadCrossSectionTable(const std::string& filename);
  void SelectCrossSectionTable();
};
}

#endif
