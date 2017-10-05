#ifndef G_TRACK_ELECTRON
#define G_TRACK_ELECTRON

#include <string>
#include <vector>

#include "Track.hh"

namespace Garfield {

/// Ionization calculation based on MIP program (S. Biagi).

class TrackElectron : public Track {

 public:
  // Constructor
  TrackElectron();
  // Destructor
  virtual ~TrackElectron() {}

  virtual void SetParticle(const std::string& particle);

  virtual bool NewTrack(const double x0, const double y0, const double z0,
                        const double t0, const double dx0, const double dy0,
                        const double dz0);

  virtual bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                          int& ncls, double& ecls, double& extra);

  virtual double GetClusterDensity();
  virtual double GetStoppingPower();

 private:
  bool m_ready;

  // Particle coordinates and direction
  double m_x, m_y, m_z, m_t;
  double m_dx, m_dy, m_dz;

  // Parameters in ionization cross-section
  struct component {
    double fraction;
    // Dipole moment
    double m2Ion;
    // Constant
    double cIon;
    // Density correction term
    double x0Dens, x1Dens;
    double cDens;
    double aDens, mDens;
    // Opal-Beaty-Peterson splitting factor
    double wSplit;
    // Ionisation threshold
    double ethr;
    // Relative cross-section
    double p;
  };
  std::vector<component> m_components;

  // Secondary electrons
  struct electron {
    double x, y, z;
    double energy;
  };
  std::vector<electron> m_electrons;

  // Medium name
  std::string m_mediumName;
  // Atomic density
  double m_mediumDensity;
  // Mean free path
  double m_mfp;

  bool SetupGas(Medium* gas);
  bool UpdateCrossSection();
};
}

#endif
