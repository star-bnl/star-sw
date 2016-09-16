// Energy loss calculation using the Photoabsorption-Ionisation Model

#ifndef G_TRACK_PAI
#define G_TRACK_PAI

#include <string>
#include <vector>

#include "Track.hh"

namespace Garfield {

class TrackPAI : public Track {

 public:
  // Constructor
  TrackPAI();
  // Destructor
  ~TrackPAI() {}

  bool NewTrack(const double x0, const double y0, const double z0,
                const double t0, const double dx0, const double dy0,
                const double dz0);

  bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                  int& ncls, double& ecls, double& extra);

  double GetClusterDensity();
  double GetStoppingPower();

 private:
  bool m_ready;

  // Particle coordinates and direction
  double m_x, m_y, m_z, m_t;
  double m_dx, m_dy, m_dz;
  // Particle energy and speed
  double m_e;
  double m_speed;
  // Max. energy transfer in a collision
  double m_emax;

  // Total inelastic mean free path
  double m_imfp;
  // Stopping power
  double m_dedx;

  // Dielectric function
  int m_nSteps;
  struct opticalData {
    double eps1, eps2;
    double integral;
  };
  std::vector<opticalData> m_opticalDataTable;

  // Tables for interpolation of cumulative distribution functions
  std::vector<double> m_energies;
  std::vector<double> m_cdf;
  std::vector<double> m_rutherford;

  struct electron {
    // Direction
    double dx, dy, dz;
    // Energy
    double energy;
    // Type (electron, hole)
    int type;
  };
  std::vector<electron> m_electrons;
  std::vector<electron> m_holes;

  // Medium properties
  std::string m_mediumName;
  double m_mediumDensity;
  double m_electronDensity;

  bool SetupMedium(Medium* medium);
  bool SetupCrossSectionTable();

  double ComputeMaxTransfer() const;

  double ComputeCsTail(const double emin, const double emax);
  double ComputeDeDxTail(const double emin, const double emax);

  double SampleAsymptoticCs(double u) const;
  double SampleAsymptoticCsSpinZero(const double emin, double u) const;
  double SampleAsymptoticCsSpinHalf(const double emin, double u) const;
  double SampleAsymptoticCsSpinOne(const double emin, double u) const;
  double SampleAsymptoticCsElectron(const double emin, double u) const;
  double SampleAsymptoticCsPositron(const double emin, double u) const;

  double LossFunction(const double eps1, const double eps2) const;
};
}

#endif
