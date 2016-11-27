#ifndef G_TRACK_SRIM_H
#define G_TRACK_SRIM_H
#include <vector>
#include "Track.hh"

namespace Garfield {

class TrackSrim : public Track {

 public:
  // Constructor
  TrackSrim();
  // Destructor
  ~TrackSrim() {}

  void SetWorkFunction(const double w) { m_work = w; }
  double GetWorkFunction() const { return m_work; }

  void SetFanoFactor(const double f) { m_fano = f; }
  double GetFanoFactor() const { return m_fano; }

  void SetDensity(const double density) { m_density = density; }
  double GetDensity() const { return m_density; }

  void SetCharge(const double q) {
    m_q = q;
    m_chargeset = true;
  }
  double GetCharge() const { return m_q; }

  void SetMass(const double m) { m_mass = m; }
  double GetMass() const { return m_mass; }

  void SetAtomicMassNumbers(const double a, const double z) {
    m_a = a;
    m_z = z;
  }
  void GetAtomicMassMumbers(double& a, double& z) const {
    a = m_a;
    z = m_z;
  }

  void SetInitialEnergy(const double e) { m_initialenergy = e; }
  double GetInitialEnergy() const { return m_initialenergy; }

  void SetModel(const int m) { m_model = m; }
  int GetModel() const { return m_model; }

  void EnableTransverseStraggling() { m_useTransStraggle = true; }
  void DisableTransverseStraggling() { m_useTransStraggle = false; }
  void EnableLongitudinalStraggling() { m_useLongStraggle = true; }
  void DisableLongitudinalStraggling() { m_useLongStraggle = false; }
  void EnablePreciseVavilov() { m_precisevavilov = true; }
  void DisablePreciseVavilov() { m_precisevavilov = false; }
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

  void SetTargetClusterSize(const unsigned int n) { m_nsize = n; }
  unsigned int GetTargetClusterSize() const { return m_nsize; }

  void SetTrack(const double x0, const double y0, const double z0,
                const double xd, const double yd, const double zd,
                const double l);
  void GetTrack(double& x0, double& y0, double& z0, double& xd, double& yd,
                double& zd, double& l);

  bool ReadFile(const std::string& file);
  void Print();
  void PlotEnergyLoss();
  void PlotRange();
  void PlotStraggling();
  double DedxEM(const double e) const;
  double DedxHD(const double e) const;
  void PreciseLoss(const double step, const double estart, double& deem,
                   double& dehd) const;
  bool EstimateRange(const double ekin, const double step, double& stpmax);
  bool SmallestStep(double ekin, double de, double step, double& stpmin);

  bool Generate();
  double RndmEnergyLoss(const double ekin, const double de, const double step);

  bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                  int& n, double& e, double& extra);

  bool Track(const double x0, const double y0, const double z0, const double t0,
             const double dx0, const double dy0, const double dz0);
  // Not used - mandatory due to error in Track.hh
  bool NewTrack(const double x0, const double y0, const double z0,
                const double t0, const double dx0, const double dy0,
                const double dz0);

 protected:
  /// Use precise Vavilov generator
  bool m_precisevavilov;
  /// Include transverse straggling
  bool m_useTransStraggle;
  /// Include longitudinal straggling
  bool m_useLongStraggle;
  /// Track has been defined
  bool m_trackset;
  /// Mass has been defined
  bool m_massset;
  /// Charge gas been defined
  bool m_chargeset;
  /// Produce debugging output
  bool m_debug;
  /// Density [g/cm3]
  double m_density;
  /// Work function [eV]
  double m_work;
  /// Fano factor [-]
  double m_fano;
  /// Charge of ion
  double m_q;
  /// Mass of ion [MeV]
  double m_mass;
  /// A and Z of the gas
  double m_a;
  double m_z;
  /// Initial energy of ion
  double m_initialenergy;
  /// Starting position
  double m_xt0, m_yt0, m_zt0;
  /// Initial direction
  double m_xdir, m_ydir, m_zdir;
  /// Specified track length
  double m_tracklength;
  /// Energy in energy loss table [MeV]
  std::vector<double> m_energy;
  /// EM energy loss [MeV cm2/g]
  std::vector<double> m_emloss;
  /// Hadronic energy loss [MeV cm2/g]
  std::vector<double> m_hdloss;
  /// Projected range [cm]
  std::vector<double> m_range;
  /// Transverse straggling [cm]
  std::vector<double> m_transstraggle;
  /// Longitudinal straggling [cm]
  std::vector<double> m_longstraggle;

  // Index of the next cluster to be returned
  unsigned int m_currcluster;
  /// Fluctuation model (0 = none, 1 = Landau, 2 = Vavilov,
  ///                    3 = Gaussian, 4 = Combined)
  unsigned int m_model;
  /// Targeted cluster size
  int m_nsize;
  struct cluster {
    double x, y, z,  // Cluster locations
        ec,          // Energy spent to make the clusterec
        kinetic;     // Ion energy when cluster was created
    int electrons;   // Number of electrons in this cluster
  };
  std::vector<cluster> clusters;
};
}

#endif
