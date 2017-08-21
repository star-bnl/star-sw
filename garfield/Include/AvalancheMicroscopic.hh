// Calculate electron avalanches using microscopic tracking

#ifndef G_AVALANCHE_MICROSCOPIC_H
#define G_AVALANCHE_MICROSCOPIC_H

#include <vector>
#include <string>

#include <TH1.h>

#include "Sensor.hh"
#include "ViewDrift.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

class AvalancheMicroscopic {

 public:
  /// Constructor
  AvalancheMicroscopic();
  /// Destructor
  ~AvalancheMicroscopic() {}

  /// Set the sensor.
  void SetSensor(Sensor* sensor);

  /// Switch on drift line plotting.
  void EnablePlotting(ViewDrift* view);
  void DisablePlotting();
  /// Draw a marker at every excitation.
  void EnableExcitationMarkers() { m_plotExcitations = true; }
  void DisableExcitationMarkers() { m_plotExcitations = false; }
  /// Draw a marker at every ionising collision.
  void EnableIonisationMarkers() { m_plotIonisations = true; }
  void DisableIonisationMarkers() { m_plotIonisations = false; }
  /// Draw a marker at every attachment.
  void EnableAttachmentMarkers() { m_plotAttachments = true; }
  void DisableAttachmentMarkers() { m_plotAttachments = false; }

  /// Switch on calculation of induced currents
  void EnableSignalCalculation() { m_useSignal = true; }
  void DisableSignalCalculation() { m_useSignal = false; }

  /// Switch on calculation of the total induced charge
  void EnableInducedChargeCalculation() { m_useInducedCharge = true; }
  void DisableInducedChargeCalculation() { m_useInducedCharge = false; }

  /// Switch on filling histograms for electron energy distribution
  void EnableElectronEnergyHistogramming(TH1* histo);
  void DisableElectronEnergyHistogramming() { m_histElectronEnergy = NULL; }
  void EnableHoleEnergyHistogramming(TH1* histo);
  void DisableHoleEnergyHistogramming() { m_histHoleEnergy = NULL; }

  /** Fill histograms of the distance between successive collisions.
    * \param opt direction ('x', 'y', 'z', 'r')
    */
  void SetDistanceHistogram(TH1* histo, const char opt = 'r');
  void EnableDistanceHistogramming(const int type);
  /// Switch on distance distribution histograms for a given collision type.
  void DisableDistanceHistogramming(const int type);
  void DisableDistanceHistogramming();
  /// Fill histograms of the energy of electrons emitted in ionising collisions.
  void EnableSecondaryEnergyHistogramming(TH1* histo);
  void DisableSecondaryEnergyHistogramming() { m_histSecondary = NULL; }

  /// Switch on storage of drift lines.
  void EnableDriftLines() { m_useDriftLines = true; }
  void DisableDriftLines() { m_useDriftLines = false; }

  /** Switch on photon transport.
    * \remark This feature has not been tested thoroughly. */
  void EnablePhotonTransport() { m_usePhotons = true; }
  void DisablePhotonTransport() { m_usePhotons = false; }

  /// Switch on stepping according to band structure E(k), for semiconductors.
  void EnableBandStructure() { m_useBandStructureDefault = true; }
  void DisableBandStructure() { m_useBandStructureDefault = false; }

  /// Switch on update of coordinates for null-collision steps (default: off).
  void EnableNullCollisionSteps() { m_useNullCollisionSteps = true; }
  void DisableNullCollisionSteps() { m_useNullCollisionSteps = false; }

  /** Set a (lower) energy threshold for electron transport.
    * This can be useful for simulating delta electrons. */ 
  void SetElectronTransportCut(const double cut) { m_deltaCut = cut; }
  double GetElectronTransportCut() const { return m_deltaCut; }

  /// Set an energy threshold for photon transport.
  void SetPhotonTransportCut(const double cut) { m_gammaCut = cut; }
  double GetPhotonTransportCut() const { return m_gammaCut; }

  /** Set a max. avalanche size (i. e. ignore ionising collisions
      once this size has been reached). */
  void EnableAvalancheSizeLimit(const unsigned int size) { m_sizeCut = size; }
  void DisableAvalancheSizeLimit() { m_sizeCut = 0; }
  int GetAvalancheSizeLimit() const { return m_sizeCut; }

  /// Enable magnetic field in stepping algorithm (default: off).
  void EnableMagneticField() { m_useBfield = true; }
  void DisableMagneticField() { m_useBfield = false; }

  /// Set number of collisions to be skipped for plotting
  void SetCollisionSteps(const unsigned int n) { m_nCollSkip = n; }

  /// Define a time interval (only carriers inside the interval are simulated).
  void SetTimeWindow(const double t0, const double t1);
  void UnsetTimeWindow();

  /// Return the number of electrons and ions in the avalanche.
  void GetAvalancheSize(int& ne, int& ni) const {
    ne = m_nElectrons;
    ni = m_nIons;
  }
  void GetAvalancheSize(int& ne, int& nh, int& ni) const {
    ne = m_nElectrons;
    nh = m_nHoles;
    ni = m_nIons;
  }

  /** Return the number of electron trajectories in the last 
    * simulated avalanche (including captured electrons). */
  unsigned int GetNumberOfElectronEndpoints() const { 
    return m_endpointsElectrons.size(); 
  }
  /** Return the coordinates and time of start and end point of a given
    * electron drift line.
    * \param i index of the drift line
    * \param x0,y0,z0,t0 coordinates and time of the starting point
    * \param x1,y1,z1,t1 coordinates and time of the end point
    * \param e0,e1 initial and final energy
    * \param status status code (see GarfieldConstants.hh) 
    */
  void GetElectronEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                           double& t0, double& e0, double& x1, double& y1,
                           double& z1, double& t1, double& e1,
                           int& status) const;
  void GetElectronEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                           double& t0, double& e0, double& x1, double& y1,
                           double& z1, double& t1, double& e1, double& dx1,
                           double& dy1, double& dz1, int& status) const;
  unsigned int GetNumberOfElectronDriftLinePoints(const unsigned int i = 0) const;
  unsigned int GetNumberOfHoleDriftLinePoints(const unsigned int i = 0) const;
  void GetElectronDriftLinePoint(double& x, double& y, double& z, double& t,
                                 const int ip, const unsigned int iel = 0) const;
  void GetHoleDriftLinePoint(double& x, double& y, double& z, double& t,
                             const int ip, const unsigned int iel = 0) const;

  unsigned int GetNumberOfHoleEndpoints() const { 
    return m_endpointsHoles.size(); 
  }
  void GetHoleEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                       double& t0, double& e0, double& x1, double& y1,
                       double& z1, double& t1, double& e1, int& status) const;

  unsigned int GetNumberOfPhotons() const { return m_photons.size(); }
  // Status codes:
  //   -2: photon absorbed by gas molecule
  void GetPhoton(const unsigned int i, double& e, double& x0, double& y0, double& z0,
                 double& t0, double& x1, double& y1, double& z1, double& t1,
                 int& status) const;

  /** Calculate an electron drift line.
    * \param x0,y0,z0,t0 starting point of the electron
    * \param e0 initial energy of the electron
    * \param dx0,dy0,dz0 initial direction of the electron 
    * If the initial direction is not specified, it is sampled randomly.
    * Secondary electrons are not transported. */
  bool DriftElectron(const double x0, const double y0, const double z0,
                     const double t0, const double e0, const double dx0 = 0.,
                     const double dy0 = 0., const double dz0 = 0.);

  /// Calculate an avalanche initiated by a given electron.
  bool AvalancheElectron(const double x0, const double y0, const double z0,
                         const double t0, const double e0,
                         const double dx0 = 0., const double dy0 = 0.,
                         const double dz0 = 0.);

  /// Set a user handling procedure. This function is called at every step.
  void SetUserHandleStep(void (*f)(double x, double y, double z, double t,
                                   double e, double dx, double dy, double dz,
                                   bool hole));
  void UnsetUserHandleStep();
  /// Set a user handling procedure, to be called at every attachment.
  void SetUserHandleAttachment(void (*f)(double x, double y, double z, double t,
                                         int type, int level, Medium* m));
  void UnsetUserHandleAttachment();
  /// Set a user handling procedure, to be called at every inelastic collision.
  void SetUserHandleInelastic(void (*f)(double x, double y, double z, double t,
                                        int type, int level, Medium* m));
  void UnsetUserHandleInelastic();
  /// Set a user handling procedure, to be called at every ionising collision.
  void SetUserHandleIonisation(void (*f)(double x, double y, double z, double t,
                                         int type, int level, Medium* m));
  void UnsetUserHandleIonisation();

  /// Switch on debugging messages.
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  Sensor* m_sensor;

  struct point {
    double x, y, z, t;
  };

  struct electron {
    // Status
    int status;
    // Electron or hole
    bool hole;
    // Starting point
    double x0, y0, z0, t0;
    // Initial energy
    double e0;
    // Band
    int band;
    // Current position
    double x, y, z, t;
    // Current direction/wavevector
    double kx, ky, kz;
    // Current energy
    double energy;
    // Drift line
    std::vector<point> driftLine;
    double xLast, yLast, zLast;
  };
  std::vector<electron> m_stack;
  std::vector<electron> m_endpointsElectrons;
  std::vector<electron> m_endpointsHoles;

  struct photon {
    // Status
    int status;
    // Energy
    double energy;
    // Starting point
    double x0, y0, z0, t0;
    // End point
    double x1, y1, z1, t1;
  };
  std::vector<photon> m_photons;

  /// Number of electrons produced
  int m_nElectrons;
  /// Number of holes produced
  int m_nHoles;
  /// Number of ions produced
  int m_nIons;

  bool m_usePlotting;
  ViewDrift* m_viewer;
  bool m_plotExcitations;
  bool m_plotIonisations;
  bool m_plotAttachments;

  TH1* m_histElectronEnergy;
  TH1* m_histHoleEnergy;
  TH1* m_histDistance;
  char m_distanceOption;
  std::vector<int> m_distanceHistogramType;

  TH1* m_histSecondary;

  bool m_useSignal;
  bool m_useInducedCharge;
  bool m_useDriftLines;
  bool m_usePhotons;
  bool m_useBandStructureDefault;
  bool m_useNullCollisionSteps;
  bool m_useBfield;

  // Rotation matrices
  double m_rb11, m_rb12, m_rb13;
  double m_rb21, m_rb22, m_rb23;
  double m_rb31, m_rb32, m_rb33;
  double m_rx22, m_rx23, m_rx32, m_rx33;

  // Transport cuts
  double m_deltaCut;
  double m_gammaCut;

  // Max. avalanche size
  unsigned int m_sizeCut;

  unsigned int m_nCollSkip;

  bool m_hasTimeWindow;
  double m_tMin, m_tMax;

  // User procedures
  bool m_hasUserHandleStep;
  bool m_hasUserHandleAttachment;
  bool m_hasUserHandleInelastic;
  bool m_hasUserHandleIonisation;
  void (*m_userHandleStep)(double x, double y, double z, double t, double e,
                         double dx, double dy, double dz, bool hole);
  void (*m_userHandleAttachment)(double x, double y, double z, double t, int type,
                               int level, Medium* m);
  void (*m_userHandleInelastic)(double x, double y, double z, double t, int type,
                              int level, Medium* m);
  void (*m_userHandleIonisation)(double x, double y, double z, double t, int type,
                               int level, Medium* m);

  // Switch on/off debugging messages
  bool m_debug;

  // Electron transport
  bool TransportElectron(const double x0, const double y0, const double z0,
                         const double t0, const double e0, const double dx0,
                         const double dy0, const double dz0, const bool aval,
                         bool hole);
  // Photon transport
  void TransportPhoton(const double x, const double y, const double z,
                       const double t, const double e);

  void ComputeRotationMatrix(const double bx, const double by, const double bz,
                             const double bmag, const double ex,
                             const double ey, const double ez);

  void RotateGlobal2Local(double& dx, double& dy, double& dz) const;
  void RotateLocal2Global(double& dx, double& dy, double& dz) const;

  static bool IsInactive(const electron& item) {

    return item.status == StatusLeftDriftMedium ||
           item.status == StatusBelowTransportCut ||
           item.status == StatusOutsideTimeWindow ||
           item.status == StatusLeftDriftArea ||
           item.status == StatusAttached;
  }
  void AddToEndPoints(const electron& item, const bool hole) {
    if (hole) {
      m_endpointsHoles.push_back(item);
    } else {
      m_endpointsElectrons.push_back(item);
    }
  }
};
}

#endif
