// Calculate electron avalanches using microscopic tracking

#ifndef G_AVALANCHE_MICROSCOPIC_H
#define G_AVALANCHE_MICROSCOPIC_H

#include <vector>
#include <string>

#include <TH1.h>

#include "Sensor.hh"
#include "ViewDrift.hh"

namespace Garfield {

class AvalancheMicroscopic {

 public:
  // Constructor
  AvalancheMicroscopic();
  // Destructor
  ~AvalancheMicroscopic() {}

  void SetSensor(Sensor* sensor);

  // Switch on/off drift line plotting
  void EnablePlotting(ViewDrift* view);
  void DisablePlotting();
  void EnableExcitationMarkers() { m_plotExcitations = true; }
  void DisableExcitationMarkers() { m_plotExcitations = false; }
  void EnableIonisationMarkers() { m_plotIonisations = true; }
  void DisableIonisationMarkers() { m_plotIonisations = false; }
  void EnableAttachmentMarkers() { m_plotAttachments = true; }
  void DisableAttachmentMarkers() { m_plotAttachments = false; }

  // Switch on/off calculation of induced currents
  void EnableSignalCalculation() { m_useSignal = true; }
  void DisableSignalCalculation() { m_useSignal = false; }

  // Switch on/off calculation of total induced charge
  void EnableInducedChargeCalculation() { m_useInducedCharge = true; }
  void DisableInducedChargeCalculation() { m_useInducedCharge = false; }

  // Switch on/off filling histograms for energy distribution
  void EnableElectronEnergyHistogramming(TH1* histo);
  void DisableElectronEnergyHistogramming();
  void EnableHoleEnergyHistogramming(TH1* histo);
  void DisableHoleEnergyHistogramming();

  // Switch on/off filling histograms for distance distribution
  void SetDistanceHistogram(TH1* histo, const char opt = 'r');
  void EnableDistanceHistogramming(const int type);
  void DisableDistanceHistogramming(const int type);
  void DisableDistanceHistogramming();

  void EnableSecondaryEnergyHistogramming(TH1* histo);
  void DisableSecondaryEnergyHistogramming();

  // Switch on/off storage of drift lines
  void EnableDriftLines() { m_useDriftLines = true; }
  void DisableDriftLines() { m_useDriftLines = false; }

  // Switch on/off photon transport
  void EnablePhotonTransport() { m_usePhotons = true; }
  void DisablePhotonTransport() { m_usePhotons = false; }

  // Switch on/off stepping according to band structure E(k)
  void EnableBandStructure() { m_useBandStructureDefault = true; }
  void DisableBandStructure() { m_useBandStructureDefault = false; }

  // Switch on/off update of coordinates for null-collision steps
  void EnableNullCollisionSteps() { m_useNullCollisionSteps = true; }
  void DisableNullCollisionSteps() { m_useNullCollisionSteps = false; }

  // Set/get energy threshold for electron transport
  // (useful for delta electrons)
  void SetElectronTransportCut(const double cut) { m_deltaCut = cut; }
  double GetElectronTransportCut() const { return m_deltaCut; }

  // Set/get energy threshold for photon transport
  void SetPhotonTransportCut(const double cut) { m_gammaCut = cut; }
  double GetPhotonTransportCut() const { return m_gammaCut; }

  // Enable/disable max. avalanche size
  void EnableAvalancheSizeLimit(const int size) { m_sizeCut = size; }
  void DisableAvalancheSizeLimit() { m_sizeCut = -1; }
  int GetAvalancheSizeLimit() const { return m_sizeCut; }

  // Enable/disable magnetic field in stepping algorithm
  void EnableMagneticField() { m_useBfield = true; }
  void DisableMagneticField() { m_useBfield = false; }

  // Set number of collisions to be skipped for plotting
  void SetCollisionSteps(const int n);

  void SetTimeWindow(const double t0, const double t1);
  void UnsetTimeWindow();

  void GetAvalancheSize(int& ne, int& ni) const {
    ne = m_nElectrons;
    ni = m_nIons;
  }
  void GetAvalancheSize(int& ne, int& nh, int& ni) const {
    ne = m_nElectrons;
    nh = m_nHoles;
    ni = m_nIons;
  }

  int GetNumberOfElectronEndpoints() const { return m_nElectronEndpoints; }
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

  int GetNumberOfHoleEndpoints() const { return m_nHoleEndpoints; }
  void GetHoleEndpoint(const unsigned int i, double& x0, double& y0, double& z0,
                       double& t0, double& e0, double& x1, double& y1,
                       double& z1, double& t1, double& e1, int& status) const;

  int GetNumberOfPhotons() const { return m_usePhotons ? m_nPhotons : 0; }
  // Status codes:
  //   -2: photon absorbed by gas molecule
  void GetPhoton(const unsigned int i, double& e, double& x0, double& y0, double& z0,
                 double& t0, double& x1, double& y1, double& z1, double& t1,
                 int& status) const;

  // Calculate an electron drift line for an electron with given
  // initial coordinates, energy and direction (random if not specified)
  // Secondary electrons are not transported
  bool DriftElectron(const double x0, const double y0, const double z0,
                     const double t0, const double e0, const double dx0 = 0.,
                     const double dy0 = 0., const double dz0 = 0.);

  // Calculate an avalanche initiated by an electron with given
  // initial coordinates, energy and direction (random if not specified)
  bool AvalancheElectron(const double x0, const double y0, const double z0,
                         const double t0, const double e0,
                         const double dx0 = 0., const double dy0 = 0.,
                         const double dz0 = 0.);

  // Set user handling procedures
  void SetUserHandleStep(void (*f)(double x, double y, double z, double t,
                                   double e, double dx, double dy, double dz,
                                   bool hole));
  void UnsetUserHandleStep();
  void SetUserHandleAttachment(void (*f)(double x, double y, double z, double t,
                                         int type, int level, Medium* m));
  void UnsetUserHandleAttachment();
  void SetUserHandleInelastic(void (*f)(double x, double y, double z, double t,
                                        int type, int level, Medium* m));
  void UnsetUserHandleInelastic();
  void SetUserHandleIonisation(void (*f)(double x, double y, double z, double t,
                                         int type, int level, Medium* m));
  void UnsetUserHandleIonisation();

  // Switch on/off debugging messages
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

  unsigned int m_nPhotons;
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

  // Number of electrons, holes and ions produced
  int m_nElectrons;
  int m_nHoles;
  int m_nIons;
  // Number of electron/hole trajectories
  // (including captured electrons)
  unsigned int m_nElectronEndpoints;
  unsigned int m_nHoleEndpoints;

  bool m_usePlotting;
  ViewDrift* m_viewer;
  bool m_plotExcitations;
  bool m_plotIonisations;
  bool m_plotAttachments;

  TH1* m_histElectronEnergy;
  TH1* m_histHoleEnergy;
  bool m_hasElectronEnergyHistogram;
  bool m_hasHoleEnergyHistogram;
  TH1* m_histDistance;
  bool m_hasDistanceHistogram;
  char m_distanceOption;
  std::vector<int> m_distanceHistogramType;

  TH1* m_histSecondary;
  bool m_hasSecondaryHistogram;

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
  int m_sizeCut;

  int m_nCollSkip;

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

  void RotateGlobal2Local(double& dx, double& dy, double& dz);
  void RotateLocal2Global(double& dx, double& dy, double& dz);
};
}

#endif
