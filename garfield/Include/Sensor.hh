// Sensor

#ifndef G_SENSOR_H
#define G_SENSOR_H

#include <vector>

#include "ComponentBase.hh"

namespace Garfield {

class Sensor {

 public:
  // Constructor
  Sensor();
  // Destructor
  ~Sensor() {}

  // Add a component
  void AddComponent(ComponentBase* comp);
  unsigned int GetNumberOfComponents() const { return m_components.size(); }
  virtual ComponentBase* GetComponent(const unsigned int componentNumber);

  // Add an electrode
  void AddElectrode(ComponentBase* comp, const std::string& label);
  unsigned int GetNumberOfElectrodes() const { return m_electrodes.size(); }
  // Remove all components, electrodes and reset the sensor
  void Clear();

  // Get the drift field at (x, y, z)
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& medium,
                     int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& medium, int& status);

  // Get the magnetic field at (x, y, z)
  void MagneticField(const double x, const double y, const double z, double& bx,
                     double& by, double& bz, int& status);

  // Get the weighting field at (x, y, z)
  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label);
  // Get the weighting potential at (x, y, z)
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label);

  // Get the medium at (x, y, z)
  bool GetMedium(const double x, const double y, const double z,
                 Medium*& medium);

  // Set the user area
  bool SetArea();
  bool SetArea(const double xmin, const double ymin, const double zmin,
               const double xmax, const double ymax, const double zmax);
  // Return the current user area
  bool GetArea(double& xmin, double& ymin, double& zmin, double& xmax,
               double& ymax, double& zmax);
  // Check if a point is inside the user area
  bool IsInArea(const double x, const double y, const double z);

  bool IsWireCrossed(const double x0, const double y0, const double z0,
                     const double x1, const double y1, const double z1,
                     double& xc, double& yc, double& zc);

  bool IsInTrapRadius(const double q0, const double x0, const double y0, 
                      const double z0, double& xw, double& yw, double& rw);

  // Return the voltage range
  bool GetVoltageRange(double& vmin, double& vmax);

  // Signal calculation
  void NewSignal() { ++m_nEvents; }
  // Reset signals and induced charges of all electrodes
  void ClearSignal();
  void AddSignal(const double q, const double t, const double dt,
                 const double x, const double y, const double z,
                 const double vx, const double vy, const double vz);
  void AddInducedCharge(const double q, const double x0, const double y0,
                        const double z0, const double x1, const double y1,
                        const double z1);
  // Set/get the time window and binning for the signal calculation
  void SetTimeWindow(const double tstart, const double tstep, 
                     const unsigned int nsteps);
  void GetTimeWindow(double& tstart, double& tstep, unsigned int& nsteps) {
    tstart = m_tStart;
    tstep = m_tStep;
    nsteps = m_nTimeBins;
  }
  double GetSignal(const std::string& label, const unsigned int bin);
  double GetElectronSignal(const std::string& label, const unsigned int bin);
  double GetIonSignal(const std::string& label, const unsigned int bin);
  double GetInducedCharge(const std::string& label);
  void SetTransferFunction(double (*f)(double t));
  void SetTransferFunction(const std::vector<double>& times,
                           const std::vector<double>& values);
  double GetTransferFunction(const double t);
  bool ConvoluteSignal();
  bool IntegrateSignal();
  void SetNoiseFunction(double (*f)(double t));
  void AddNoise();
  bool ComputeThresholdCrossings(const double thr, const std::string& label,
                                 int& n);
  unsigned int GetNumberOfThresholdCrossings() const { 
    return m_thresholdCrossings.size(); 
  }
  bool GetThresholdCrossing(const unsigned int i, double& time, double& level,
                            bool& rise) const;

  // Switch on/off debugging messages
  void EnableDebugging() { m_debug = true; }
  void DisableDebugging() { m_debug = false; }

 private:
  std::string m_className;

  // Components
  struct component {
    ComponentBase* comp;
  };
  std::vector<component> m_components;
  int m_lastComponent;

  // Electrodes
  struct electrode {
    ComponentBase* comp;
    std::string label;
    std::vector<double> signal;
    std::vector<double> electronsignal;
    std::vector<double> ionsignal;
    double charge;
  };
  std::vector<electrode> m_electrodes;

  // Time window for signals
  unsigned int m_nTimeBins;
  double m_tStart, m_tStep;
  unsigned int m_nEvents;
  static double m_signalConversion;

  // Transfer function
  bool m_hasTransferFunction;
  double (*m_fTransfer)(double t);
  std::vector<double> m_transferFunctionTimes;
  std::vector<double> m_transferFunctionValues;

  // Noise
  bool m_hasNoiseFunction;
  double (*m_fNoise)(double t);

  struct thresholdCrossing {
    double time;
    bool rise;
  };
  std::vector<thresholdCrossing> m_thresholdCrossings;
  double m_thresholdLevel;

  // User bounding box
  bool m_hasUserArea;
  double m_xMinUser, m_yMinUser, m_zMinUser;
  double m_xMaxUser, m_yMaxUser, m_zMaxUser;

  // Switch on/off debugging messages
  bool m_debug;

  // Return the current sensor size
  bool GetBoundingBox(double& xmin, double& ymin, double& zmin, double& xmax,
                      double& ymax, double& zmax);

  double InterpolateTransferFunctionTable(double t);
};
}

#endif
