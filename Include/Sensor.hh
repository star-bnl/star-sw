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
    int GetNumberOfComponents() {return nComponents;}
    // Add an electrode
    void AddElectrode(ComponentBase* comp, std::string label);
    int GetNumberOfElectrodes() {return nElectrodes;}
    // Remove all components, electrodes and reset the sensor
    void Clear();
    
    // Get the drift field at (x, y, z)
    void ElectricField(const double x, const double y, const double z, 
                       double& ex, double& ey, double& ez, double& v, 
                       Medium*& medium, int& status);
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, 
                       Medium*& medium, int& status);

    // Get the magnetic field at (x, y, z)
    void MagneticField(const double x, const double y, const double z,
                       double& bx, double& by, double& bz,
                       int& status);

    // Get the weighting field at (x, y, z)
    void WeightingField(const double x, const double y, const double z,
                        double& wx, double& wy, double& wz,
                        const std::string label);
    // Get the weighting potential at (x, y, z)
    double WeightingPotential(const double x, const double y, const double z,
                              const std::string label);
 
    // Get the medium at (x, y, z)
    bool GetMedium(const double x, const double y, const double z, 
                   Medium*& medium);

    // Set the user area
    bool SetArea();
    bool SetArea(const double xmin, const double ymin, const double zmin,
                 const double xmax, const double ymax, const double zmax);
    // Return the current user area
    bool GetArea(double& xmin, double& ymin, double& zmin,
                 double& xmax, double& ymax, double& zmax);
    // Check if a point is inside the user area
    bool IsInArea(const double x, const double y, const double z);
    
    bool IsWireCrossed(const double x0, const double y0, const double z0,
                       const double x1, const double y1, const double z1,
                       double& xc, double& yc, double& zc);
   
    bool IsInTrapRadius(double x0, double y0, double z0, double& xw, double& yw, double& rw);

    // Return the voltage range
    bool GetVoltageRange(double& vmin, double& vmax);

    // Signal calculation
    void NewSignal() {++nEvents;}
    // Reset signals and induced charges of all electrodes
    void ClearSignal();
    void AddSignal(const int q, const double t, const double dt,
                   const double x,  const double y,  const double z,
                   const double vx, const double vy, const double vz);
    void AddInducedCharge(const int q, 
                          const double x0, const double y0, const double z0,
                          const double x1, const double y1, const double z1);
    // Set/get the time window and binning for the signal calculation
    void SetTimeWindow(const double tstart, const double tstep, 
                       const int nsteps);
    void GetTimeWindow(double& tstart, double& tstep, int& nsteps) {
      tstart = tStart; tstep = tStep; nsteps = nTimeBins;
    }
    double GetSignal(const std::string label, const int bin);
    double GetElectronSignal(const std::string label, const int bin);
    double GetIonSignal(const std::string label, const int bin);
    double GetInducedCharge(const std::string label);
    void SetTransferFunction(double (*f)(double t));
    bool ConvoluteSignal();
    bool IntegrateSignal();
    void SetNoiseFunction(double (*f)(double t));
    void AddNoise();
    bool ComputeThresholdCrossings(const double thr, 
                                   const std::string label, int& n);
    int  GetNumberOfThresholdCrossings() {return nThresholdCrossings;}
    bool GetThresholdCrossing(const int i, 
                              double& time, double& level, bool& rise); 

    // Switch on/off debugging messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}    

  private:

    std::string className;

    // Components
    int nComponents;
    struct component {
      ComponentBase* comp;
    };
    std::vector<component> components;
    int lastComponent;
    
    // Electrodes
    int nElectrodes;
    struct electrode {
      ComponentBase* comp;
      std::string label;
      std::vector<double> signal;
      std::vector<double> electronsignal;
      std::vector<double> ionsignal;
      double charge;
    };
    std::vector<electrode> electrodes;

    // Time window for signals
    int nTimeBins;
    double tStart, tStep;
    int nEvents;
    static double signalConversion;
   
    // Transfer function
    bool hasTransferFunction;
    double (*fTransfer) (double t);

    // Noise
    bool hasNoiseFunction;
    double (*fNoise) (double t);

    int nThresholdCrossings;
    struct thresholdCrossing {
      double time;
      bool rise;
    };
    std::vector<thresholdCrossing> thresholdCrossings;
    double thresholdLevel;

    // Bounding box
    double xMin, yMin, zMin;
    double xMax, yMax, zMax;
    // User bounds
    bool hasUserArea;
    double xMinUser, yMinUser, zMinUser;
    double xMaxUser, yMaxUser, zMaxUser;

    // Switch on/off debugging messages
    bool debug;

    // Return the current sensor size
    bool GetBoundingBox(double& xmin, double& ymin, double& zmin,
                        double& xmax, double& ymax, double& zmax);

};

}

#endif
