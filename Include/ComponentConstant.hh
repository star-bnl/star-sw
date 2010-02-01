// Component with constant electric field

#ifndef G_COMPONENT_CONSTANT_H
#define G_COMPONENT_CONSTANT_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentConstant : public ComponentBase {
  
  public:
    // Constructor
    ComponentConstant();
    // Destructor
    ~ComponentConstant() {}
    
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, 
                       Medium*& m, int& status);
    void ElectricField(const double x, const double y, const double z, 
                       double& ex, double& ey, double& ez, double& v, 
                       Medium*& m, int& status);
    bool GetVoltageRange(double& vmin, double& vmax);
    void WeightingField(const double x, const double y, const double z,
                        double& wx, double& wy, double& wz,
                        const std::string label);

    void SetElectricField(const double ex, const double ey, const double ez);
    void SetPotential(const double x, const double y, const double z, 
                      const double v = 0.);
    
    void AddWeightingField(const double wx, const double wy, const double wz,
                           const std::string label);

  private:
  
    // Electric field
    double fx, fy, fz;
    
    // Potential
    bool hasPotential;
    // Point where potential was specified
    double x0, y0, z0;
    // Potential at this point
    double v0;
    
    // Weighting field(s)
    int nWeightingFields;
    struct wfield {
      double wx, wy, wz;
      std::string label;
    };
    std::vector<wfield> wfields;

    // Geometry checks
    bool CheckSolidType(Solid* s);
    void CheckBoundaryConditionType(int& bctype, double& bcval);
    // Reset the component
    void Reset();
    // Verify periodicities
    void UpdatePeriodicity();

};

}
#endif
