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
    double WeightingPotential(const double x, const double y, const double z,
                              const std::string label);

    void SetElectricField(const double ex, const double ey, const double ez);
    void SetPotential(const double x, const double y, const double z, 
                      const double v = 0.);
    
    void SetWeightingField(const double wx, const double wy, const double wz,
                           const std::string label);
    void SetWeightingPotential(const double x, const double y, const double z,
                               const double v = 0.);

  private:
  
    // Electric field
    double fx, fy, fz;
    
    // Potential
    bool hasPotential;
    // Point where potential was specified
    double x0, y0, z0;
    // Potential at this point
    double v0;
    
    // Weighting field
    bool hasWeightingField;
    std::string wfield;
    double fwx, fwy, fwz;
    bool hasWeightingPotential;
    // Point where the weighting potential was specified
    double wx0, wy0, wz0;
    // Weighting potential at this point
    double w0;

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
