// Simple component with electric field given by a user function

#ifndef G_COMPONENT_USER_H
#define G_COMPONENT_USER_H

#include "ComponentBase.hh"

namespace Garfield {

class ComponentUser : public ComponentBase {

  public:
    // Constructor
    ComponentUser();
    // Destructor
    ~ComponentUser() {}

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

    void SetElectricField(void (*f)(const double, const double, const double, double&, double&, double&));
    void SetPotential(void (*f)(const double, const double, const double, double&));
    
    void AddWeightingField(void (*f)(const double, const double, const double, double&, double&, double&),
                           const std::string label);
    

  private:

    // Electric field function
    bool hasField;
    void (*field)(const double, const double, const double, double&, double&, double&);

    // Potential
    bool hasPotential;
    void (*potential)(const double, const double, const double, double&);
    
    int nWeightingFields;
    struct wfield {
      void (*field)(const double, const double, const double, double&, double&, double&);
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
