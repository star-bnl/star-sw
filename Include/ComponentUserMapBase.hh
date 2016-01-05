#ifndef COMPONENT_USER_MAP_BASE_H
#define COMPONENT_USER_MAP_BASE_H

#include <iostream>
#include "ComponentBase.hh"

namespace Garfield
{

class ComponentUserMapBase : public ComponentBase
{
 public:
	ComponentUserMapBase();
	virtual ~ComponentUserMapBase();
  
  Medium* GetMedium(const double x, const double y, const double z);
	
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, Medium*& m, int& status);
  void ElectricField(const double x, const double y, const double z, double& ex,
                     double& ey, double& ez, double& v, Medium*& m,
                     int& status);
  bool GetVoltageRange(double& vmin, double& vmax){ vmin = vmax = 0.; return false; }
  
  void WeightingField(const double x, const double y, const double z,
                      double& wx, double& wy, double& wz,
                      const std::string& label);
  double WeightingPotential(const double x, const double y, const double z,
                            const std::string& label);

  // This method should be overloaded in derived classes to implement the 
  // coordinate mapping. For example, when ComponentUserMapBase::ElectricField 
  // calls this method, the parameters p1, p2, and p3 will contain the global 
  // coordinates (x, y, z). MapCoordinates should then map them to local 
  // coordinates. It should also set pComponent to the component that will be 
  // used with these coordinates. In order to properly unmap the field, this 
  // method should also set the local base vectors u1, u2 and u3. Their initial 
  // values will be u1 = (1, 0, 0), u2 = (0, 1, 0), and u3 = (0, 0, 1).
  virtual void MapCoordinates(double& p1 , double& p2 , double& p3 ,
                              double& u1x, double& u2x, double& u3x,
                              double& u1y, double& u2y, double& u3y,
                              double& u1z, double& u2z, double& u3z,
                              ComponentBase*& pComponent) = 0;
                              
  // Overloading this method is optional. It enables the use of a separate 
  // coordinate mapping for weighting fields. And allows the user to parse the
  // label and change it.
  virtual void MapCoordinates(double& p1 , double& p2 , double& p3 ,
                              double& u1x, double& u2x, double& u3x,
                              double& u1y, double& u2y, double& u3y,
                              double& u1z, double& u2z, double& u3z,
                              ComponentBase*& pComponent, 
                              std::string& label){
    (void)label; // Suppress compiler warning
    MapCoordinates(p1, p2, p3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, 
                   pComponent);
  }

 protected:
  
  void Reset(){}
  void UpdatePeriodicity();
  
  void UnmapField(const double e1 , const double e2 , const double e3 ,
                  const double u1x, const double u2x, const double u3x,
                  const double u1y, const double u2y, const double u3y,
                  const double u1z, const double u2z, const double u3z,
                  double& ex, double& ey, double& ez) const
  {
    ex = u1x*e1 + u2x*e2 + u3x*e3;
    ey = u1y*e1 + u2y*e2 + u3y*e3;
    ez = u1z*e1 + u2z*e2 + u3z*e3;
  }
};

}

#endif //COMPONENT_USER_MAP_BASE_H

