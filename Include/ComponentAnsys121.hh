#ifndef G_COMPONENT_ANSYS121_H
#define G_COMPONENT_ANSYS121_H

#include <RQ_OBJECT.h>

#include "ComponentFieldMap.hh"

namespace Garfield {

  // -------------------------------------------------------------------------------------------------------------------------------------
class ComponentAnsys121: public ComponentFieldMap { 

    RQ_OBJECT("ComponentAnsys121")
    
  public:
    // Constructor  
    ComponentAnsys121(char* elist = "ELIST.lis",
                      char* nlist = "NLIST.lis",
                      char* mplist = "MPLIST.lis",
                      char* prnsol = "PRNSOL.lis",
                      char* unit = "cm");
    // Destructor                      
    ~ComponentAnsys121() {}
    
    bool GetMedium(const double x, const double y, const double z, 
                   Medium*& medium);
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, 
                       Medium*& m, int& status);
    void ElectricField(const double x, const double y, const double z, 
                       double& ex, double& ey, double& ez, double& v, 
                       Medium*& m, int& status);
    
    // Range
    bool IsInBoundingBox(const double x, const double y, const double z) {
      return x >= xMinBoundingBox && x <= xMaxBoundingBox && 
             y >= yMinBoundingBox && y <= yMaxBoundingBox;
    }
      
  protected:
  
    // Verify periodicities
    void UpdatePeriodicity();

};

}

#endif
