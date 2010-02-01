#ifndef G_COMPONENT_ANSYS123_H
#define G_COMPONENT_ANSYS123_H

#include <RQ_OBJECT.h>

#include "ComponentFieldMap.hh"

namespace Garfield {

  // -------------------------------------------------------------------------------------------------------------------------------------
  class ComponentAnsys123: public ComponentFieldMap { 

    RQ_OBJECT("FieldMapAnsys123")
    
  public:
    // Constructor
    ComponentAnsys123(char* elist = "ELIST.lis",
                      char* nlist = "NLIST.lis",
                      char* mplist = "MPLIST.lis",
                      char* prnsol = "PRNSOL.lis",
                      char* unit = "cm");
    // Destructor
    ~ComponentAnsys123() {}
    
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, 
                       Medium*& m, int& status);
    void ElectricField(const double x, const double y, const double z, 
                       double& ex, double& ey, double& ez, double& v, 
                       Medium*& m, int& status);
    
    bool GetMedium(const double x, const double y, const double z,
                   Medium*& medium);
           
    bool IsInBoundingBox(const double x, const double y, const double z) {
      return x >= xMinBoundingBox && x <= xMaxBoundingBox && 
             y >= yMinBoundingBox && y <= yMaxBoundingBox && 
             z >= zMinBoundingBox && y <= zMaxBoundingBox;
    }
        
  protected:
  
    // Verify periodicities
    void UpdatePeriodicity() {UpdatePeriodicityCommon();}
    
  };
  
}
#endif
