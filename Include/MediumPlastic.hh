// Plastic medium

#ifndef G_MEDIUM_PLASTIC_H
#define G_MEDIUM_PLASTIC_H

#include "Medium.hh"

namespace Garfield {

class MediumPlastic : public Medium {
    
  public:
    // Constructor
    MediumPlastic() : Medium() {
      className = "MediumPlastic";
      name = "Plastic";
    }
    // Destructor
    ~MediumPlastic() {}
    
    void EnableDrift() {}
    void EnableIonisation() {}    

};

}

#endif
