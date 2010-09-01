#ifndef G_OPTICAL_DATA_H
#define G_OPTICAL_DATA_H

#include <string>
#include <vector>

namespace Garfield {

class OpticalData {

  public:
    // Constructor
    OpticalData();
    // Destructor
    ~OpticalData();

    bool IsAvailable(const std::string material) const;
    
    bool GetPhotoabsorptionCrossSection(const std::string material,
                                        const double e, 
                                        double& cs, double& eta);

    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  private:

    std::vector<double> energy;
    std::vector<double> eps1;
    std::vector<double> eps2;
    std::vector<double> pacs;

    double emin, emax;

    std::vector<double> energyIon;
    std::vector<double> yieldIon;
    double ionmin, ionmax;

    bool debug;

    bool PhotoAbsorptionCsArgon(const double e, double& cs, double& eta);
    bool PhotoAbsorptionCsMethane(const double e, double& cs, double& eta);
    bool PhotoAbsorptionCsAcetylene(const double e, double& cs, double& eta);
    void Isobutane();
 
};

}

#endif
