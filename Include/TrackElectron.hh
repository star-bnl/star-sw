// Ionization calculation based on MIP program (S. Biagi) 

#ifndef G_TRACK_ELECTRON
#define G_TRACK_ELECTRON

#include <string>
#include <vector>

#include "Track.hh"

namespace Garfield {

class TrackElectron : public Track {

  public:
    // Constructor
    TrackElectron();
    // Destructor
    ~TrackElectron();

    void SetParticle(std::string particle);
    
    void NewTrack(const double x0, const double y0, const double z0, 
                  const double t0, 
                  const double dx0, const double dy0, const double dz0);

    bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                    int& ncls, double& ecls, double& extra);
  
    double GetClusterDensity();
    double GetStoppingPower();
        
  private:

    bool ready;

    // Particle coordinates and direction
    double x, y, z, t;
    double dx, dy, dz;
    
    // Parameters in ionization cross-section
    int nComponents;
    struct component {
      double fraction;
      // Dipole moment
      double m2Ion;
      // Constant
      double cIon;
      // Density correction term
      double delta;
      // Opal-Beaty-Peterson splitting factor
      double wSplit;
      // Ionisation threshold
      double ethr;
      // Relative cross-section
      double p;
    };
    std::vector<component> components;

    // Secondary electrons
    int nElectrons;
    struct electron {
      double x, y, z;
      double energy;
    };
    std::vector<electron> electrons;

    // Medium name
    std::string mediumName;
    // Atomic density
    double mediumDensity;
    // Mean free path
    double mfp;

    bool SetupGas(Medium* gas);
    bool UpdateCrossSection();

};

}

#endif
