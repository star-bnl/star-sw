// Track generation using Heed++

#ifndef G_TRACK_HEED_H
#define G_TRACK_HEED_H

#include <vector>

#include "Track.hh"

class HeedParticle;
class HeedMatterDef;
class GasDef;
class MatterDef;
class AtomPhotoAbsCS;
class MolecPhotoAbsCS;
class EnergyMesh;
class EnTransfCS;
class ElElasticScat;
class ElElasticScatLowSigma;
class PairProd;
class HeedDeltaElectronCS;

namespace Garfield {

class HeedChamber;
class Medium;

class TrackHeed : public Track {

  public:
    // Constructor
    TrackHeed();
    // Destructor
    ~TrackHeed();

    bool NewTrack(
        const double x0, const double y0, const double z0, const double t0,
        const double dx0, const double dy0, const double dz0);
    bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                    int& n, double& e, double& extra);
    bool GetElectron(const int i, double& x, double& y, double& z,
                     double& t, double& e,
                     double& dx, double& dy, double& dz); 

    double GetClusterDensity();
    double GetStoppingPower();

    void TransportDeltaElectron(
        const double x0, const double y0, const double z0,
        const double t0, const double e0,
        const double dx0, const double dy0, const double dz0,
        int& nel);
   
    void TransportPhoton(
        const double x0, const double y0, const double z0,
        const double t0, const double e0,
        const double dx0, const double dy0, const double dz0,
        int& nel);
 
    // Specify whether the electric and magnetic field should be 
    // taken into account in the stepping algorithm.
    void EnableElectricField();
    void DisableElectricField();
    void EnableMagneticField();
    void DisableMagneticField();

    void EnableDeltaElectronTransport()  {useDelta = true;}
    void DisableDeltaElectronTransport() {useDelta = false;}

    void EnablePhotonReabsorption()  {usePhotonReabsorption = true;}
    void DisablePhotonReabsorption() {usePhotonReabsorption = false;}

    void EnablePhotoAbsorptionCrossSectionOutput()  {usePacsOutput = true;}
    void DisablePhotoAbsorptionCrossSectionOutput() {usePacsOutput = false;} 
    void SetEnergyMesh(const double e0, const double e1,
                       const int nsteps);

  private:

    // Prevent usage of copy constructor and assignment operator
    TrackHeed(const TrackHeed& heed);
    TrackHeed& operator=(const TrackHeed& heed);
 
    bool ready;
    bool hasActiveTrack;
  
    double      mediumDensity;
    std::string mediumName;
  
    bool usePhotonReabsorption;
    bool usePacsOutput;
 
    bool useDelta;
    int nDeltas;
    struct deltaElectron {
      double x, y, z, t;
      double e;
      double dx, dy, dz;
    };
    std::vector<deltaElectron> deltaElectrons;

    // Primary particle
    HeedParticle* particle;

    // Material properties
    HeedMatterDef* matter;
    GasDef* gas;
    MatterDef* material;
 
    // Photoabsorption cross-sections
    AtomPhotoAbsCS** atPacs;
    MolecPhotoAbsCS** molPacs;
 
    // Energy mesh
    double emin, emax;
    int nEnergyIntervals;
    EnergyMesh* energyMesh;
 
    // Cross-sections
    EnTransfCS* transferCs;
    ElElasticScat* elScat;
    ElElasticScatLowSigma* lowSigma;
    PairProd* pairProd;
    HeedDeltaElectronCS* deltaCs;

    HeedChamber* chamber;
    // Bounding box
    double lX, lY, lZ;
    double cX, cY, cZ;

    bool Setup(Medium* medium); 
    bool SetupGas(Medium* medium);
    bool SetupMaterial(Medium* medium);
    bool SetupDelta(const std::string databasePath);

};

}

#endif
