// Track generation using Heed++

#ifndef G_TRACK_HEED_H
#define G_TRACK_HEED_H

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

class TrackHeed : public Track {

  public:
    // Constructor
    TrackHeed();
    // Destructor
    ~TrackHeed();

    void NewTrack(
            const double x0, const double y0, const double z0, const double t0,
            const double dx0, const double dy0, const double dz0);
    bool GetCluster(double& xcls, double& ycls, double& zcls, double& tcls,
                    int& n, double& e, double& extra);
    bool GetElectron(const int i, double& x, double& y, double& z);                    
   
    void TransportDeltaElectron(const double x0, const double y0, const double z0,
                                const double dx0, const double dy0, const double dz0,
                                const double t0, const double e0, int& nel);
                                
    void SetDatabasePath(const std::string dbpath);
    
    void EnableElectricField();
    void DisableElectricField();
    void EnableMagneticField();
    void DisableMagneticField();
  
  private:

    bool ready;
    bool hasActiveTrack;
  
    double      mediumDensity;
    std::string mediumName;
    
    std::string databasePath;
    bool isPathSet;

    // Primary particle
    HeedParticle* particle;

    // Material properties
    HeedMatterDef* matter;
    GasDef* gas;
    MatterDef* material;
    
    // Photoabsorption cross-sections
    AtomPhotoAbsCS** atPacs;
    MolecPhotoAbsCS** molPacs;
    
    // Transport properties
    EnergyMesh* energyMesh;
    EnTransfCS* transferCs;
    ElElasticScat* elScat;
    ElElasticScatLowSigma* lowSigma;
    PairProd* pairProd;
    HeedDeltaElectronCS* deltaCs;

    HeedChamber* chamber;
    
    bool Setup(Medium* medium);
    bool SetupGas(Medium* medium);
    bool SetupMaterial(Medium* medium);
    bool SetupDelta();

};

}

#endif
