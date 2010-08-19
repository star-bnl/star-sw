// Track generation using Heed++

#ifndef G_TRACK_HEED_H
#define G_TRACK_HEED_H

#ifdef __MAKECINT__
#define DICT_SKIP_HEED
#endif

#ifndef DICT_SKIP_HEED
#include "wcpplib/geometry/box.h"
#include "wcpplib/matter/MatterDef.h"

#include "heed++/code/ElElasticScat.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/HeedCondElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/HeedMatterDef.h"
#include "heed++/code/HeedParticle.h"
#include "heed++/code/PhotoAbsCSLib.h"

#endif

#include "Track.hh"

namespace Garfield {

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

#ifndef DICT_SKIP_HEED 
    // Primary particle
    HeedParticle particle;
    AbsListNode<ActivePtr<gparticle> >* node;

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

    class Chamber : public sh_manip_absvol, public box,
                    public EnTransfCSType, public HeedDeltaElectronCSType,
                    public SensitiveVolume {
                    
      public:
        Chamber() {};
        Chamber(const abssyscoor& fcsys, const EnTransfCSType etcst,
                const HeedDeltaElectronCSType hdecst);
        macro_copy_total(Chamber);
        virtual absvol* Gavol() const {return (box*) this;}
      
      protected:
        virtual void get_components(ActivePtr<absref_transmit>& aref_tran) {
          sh_manip_absvol::get_components(aref_tran);
        }
        
    };
    
    Chamber chamber;
#endif

    bool Setup(Medium* medium);
    bool SetupGas(Medium* medium);
    bool SetupMaterial(Medium* medium);
    bool SetupDelta();

};

}

#ifdef DICT_SKIP_HEED
#undef DICT_SKIP_HEED
#endif

#endif
