#ifndef FwdHit_h
#define FwdHit_h

#include "KiTrack/IHit.h"
#include "KiTrack/ISectorConnector.h"
#include "KiTrack/ISectorSystem.h"
#include "KiTrack/KiTrackExceptions.h"

#include <memory>
#include <set>
#include <string.h>
#include <vector>
#include <unordered_set>

#include "StEvent/StEnumerations.h"
#include "StEvent/StFstConsts.h"

class StHit;

class FwdSystem : public KiTrack::ISectorSystem {
  public:
    static const int sNFwdLayers = 7;
    static const int sNFttLayers = 4;
    static const int sNFstLayers = 3;
    FwdSystem(const int ndisks = FwdSystem::sNFwdLayers) : KiTrack::ISectorSystem(), mNDisks(ndisks){};
    ~FwdSystem() = default;
    virtual unsigned int getLayer(int diskid) const {
        return diskid;
    }

    int mNDisks;
    std::string getInfoOnSector(int sec) const { return "NOOP"; }
    static FwdSystem *sInstance; // setup and torn down by StFwdTrackMaker
};

//_____________________________________________________________________________________________

// small class to store Mc Track information
class McTrack {
  public:
    McTrack() {
        set( -999, -999, -999, 0, -1 );
    }
    McTrack(double pt, double eta = -999, double phi = -999, int q = 0,
            int start_vertex = -1) {
        set( pt, eta, phi, q, start_vertex );
    }

    void set(double pt, double eta = -999, double phi = -999, int q = 0, int start_vertex = -1){
        mPt = pt;
        mEta = eta;
        mPhi = phi;
        mQ = q;
        mStartVertex = start_vertex;
    }

    void addFttHit(KiTrack::IHit *hit) { mFttHits.push_back(hit); }
    void addFstHit(KiTrack::IHit *hit) { mFstHits.push_back(hit); }

    double mPt, mEta, mPhi;
    int mTid, mQ, mStartVertex;

    std::vector<KiTrack::IHit *> mFttHits;
    std::vector<KiTrack::IHit *> mFstHits;
};


/*
 * Note, this class does not follow STAR naming convention.
 * Instead, keep the conventions of KiTrack
 */
class FwdHit : public KiTrack::IHit {
  public:
  // Default ctor
    FwdHit() = default;
    FwdHit(unsigned int id, float x, float y, float z, int vid, int detid, int tid,
        const TMatrixDSym &covmat, std::shared_ptr<McTrack> mcTrack, StHit *hit = nullptr, unsigned int genfit_plane_index = 0)
        : KiTrack::IHit(), _covmat(covmat) {
        _id = id;
        _x = x;
        _y = y;
        _z = z;
        _detid = detid;
        _tid = tid;
        _vid = vid;
        _mcTrack = mcTrack;
        _hit = hit;
        _genfit_plane_index = genfit_plane_index;

        // these are the sector ids mapped to layers
        static const std::array<int,14> sector_map = {0, 0, 0, 0, 0, 1, 2, 0, 0, 3, 4, 5, 6, 7}; // ftsref6a

        if (vid > 0)
            _sector = sector_map[vid];
        else {
            _sector = abs(vid); // set directly if you want
            // now set vid back so we retain info on the tru origin of the hit
            _vid = abs(vid) + 9; // we only use this for sTGC only.  Needs to be
                                 // cleaner in future.
        }
    };

    static int fstGlobalSensorIndex( int disk, int wedge, int sensor ) {
        return (disk * kFstNumWedgePerDisk * kFstNumSensorsPerWedge) + (wedge * kFstNumSensorsPerWedge) + sensor;
    }
    static int fstSensorWedgeDiskFromGlobalIndex( int globalIndex, int &disk, int &wedge, int &sensor ) {
        disk   = globalIndex / (kFstNumWedgePerDisk * kFstNumSensorsPerWedge);
        wedge  = (globalIndex / kFstNumSensorsPerWedge) % kFstNumWedgePerDisk;
        sensor = globalIndex % kFstNumSensorsPerWedge;
        return 0;
    }

    // Set basic props for e.g. Primary Vertex type hits
    void setXYZDetId( float x, float y, float z, int detid ){
        _x = x;
        _y = y;
        _z = z;
        _detid = detid;
    }

    bool isFst() const { return _detid == kFstId; } 
    bool isFtt() const { return _detid == kFttId; } 
    bool isEpd() const { return _detid == kFcsPresId; } 
    bool isPV() const { return _detid == kTpcId; }

    std::shared_ptr<McTrack> getMcTrack() const { return _mcTrack; }

    const KiTrack::ISectorSystem *getSectorSystem() const {
        return FwdSystem::sInstance;
    }

    std::string Print() const {
        return "FwdHit: \n" + std::to_string(_id) + " x: " + std::to_string(_x) +
               " y: " + std::to_string(_y) + " z: " + std::to_string(_z) +
               " detid: " + std::to_string(_detid) + " tid: " + std::to_string(_tid) +
               " vid: " + std::to_string(_vid) + " sector: " + std::to_string(_sector) + 
               // output covmat as a 3x3 matrix
                " covmat: \n" + std::to_string(_covmat(0,0)) + " " + std::to_string(_covmat(0,1)) + " " + std::to_string(_covmat(0,2)) + " \n" +
                std::to_string(_covmat(1,0)) + " " + std::to_string(_covmat(1,1)) + " " + std::to_string(_covmat(1,2)) + " \n" +
                std::to_string(_covmat(2,0)) + " " + std::to_string(_covmat(2,1)) + " " + std::to_string(_covmat(2,2));
    }

    void setSector( int s ){ _sector = s; }
    int getTrackId() const { return _tid;}
    int _tid = -1; // aka ID truth
    int _vid = -1; // volume id
    int _detid = -1; // detector id
    unsigned int _id = 0; // just a unique id for each hit in this event.
    std::shared_ptr<McTrack> _mcTrack;
    TMatrixDSym _covmat = TMatrixDSym(3);

    StHit *_hit = nullptr;
    unsigned int _genfit_plane_index = 0; //NOT used for isPV==True, since that is an abs measurement
};

// Track Seed typdef
typedef std::vector<KiTrack::IHit *> Seed_t;

class FwdConnector : public KiTrack::ISectorConnector {
  public:
    FwdConnector(unsigned int distance)
        : _distance(distance) {}
    ~FwdConnector() = default;

    // Return the possible sectors (layers) given current
    virtual std::set<int> getTargetSectors(int disk) {

        std::set<int> r;

        if (disk > 0 && _distance >= 1)
            r.insert(disk - 1);

        if (disk > 1 && _distance >= 2)
            r.insert(disk - 2);

        if (disk > 2 && _distance >= 3)
            r.insert(disk - 3);

        if (disk > 3 && _distance >= 4)
            r.insert(disk - 4);

        return r;
    };

  private:
  protected:
    // const FwdSystem _system; // numbering system
    unsigned int _distance;  // number of layers forward to search
};                           // FwdConnector

struct SeedQual {
    inline double operator()(Seed_t s) { return double(s.size()) / FwdSystem::sNFstLayers ; } // seeds only use the 3 hits from Fst
};

struct SeedCompatible {
    inline bool operator()(Seed_t trackA, Seed_t trackB) {
        // we are assuming that the same hit can never be used twice on a single
        // track!

        std::unordered_set<unsigned int> ids;
        for (auto h : trackA) {
            ids.insert(static_cast<FwdHit*>(h)->_id);
        }

        // now look at the other track and see if it has the same hits in it
        for (auto h : trackB){
            if (ids.count(static_cast<FwdHit*>(h)->_id))
                return false;
        }

        // no hits are shared, they are compatible
        return true;
    }
};

#endif
