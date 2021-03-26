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

class StHit;

class FwdSystem : public KiTrack::ISectorSystem {
  public:
    static const int sNFwdLayers = 7;
    static const int sNFttLayers = 4;
    static const int sNFstLayers = 3;
    FwdSystem(const int ndisks = FwdSystem::sNFwdLayers) : KiTrack::ISectorSystem(), mNDisks(ndisks){};
    ~FwdSystem(){/* */};
    virtual unsigned int getLayer(int diskid) const throw(KiTrack::OutOfRange) {
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

    void addHit(KiTrack::IHit *hit) { mHits.push_back(hit); }
    // void addFstHit(KiTrack::IHit *hit) { mFstHits.push_back(hit); }

    double mPt, mEta, mPhi;
    int mTid, mQ, mStartVertex;

    std::vector<KiTrack::IHit *> mHits;
    // std::vector<KiTrack::IHit *> mFstHits;
};


/*
 * Note, this class does not follow STAR naming convention.
 * Instead, keep the conventions of KiTrack
 */
class FwdHit : public KiTrack::IHit {
  public:
    FwdHit(unsigned int id, float x, float y, float z, int vid, int tid,
           TMatrixDSym covmat, std::shared_ptr<McTrack> mcTrack = nullptr )
        : KiTrack::IHit() {
        _id = id;
        _x = x;
        _y = y;
        _z = z;
        _tid = tid;
        _vid = vid;
        _mcTrack = mcTrack;
        _hit = 0;
        _covmat.ResizeTo( 3, 3 );
        _covmat = covmat;

        // these are the sector ids mapped to layers
        int map[] = {0, 0, 0, 0, 0, 1, 2, 0, 0, 3, 4, 5, 6}; // ftsref6a

        if (vid > 0)
            _sector = map[vid];
        else {
            _sector = abs(vid); // set directly if you want
            // now set vid back so we retain info on the tru origin of the hit
            _vid = abs(vid) + 9; // we only use this for sTGC only.  Needs to be
                                 // cleaner in future.
        }
    };

    const KiTrack::ISectorSystem *getSectorSystem() const {
        return FwdSystem::sInstance;
    }

    int _tid; // aka ID truth
    int _vid; // volume id
    unsigned int _id; // just a unique id for each hit in this event.
    std::shared_ptr<McTrack> _mcTrack;
    TMatrixDSym _covmat;

    StHit *_hit;
};

using Seed_t = std::vector<KiTrack::IHit *>;

class FwdConnector : public KiTrack::ISectorConnector {
  public:
    FwdConnector(unsigned int distance)
        : _distance(distance) {}
    ~FwdConnector(){/**/};

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
    inline double operator()(Seed_t s) { return double(s.size()) / FwdSystem::sNFttLayers ; } // seeds only use the 4 hits from Ftt
};

struct SeedCompare {
    inline bool operator()(Seed_t trackA, Seed_t trackB) {
        std::map<unsigned int, unsigned int> hit_counts;
        // we are assuming that the same hit can never be used twice on a single
        // track!

        for (auto h : trackA) {
            hit_counts[static_cast<FwdHit *>(h)->_id]++;
        }

        // now look at the other track and see if it has the same hits in it
        for (auto h : trackB) {
            hit_counts[static_cast<FwdHit *>(h)->_id]++;

            // incompatible if they share a single hit
            if (hit_counts[static_cast<FwdHit *>(h)->_id] >= 2) {
                return false;
            }
        }

        // no hits are shared, they are compatible
        return true;
    }
};

#endif
