#ifndef Globals_h
#define Globals_h

#include <map>
#include <set>
#include <string>

#include "TH1.h"
#include "TH2.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StSpinPool/StJets/StJet.h"

#include "utils/MultiGraph.h"


enum EBosonType {kWBoson, kZBoson};
enum EAsymType  {kAsymPlain, kAsymSqrtPhys, kAsymSqrtGeom, kAsymSqrtLumi};

inline ostream& operator<<(ostream &os, const EBosonType &v) { return os << (int) v; }
inline istream& operator>>(istream &is, const EBosonType &v) { return is >> v; }


// bsmd indexes needed for calibration
enum {mxBStrips = 18000, mxBSmd  = 2, kBSE  = 0, kBSP  = 1};
enum {mxBtow    = 4800,  mxBTile = 2, kBTow = 0, kBPrs = 1} ; // tower, preshower indexes

//enum {mxBprsCrate=4};
enum {mxBTphiBin   = 120}; // of phi bins for towers, preshower
enum {mxBTetaBin   = 40};  // of eta bins for towers, preshower
enum {mxBetaStrMod = 150}; // of Eta strip in module
enum {mxBMod2Pi    = 60};   // of barrel modules over 2pi strip in module
//enum {mxBphiStrBand=900}; // of Phi strip in Barrel at fixed eta
//enum {mxBXcell=20};       // dimension of 2D eta-phi array of cells,?? mxBYcell=60

enum {mxEtow = 720, mxEtowSec = 12, mxEtowSub = 5, mxEtowEta = 12, mxEsmdPlane = 2, mxEsmdStrip = 288, mxPrs = 3};
enum {mxEtowPhiBin = mxEtowSec * mxEtowSub};

enum {mxTpcSec = 24};

enum ESpinState        {kSPIN_DOWN = -1, kSPIN_NULL = 0, kSPIN_UP = +1};
enum EBeamId           {kBLUE_BEAM = 1,  kYELLOW_BEAM = 2, kUNKNOWN_BEAM};
enum EBeamSpinState    {kBLUE_UP = 4, kBLUE_DOWN = 8, kYELLOW_UP = 1, kYELLOW_DOWN = 2};
enum ESingleSpinState  {kBU_Y0 = 4, kBD_Y0 = 8, kB0_YU = 1, kB0_YD = 2};
enum EDoubleSpinState  {kBU_YU = 5, kBU_YD = 6, kBD_YU = 9, kBD_YD = 10};

typedef std::map<std::string, std::string> Str2StrMap;
typedef std::map<ESingleSpinState, TH1I*> Sss2TH1IMap;
typedef std::map<ESingleSpinState, TH2I*> Sss2TH2IMap;
typedef std::map<EDoubleSpinState, TH1I*> Dss2TH1IMap;
typedef std::map<EDoubleSpinState, TH2I*> Dss2TH2IMap;
typedef std::map<EBeamId, TH1*>  BeamId2TH1Map;
typedef std::map<EBeamId, TH1D*> BeamId2TH1DMap;
typedef std::map<EBeamId, TH2D*> BeamId2TH2DMap;
typedef std::map<EBeamId, rh::MultiGraph*> BeamId2MultGraphMap;

typedef std::set<EBeamId>            BeamIdSet;
typedef BeamIdSet::iterator          BeamIdSetIter;

typedef std::set<ESingleSpinState>   SingleSpinStateSet;
typedef SingleSpinStateSet::iterator SingleSpinStateSetIter;

typedef std::set<EDoubleSpinState>   DoubleSpinStateSet;
typedef DoubleSpinStateSet::iterator DoubleSpinStateSetIter;

inline bool operator==(const StJet& lhs, const StJet& rhs) { return (TLorentzVector) lhs == (TLorentzVector) rhs; }
inline bool operator!=(const StJet& lhs, const StJet& rhs) { return !operator==(lhs,rhs); }
inline bool operator< (const StJet& lhs, const StJet& rhs) { return lhs.E() < rhs.E(); }
inline bool operator> (const StJet& lhs, const StJet& rhs) { return  operator< (rhs,lhs); }
inline bool operator<=(const StJet& lhs, const StJet& rhs) { return !operator> (lhs,rhs); }
inline bool operator>=(const StJet& lhs, const StJet& rhs) { return !operator< (lhs,rhs); }


struct CompareStJets
{
   bool operator()(const StJet* lhs, const StJet* rhs) const { return (*lhs) > (*rhs); }
};


typedef std::set<StJet*, CompareStJets>   StJetPtrSet;         // Jets are sorted according to their E
typedef StJetPtrSet::iterator             StJetPtrSetIter;
typedef StJetPtrSet::const_iterator       StJetPtrSetConstIter;


extern StEmcGeom  *gBTowGeom;
extern StEmcGeom  *mBSmdGeom[mxBSmd];
extern TVector3    gBCalTowerCoords[mxBtow];               // vs. tower ID
extern TVector3    gBSmdStripCoords[mxBSmd][mxBStrips];    // vs. strip ID
extern TVector3    gETowCoords[mxEtowSec *mxEtowSub][mxEtowEta];
extern int         gMapBTowEtaPhiBin2Id[mxBTetaBin * mxBTphiBin];  // vs. (iEta, iPhi)

extern EBeamId            aBeams[2];
extern BeamIdSet          gBeams;
extern ESingleSpinState   aSingleSpinStates[4];
extern SingleSpinStateSet gSingleSpinStateSet;
extern EDoubleSpinState   aDoubleSpinStates[4];
extern DoubleSpinStateSet gDoubleSpinStateSet;


bool        ConvertEtaPhi2Bins(float etaF, float phiF, int &kEta, int &kPhi);
void        PatchToEtaPhi(int patch, int *eta, int *phi);
std::string AsString(EBeamId beamId);
std::string AsString(EBeamId beamId, ESpinState spinState);
std::string AsString(ESingleSpinState dss);
std::string AsString(EDoubleSpinState dss);
ESingleSpinState AsSingleSpinState(EBeamId beamId, ESpinState spinState);

#endif
