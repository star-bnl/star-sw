/******************* NOTE ********************************************
 * This file should never be included directly. It is included from  * 
 * /DAQ/include/daqFormats.h after the necessary structs are defined *
 *********************************************************************/

#ifndef _HLT_FORMATS_H
#define _HLT_FORMATS_H

// #define HLT_GL3_VERSION 0x20100107
// #define HLT_GL3_VERSION 0x20110114 // add di-electron selection method bits
// #define HLT_GL3_VERSION 0x20130117 // optimazed mamoty layout
// #define HLT_GL3_VERSION 0x20140522 // add MTD data structure
// #define HLT_GL3_VERSION 0x20160120 // add MTDQuarkonium
// #define HLT_GL3_VERSION 0x20160210 // add isTrigger to hlt_MtdHit
// #define HLT_GL3_VERSION 0x20180607 // add bField in HLT_EVE
// #define HLT_GL3_VERSION 0x20190602 // add bunch_id in HLT_EVE
// #define HLT_GL3_VERSION 0x20190603 // add earliest TACs for BBC/VPD/EPD to HLT_EVE
// #define HLT_GL3_VERSION 0x20191201 // add ETOF hits
#define HLT_GL3_VERSION 0x20200107 // add ETOF time of flight under pion assumption

struct hlt_track {
    int            id ;         //primary key
    unsigned short flag ;       // Primaries flag = 1, Secondaries flag=0
    char           innerMostRow ;
    char           outerMostRow ;
    unsigned char  nHits ;      // Number of points assigned to that track
    char           reserved ; 
    unsigned char  ndedx;       // nr of clusters contributing to the dedx value
    char           q ;          // charge
    float          chi2[2];     // chi squared of the momentum fit
    float          dedx;        // dE/dx information
    float          pt ;         // pt (transverse momentum) at (r,phi,z)
    float          phi0;        // azimuthal angle of the first point
    float          psi ;        // azimuthal angle of the momentum at (r,..
    float          r0 ;         // r (in cyl. coord.) for the first point
    float          tanl;        // tg of the dip angle at (r,phi,z)
    float          z0 ;         // z coordinate of the first point
    float          length ;
    float          dpt ;
    float          dpsi;
    float          dz0 ;
    float          dtanl ;
};                              //16 dwords

// Bank which actually has the global tracks in it:
// compared to the sector level tracks this merges the pointer and data bank
// into one Bank.

struct HLT_GT {
    unsigned int     nGlobalTracks;
    struct hlt_track globalTrack[10000];
};
 
struct HLT_PT {
    unsigned int     nPrimaryTracks;
    struct hlt_track primaryTrack[10000];
};
 
struct hlt_TofHit{
    short trayId;
    short channel;              // = nModule*6+nCell
    float tdc;
    float tot;
    float tof;
    float triggertime;
};

struct HLT_TOF {
    unsigned int      nTofHits;
    struct hlt_TofHit tofHit[10000];
};

struct hlt_ETofHit{
    float  time;

    float  localX;
    float  localY;

    float  globalX;
    float  globalY;
    float  globalZ;

    float  tot;

    short  sector;
    short  module;
    short  counter;
    short  clustersize;

    short trackId;
};

struct HLT_ETOF{
    unsigned int nETofHits;
    struct hlt_ETofHit etofHit[10000];
};
    
struct hlt_MtdHit {
  float   leadingEdgeTime[2];
  float   trailingEdgeTime[2];
  char    fiberId;
  char    backleg; // 1-30
  char    tray;    // 1-5
  char    channel; // 0-12
  int     hlt_trackId;
  float   delta_z;
  float   delta_y;
  int     isTrigger;
};

struct HLT_MTD {
  unsigned int      nMtdHits;
  struct hlt_MtdHit mtdHit[10000];
};

struct HLT_MTDPair {
    int   muonTrackId1;         // leading track
    int   muonTrackId2;
    float pt;                   // pair py,eta,phi
    float eta;
    float phi;
    float invMass;              // pair InvMass
};

struct HLT_MTDQuarkonium {
    int nMTDQuarkonium;
    struct HLT_MTDPair MTDQuarkonium[1000];
};

struct HLT_PVPD {
    unsigned int      nPvpdHits;
    struct hlt_TofHit pvpdHit[10000];
};

struct hlt_emcTower {
    int   adc;
    float energy;

    float phi; 
    float eta;
    float z;
    int softId;
    int daqId;
};

struct HLT_EMC {
    unsigned int        nEmcTowers; 
    struct hlt_emcTower emcTower[4800];
};

struct hlt_node {
    int    globalTrackSN;       // serial number in HLT_GT
    int    primaryTrackSN;      // serial number in HLT_PT
    int    tofHitSN;            // serial number in HLT_TOF
    int    emcTowerSN;          // serial number in HLT_EMC
    
    double emcMatchPhiDiff;     // make this struct must be 8-byte aligned
    double emcMatchZEdge;

    int    projChannel;
    float  localY;
    float  localZ;
    float  beta;
    float  tof;
    float  pathlength;

    float  etofBeta;    // -999: no intercept; -1: intercept, no match; >= 0: match
    float  etofPi;      // time of flight measured with ETOF assume pi
};

struct HLT_NODE {
    unsigned long long int nNodes; // keep alignment 8 bytes
    struct hlt_node        node[10000];
};
 
struct HLT_EVE {
    unsigned int version ;
    unsigned int hltDecision;   // HLT trigger bits
    float        vertexX;
    float        vertexY;
    float        vertexZ;
    float        lmVertexX;
    float        lmVertexY;
    float        lmVertexZ;
    float        vpdVertexZ;
    float        T0;
    float        innerSectorGain;
    float        outerSectorGain;
    float        beamlineX;
    float        beamlineY;
    float        bField;
    unsigned short bunch_id;
    unsigned short bbce; // earliest TACs for BBC/VPD/EPD
    unsigned short bbcw;
    unsigned short vpde;
    unsigned short vpdw;
    unsigned short epde;
    unsigned short epdw;
};

struct hlt_diElectronPair {
    int   dau1NodeSN;
    int   dau2NodeSN;
    float invariantMass;
    float pt;
    float psi;
    float tanl;
    int   dau1SelectionBit;
    int   dau2SelectionBit;
};

struct hlt_diPionPair {
    int   dau1NodeSN;
    int   dau2NodeSN;
    float invariantMass;
    float pt;
    float psi;
    float tanl;
    float deltphi;              // phi difference between two dau tracks
    //int dau1SelectionBit;
    //int dau2SelectionBit;
};

struct hlt_MatchedHT2{
    float p2;
    float invMass;
};

// di-electron pair bank
struct HLT_DIEP {
    unsigned int              nEPairs; 
    struct hlt_diElectronPair ePair[1000];
};

// UPC rho bank (pion pair)
struct HLT_RHO {
    unsigned int          nRhos;
    struct hlt_diPionPair PionPair[1000];
};

// high pt bank
struct HLT_HIPT {
    unsigned int nHighPt; 
    int          highPtNodeSN[1000];
};

// heavy fragment bank
struct HLT_HF {
    unsigned int nHeavyFragments; 
    int          heavyFragmentSN[1000];
};

// MatchedHT2 bank
struct HLT_HT2 {
    int   nPairs;
    float p0;
    float EoP0;
    int   towerADC;
    int   towerSoftID;
    int   maxADC;
    float r;
    float phi;
    float z;
    float eta;
    float pt;
    float psi;                  //azimuthl angle of p at r,phi,z
    bool  triggered;
    char  reserved[3];          // padding
    struct hlt_MatchedHT2 trackPair[1000];
};

// Low Multipilicity 
struct HLT_LM {
    int nPrimaryTracks;
};

// V0 bank
struct hlt_v0 {
    unsigned mIndex2Track[2];   // two daughters, 0 for pos, 1 for neg
    float    mMomentum[2][3];
    float    mV0Pos[3];         // v0 position
    float    mDcaDaughters;     // dca between two daughters
    float    mDca2Vtx;          // dca between V0 and primary vertex
    float    mM;                // invariant mass of V0
};

struct HLT_V0ARRAY {
    static const unsigned MAXNV0S = 1000;
    unsigned nV0s;
    hlt_v0 v0Array[MAXNV0S];
};

#endif
