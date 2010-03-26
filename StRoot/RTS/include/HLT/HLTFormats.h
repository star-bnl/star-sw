/* Written 11/23/99 cle
 */

/******************* NOTE ********************************************
 * This file should never be included directly. It is included from  * 
 * /DAQ/include/daqFormats.h after the necessary structs are defined *
 *********************************************************************/

#ifndef _HLT_FORMATS_H
#define _HLT_FORMATS_H

//#define HLT_GL3_VERSION	0x20100107
#define HLT_GL3_VERSION	0x20100216  // add dedx gain parameters in HLT_EVE

// Global tracks:

struct hlt_track {
     int id ;        //primary key
     unsigned short flag ;    // Primaries flag=1, Secondaries flag=0
     char innerMostRow ;
     char outerMostRow ;
     unsigned char nHits ;     // Number of points assigned to that track
     char reserved ; 
     unsigned char ndedx;    // nr of clusters contributing to the dedx value
     char q ;       // charge
     float chi2[2];  // chi squared of the momentum fit
     float dedx;     // dE/dx information
     float pt ;      // pt (transverse momentum) at (r,phi,z)
     float phi0;     // azimuthal angle of the first point
     float psi ;     // azimuthal angle of the momentum at (r,..
     float r0 ;      // r (in cyl. coord.) for the first point
     float tanl;     // tg of the dip angle at (r,phi,z)
     float z0 ;      // z coordinate of the first point
     float length ;
     float dpt ;
     float dpsi;
     float dz0 ;
     float dtanl ;
}; //16 dwords

// Bank which actually has the global tracks in it:
// compared to the sector level tracks this merges the pointer and data bank
// into one Bank.

struct HLT_GT {
    unsigned int      nGlobalTracks;
    struct hlt_track globalTrack[10000];
};
 
struct HLT_PT {
    unsigned int      nPrimaryTracks;
    struct hlt_track primaryTrack[10000];
};
 
struct hlt_TofHit{
  short trayId;
  short channel;// = nModule*6+nCell
  float tdc;
  float tot;
  float tof;
  float triggertime;
};


struct HLT_TOF {
    unsigned int      nTofHits;
    struct hlt_TofHit tofHit[10000];
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
    unsigned int      nEmcTowers; 
    struct hlt_emcTower emcTower[4800];
};
 
struct hlt_node {
	int globalTrackSN;  // serial number in HLT_GT
	int primaryTrackSN;  // serial number in HLT_PT
	int tofHitSN;  // serial number in HLT_TOF
	int emcTowerSN;  // serial number in HLT_EMC

        double emcMatchPhiDiff;
        double emcMatchZEdge;

	int projChannel;
	float localY;
	float localZ;
	float beta;
	float tof;
	float pathlength;
};

struct HLT_NODE {
    unsigned int      nNodes; 
    struct hlt_node node[10000];
};
 
struct HLT_EVE {
  unsigned int version ;
  unsigned int hltDecision;  // HLT trigger bits
  float vertexX;
  float vertexY;
  float vertexZ;
  float lmVertexX;
  float lmVertexY;
  float lmVertexZ;
  float vpdVertexZ;
  float T0;
  float innerSectorGain;
  float outerSectorGain;

};

struct hlt_diElectronPair {
  int dau1NodeSN;
  int dau2NodeSN;
  float invariantMass;
  float pt;
  float psi;
  float tanl;
};

// di-electron pair bank
struct HLT_DIEP {
  unsigned int      nEPairs; 
  struct hlt_diElectronPair ePair[1000];
};

// high pt bank
struct HLT_HIPT {
  unsigned int      nHighPt; 
  int highPtNodeSN[1000];
};

// heavy fragment bank
struct HLT_HF {
  unsigned int      nHeavyFragments; 
  int heavyFragmentSN[1000];
};
#endif
