/***************************************************************************
 *
 * $Id: L3.Banks.hh,v 1.4 2000/09/11 16:25:20 struck Exp $
 *
 * Author: Christof Struck, struck@star.physics.yale.edu
 ***************************************************************************
 *
 * Description: L3 raw data banks
 *
 *
 *
 * change log:
 *   06 Jun 00 CS initial version
 *   24 Jul 00 CS added i960 cluster banks
 *   09 Sep 00 CS added l3_summary and summary_data in L3_P
 *
 ***************************************************************************
 *
 * $Log: L3.Banks.hh,v $
 * Revision 1.4  2000/09/11 16:25:20  struck
 * added L3_summary and summary_data to L3_P
 *
 * Revision 1.3  2000/07/26 02:12:27  struck
 * added i960 cluster reader
 *
 * Revision 1.2  2000/07/06 18:16:00  ward
 * Install L3 code from Christof Struck.
 *
 *
 **************************************************************************/
#ifndef L3_BANKS_HH
#define L3_BANKS_HH


//////////////////////////////////////////////  includes  //////////////////////
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"
/////////////////////////////////////////////  classes and structures  /////////

#define CHAR_L3_P	"L3_P    "
#define CHAR_L3_SECP	"L3_SECP "
#define CHAR_L3_SECTP	"L3_SECTP"
#define CHAR_L3_STK1D	"L3_STK1D"
#define CHAR_L3_STK2D	"L3_STK2D"
#define CHAR_L3_STK3D	"L3_STK3D"
#define CHAR_L3_LTD     "L3_LTD  "
#define CHAR_L3_GTD     "L3_GTD  "
#define CHAR_L3_SECCD   "L3_SECCD"
// i960 cluster banks
#define CHAR_TPCSECLP   "TPCSECLP"
#define CHAR_TPCRBCLP   "TPCRBCLP"
#define CHAR_TPCMZCLD   "TPCMZCLD"


// Top-level pointer bank
struct Bank_L3_P: public Bank
{
  INT32   len;
  INT32   time;
  INT32   seq;
  INT32   trg_word;
  INT32   trg_in_word;
  Pointer sector[24];
  Pointer tracks;
  Pointer summary_data;
  INT32   L3_summary[4];
};


struct Bank_L3_SECP: public Bank
{
  unsigned int len;         // length of the entire sector contribution
  unsigned int time;        // time when the event is put together in unix format
  unsigned int seq;         // sequence nr. hopefully unique inside one run ;)
  unsigned int trg_word;    // for the future
  unsigned int trg_in_word; // also future... don't even know what that means...
  Pointer      clusterp;    // offset/length to/of TPCSECLP
  Pointer      trackp;      // offset/length to/of L3_SECTP
  Pointer      sl3clusterp; // offlen for sl3 produced cluster data
                            // if length = 0 Bank is not present. 
};


// only tracktype produced on sl3 beginning 04/06/00
struct localTrack
{
       short id;            // track id
       char  nHits;         // Number of hits assigned to the track 
       char  ndedx;         // Number of points used for dedx 
       short innerMostRow ; // Inner most row track expands 
       short outerMostRow ; // Outer most row track expands 
       short xy_chisq;      // xy & sz chi2 packed in 16 bits each 
       short sz_chisq;      // same as with track type II, divide by 10 for
                            // real result
       float dedx;          // dE/dx information 
       float pt;            // pt time charge 
       float psi;           // azimuthal angle of the momentum at (r,.. 
       float tanl;          // tg of the dip angle at (r,phi,z) 
       float z0;            // z coordinate of the first point 
       float r0;            // r coordinate of the first point 
       float phi0;          // phi coordinate of the first point
       float trackLength;
       unsigned short dpt;
       unsigned short dpsi;
       unsigned short dtanl;
       unsigned short dz0;
};


// Global tracks
struct globalTrack
{
     int            id;            //primary key
     unsigned short flag;          // Primaries flag=1, Secondaries flag=0
     char           innerMostRow;
     char           outerMostRow;
     unsigned char  nHits;         // Number of points assigned to that track
     char           reserved; 
     unsigned char  ndedx;         // nr of clusters contributing to the dedx value
     char           q;             // charge
     float          chi2[2];       // chi squared of the momentum fit
     float          dedx;          // dE/dx information
     float          pt;            // pt (transverse momentum) at (r,phi,z)
     float          phi0;          // azimuthal angle of the first point
     float          psi;           // azimuthal angle of the momentum at (r,..
     float          r0;            // r (in cyl. coord.) for the first point
     float          tanl;          // tg of the dip angle at (r,phi,z)
     float          z0;            // z coordinate of the first point
     float          length;
     float          dpt;
     float          dpsi;
     float          dz0;
     float          dtanl;
};


// cluster data produced on sl3:
struct l3_cluster
{
    unsigned short pad;     // in 1/64 pads
    unsigned short time;    // in 1/64 time bins
    unsigned short charge;
    unsigned short flags;
    unsigned short trackId;
    char           padrow;
    unsigned char  RB_MZ;   // RB*16 | MZ, meaning upper 4 bits are RB,
                            // lower 4 bits are MZ
};


// Bank which actually has the global tracks in it:
// compared to the sector level tracks this merges the pointer and data bank
// into one Bank.
struct Bank_L3_GTD: public Bank
{
    unsigned int  nHits;         // Nr of space points
    unsigned int  nTracks;       // Nr of Tracks
    int           xVert;         // x vertex position in 10**-6 cm
    int           yVert;         // y vertex position
    int           zVert;         // z vertex postion
    globalTrack   track[1];

    int swap();
};


struct Bank_L3_LTD: public Bank
{
    localTrack track[1];

    int swap();
};

// pointer bank for all sl3 track data
// here implemented: banks[0] points to local_track (L3_LTD struct)
struct Bank_L3_SECTP: public Bank
{
    unsigned int nHits;    // Nr of space points
    unsigned int nTracks;  // Nr of Tracks
    unsigned int cpuTime;  // CPU time in microseconds
    unsigned int realTime; // real time in microseconds
    int          xVert;    // x vertex position in 10**-6 cm
    int          yVert;    // y vertex position
    int          zVert;    //z vertex postion
    int          para;     // parameter set used
    Pointer      banks[3]; // offset and length in 4 byte words for the 
};


// cluster data produced on sl3:
struct Bank_L3_SECCD: public Bank
{
    unsigned int  nrClusters_in_sector;
    l3_cluster    cluster[1];

    int swap();
};



// Vertex
struct vertex {
     float x;
     float y;
     float z;
};


// i960 cluster banks
// exported by SL3
struct Bank_TPCSECLP: public Bank
{
     Pointer receiverBoard[12];   // year one: rb 1-6 odd sector, rb 7-12 even sector 
};


struct Bank_TPCRBCLP: public Bank
{
     Pointer mezzBoard[3];
     int     fiberHeader[12];
};


// already defined in TPCV2PO.Banks.hh
// defined here for consistency, since bank is normally 
// exported by SL3
struct Bank_TPCMZCLD: public Bank
{
     int numberOfRows;
     int stuff[10];     // place holder for cluster data

     int swap();
};


// Swap short
int l3Swap_short (short* data, short size);

#endif
