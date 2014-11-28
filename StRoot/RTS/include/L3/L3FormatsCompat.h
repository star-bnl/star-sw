/* Written 11/23/99 cle
 * changes:
 *       11/24/99 cle: renamed some variables  
 *       12/06/99 ppy: add xLastHit, yLastHit to type1_track
 *       01/31/00 ppy: add innerMostRow and outerMostRow to type1_track
 *       03/29/00 cle: added global_track, L3_GTD, L3_SECCD.
 *                     added offlen sl3clusterp to L3_SECP.
 *                     added offlen tracks to L3_P.
 *       04/06/00 cle: implement pablos suggestion to divide L3_SECCD into
 *		       L3_SECCD and L3_CLUSTER, named it l3_cluster
 *		       added L3_LTD and local_track (pablos modified 
 *                     type3_track).
 *                     format_number in L3_SECTP->bh defines the use of 
 *                     L3_SECTP->banks. 0: banks[0-2] = type1-3 track
 *                     1: banks[0] = local_track (L3_LTD)
 *       08/18/00 cle: put L3 summary data and L3 summary into L3_P
 *       03/07/01 cle: add svt/FTPC banks to L3_P
 *	 06/21/01 cle: changed L3_P->seq to L3_P->gl3Id for recontruction of counters 
 *	
 */

/******************* NOTE ********************************************
 * This file should never be included directly. It is included from  * 
 * /DAQ/include/daqFormats.h after the necessary structs are defined *
 *********************************************************************/

#ifndef _L3_FORMATS_H
#define _L3_FORMATS_H


#define CHAR_L3_P	"L3_P    "
#define CHAR_L3_SECP	"L3_SECP "
#define CHAR_L3_SECTP	"L3_SECTP"
#define CHAR_L3_STK1D	"L3_STK1D"
#define CHAR_L3_STK2D	"L3_STK2D"
#define CHAR_L3_STK3D	"L3_STK3D"
#define CHAR_L3_LTD     "L3_LTD  "
#define CHAR_L3_GTD     "L3_GTD  "
#define CHAR_L3_SECCD   "L3_SECCD"
#define CHAR_L3_SUMD	"L3_SUMD "

// structure for the 4 l3 summary words
//struct L3_summary{
//	char quality[8];
//	int nrTracks;
//	unsigned int decision;
//};


struct L3_summary{
        unsigned int accept;
        unsigned int build;
        unsigned int on;
        unsigned int nTracks; 
};

struct algorithm_data{
        int algId; // unique algorithm identifier (for non-humans)
        char on; // 1 if this alg. was running on this event, 0 if not.
        char accept;
        char build;
        char blub; // padding
	unsigned int nProcessed; // events processed by that algorithm so far
        unsigned int nAccept; // events that fullfilled alg. so far
        unsigned int nBuild; // events that were flagged to be built 
        float data[10];
};

struct L3_SUMD {
        bankHeader bh;
        unsigned int nProcessed; // all events looked at
        unsigned int nReconstructed; // nProcessed that didn't crash
        int nAlg; // nr of registered algorithms
        struct algorithm_data alg[1]; // array of size nAlg
};

struct algorithm_counter{
	int id; // algorithm Id
	unsigned int nProcessed;
	unsigned int nAccept;
	unsigned int nBuild;
};

struct L3_counter{
	unsigned int nProcessed;
	unsigned int nRecontructed;
	struct algorithm_counter alg[32];
};



// Track Type I: Mergable Primary Track
struct type1_track { 
    short id ;// id 
    short nHits ;// # Hits 
    short innerMostRow ; /* Inner most row track expands */
    short outerMostRow ; /* Outer most row track expands */
    float dedx ; 
    float s11Xy ;// Fit parameters 
    float s12Xy ;// conformal line in xy plane 
    float s22Xy ; 
    float g1Xy ; 
    float g2Xy ; 
    float s11Sz ;// Fit parameters in sz plane 
    float s12Sz ; 
    float s22Sz ; 
    float g1Sz ; 
    float g2Sz ; 
    float xLastHit ;
    float yLastHit ;
    float trackLength ; 
}; // 16 words



// Track Type II: Unmergable Primary Track 
struct type2_track { 
    short     id;        /* track id */
    short     nrec;      /* Number of points assigned to that track */
    short  xy_chisq;    /* xy & sz chi2 packed in 16 bits each */
    short  sz_chisq;    /* both nr.'s  are multiplied by 10 and the fraction
			   cut off   */
    float     dedx;      /* dE/dx information */
    float     pt  ;      /* pt time charge */
    float     psi;       /* azimuthal angle of the momentum at (r,.. */
    float     tanl;      /* tg of the dip angle at (r,phi,z) */
    float     z0;        /* z coordinate of the first point */
    float     trackLength; 
    unsigned int Errors ;   /* dpt, dpsi, dtanl errors (10bits*3)*/
}; //9 words


// Track Type III: Unmergable Secondary Track
struct type3_track { 
       short     id;        /* track id */
       short     nrec;      /* Number of points assigned to that track */
       short  xy_chisq;     /* xy & sz chi2 packed in 16 bits each */
       short  sz_chisq;     /* same as with track type II, divide by 10 for 
			       real result*/
       float     dedx;      /* dE/dx information */
       float     pt  ;      /* pt time charge */
       float     psi;       /* azimuthal angle of the momentum at (r,.. */
       float     tanl;      /* tg of the dip angle at (r,phi,z) */
       float     z0;        /* z   coordinate of the first point */
       float     r0;        /* r   coordinate of the first point */
       float     phi0;      /* phi coordinate of the first point */
       float     trackLength;
       unsigned int  Errors ;   /* dpt, dpsi, dtanl errors (10bits*3)  */
};  // 11 words




// only tracktype produced on sl3 beginning 04/06/00
struct local_track {
       short     id;        /* track id */
       char     nHits;      /* Number of hits assigned to the track */
       char     ndedx;     /* Number of points used for dedx */
       short innerMostRow ; /* Inner most row track expands */
       short outerMostRow ; /* Outer most row track expands */
       short  xy_chisq;     /* xy & sz chi2 packed in 16 bits each */
       short  sz_chisq;     /* same as with track type II, divide by 10 for
                               real result*/
       float     dedx;      /* dE/dx information */
       float     pt  ;      /* pt time charge */
       float     psi;       /* azimuthal angle of the momentum at (r,.. */
       float     tanl;      /* tg of the dip angle at (r,phi,z) */
       float     z0;        /* z   coordinate of the first point */
       float     r0;        /* r   coordinate of the first point */
       float     phi0;      /* phi coordinate of the first point */
       float     trackLength;
       unsigned short dpt ;
       unsigned short dpsi ;
       unsigned short dtanl ;
       unsigned short dz0   ;
};   // 13 dwords






struct L3_STK1D {
  struct bankHeader bh;
  struct type1_track track[500]; //dimension not to be taken literally
};

struct L3_STK2D {
  struct bankHeader bh;
  struct type2_track track[500]; //dimension not to be taken literally
};

struct L3_STK3D {
    struct bankHeader bh;
    struct type3_track track[500]; //dimension not to be taken literally
};

struct L3_LTD {
    struct bankHeader bh;
    struct local_track track[1];
};  // 10 (+13) dwords

// pointer bank for all track data pointing to L3_STK[1-3]D banks
// if format number in bankHeader == 0 : banks point to type1-3 tracks
// if format_number == 1 : banks[0] point to local_track (L3_LTD struct)
 
struct L3_SECTP {
    struct bankHeader bh;
    // words:
    unsigned int nHits; // Nr of space points
    unsigned int nTracks; // Nr of Tracks
    unsigned int cpuTime; // CPU time in microseconds
    unsigned int realTime; // real time in microseconds
    int          xVert;         // x vertex position in 10**-6 cm
    int          yVert;         // y vertex position
    int          zVert;         //z vertex postion
    int          para;      // parameter set used
    struct offlen banks[3]; // offset and length in 4 byte words for the 
                        // track type I, II and III in this order
                        // if len = 0 for type x there will be no L3_STKxD 
                        // bank
}; 



// cluster data produced on sl3:

struct l3_cluster{
    unsigned short    pad; // in 1/64 pads
    unsigned short    time; // in 1/64 time bins
    unsigned short    charge;
    unsigned short    flags;
    unsigned short    trackId;
    char              padrow ;
    unsigned char     RB_MZ; // RB*16 | MZ, meaning upper 4 bits are RB,
                             // lower 4 bits are MZ
};


// cluster data produced on sl3:

struct L3_SECCD{
    struct bankHeader bh;
    unsigned int      nrClusters_in_sector;
    struct l3_cluster cluster[1];
} ;




// Top level L3 pointer bank. Pointing to TPCSECLP (Cluster data) and
// to L3_SECTP (track data).  

struct L3_SECP {
    bankHeader bh;
    unsigned int len;  // length of the entire sector contribution
    unsigned int time; // time when the event is put together in unix format
    unsigned int seq;  // sequence nr. hopefully unique inside one run ;)
    unsigned int trg_word;    // for the future
    unsigned int trg_in_word; // also future... don't even know what that 
                                 // means....
    struct offlen clusterp; //  offset/length to/of TPCSECLP
    struct offlen trackp; //  offset/length to/of L3_SECTP
    struct offlen sl3clusterp; // offlen for sl3 produced cluster data
                             // if length = 0 Bank is not present. 
}; // L3_SECP is almost the same as DATAP, variables all have the same meaning
   


// Global tracks:

struct global_track {
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

struct L3_GTD {
    struct bankHeader bh;
    unsigned int      nHits; // Nr of space points
    unsigned int      nTracks; // Nr of Tracks
    int               xVert;         // x vertex position in 10**-6 cm
    int               yVert;         // y vertex position
    int               zVert;         //z vertex postion
    struct global_track track[1];
};
 


struct L3_P {
    bankHeader bh;
    unsigned int len;   // lenght of the entire L3 contribution
    unsigned int time;  // time when bank is produced
    unsigned int gl3Id; // node Id of the gl3 who produced that event
    unsigned int trg_word;
    unsigned int trg_in_word;
    struct offlen sector[24]; // sector contributions, offset and length
    struct offlen tracks; // pointer and length to/of L3_GTD
    struct offlen summary_data;
    unsigned int L3_summary[4];
    struct offlen svt[5]; // 4 svt 'sectors' + 1 ssd
    struct offlen ftpc[2]; 
    struct offlen emc;

} ; // almost the same as L3_SECP  
// 70 dwords

#endif
