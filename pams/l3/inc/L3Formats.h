/* written 11/23/99 cle
 * changes:
 *       11/24/99 cle: renamed some variables  
 *       12/06/99 ppy: add xLastHit, yLastHit to type1_track
 */

#ifndef _L3_FORMATS_H
#define _L3_FORMATS_H

/*
//#include <SECTOR/daqFormats.h> // everything should be compiled with -I/DAQ/include 
*/

#define CHAR_L3_P	"L3_P"
#define CHAR_L3_SECP	"L3_SECP"
#define CHAR_L3_SECTP	"L3_SECTP"
#define CHAR_L3_STK1D	"L3_STK1D"
#define CHAR_L3_STK2D	"L3_STK2D"
#define CHAR_L3_STK3D	"L3_STK3D"




/* Track Type I: Mergable Primary Track */
struct type1_track { 
    short id ;
    short nHits ;
    float dedx ; 
    float s11Xy ;/* Fit parameters*/ 
    float s12Xy ;/* conformal line in xy plane */ 
    float s22Xy ; 
    float g1Xy ; 
    float g2Xy ; 
    float s11Sz ;/* Fit parameters in sz plane */ 
    float s12Sz ; 
    float s22Sz ; 
    float g1Sz ; 
    float g2Sz ; 
    float xLastHit ;
    float yLastHit ;
    float trackLength ; 
}; /* 13 words */



/* Track Type II: Unmergable Primary Track */ 
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
}; /* 9 words */


/* Track Type III: Unmergable Secondary Track */
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
};  /* 11 words */



struct L3_STK1D {
  struct bankHeader bh;
  struct type1_track track[500]; /*dimension not to be taken literally */
};

struct L3_STK2D {
  struct bankHeader bh;
  struct type2_track track[500]; /*dimension not to be taken literally */
};

struct L3_STK3D {
    struct bankHeader bh;
    struct type3_track track[500]; /*dimension not to be taken literally */
};



/* pointer bank for all track data pointing to L3_STK[1-3]D banks */

struct L3_SECTP {
    struct bankHeader bh;
    /* words: */
    unsigned int nHits; /* Nr of space points */
    unsigned int nTracks; /* Nr of Tracks */
    unsigned int cpuTime; /* CPU time in microseconds */
    unsigned int realTime; /* real time in microseconds */
    int          xVert;         /* x vertex position in 10**-6 cm */
    int          yVert;         /* y vertex position */
    int          zVert;         /*z vertex postion */
    int          para;      /* parameter set used */
    struct offlen banks[3]; /* offset and length in 4 byte words for the */ 
                        /* track type I, II and III in this order        */
                        /* if len = 0 for type x there will be no L3_STKxD */ 
                        /* bank */
};


/*
// Top level L3 pointer bank. Pointing to TPCSECLP (Cluster data) and
// to L3_SECTP (track data).  
*/

struct L3_SECP {
    struct bankHeader bh;
    unsigned int len;  /* length of the entire sector contribution */
    unsigned int time; /* time when the event is put together in unix format */
    unsigned int seq;  /* sequence nr. hopefully unique inside one run ;) */
    unsigned int trg_word;    /* for the future */
    unsigned int trg_in_word; /* also future... don't even know what that */ 
                                 /* means.... */
    struct offlen clusterp; /*  offset/length to/of TPCSECLP */
    struct offlen trackp; /*  offset/length to/of L3_SECTP */
                             /* if length = 0 Bank is not present. */ 
}; /* L3_SECP is almost the same as DATAP, variables all have the same meaning */
    

struct L3_P {
    struct bankHeader bh;
    unsigned int len;   /* lenght of the entire L3 contribution */
    unsigned int time;  /* time when bank is produced */
    unsigned int seq;
    unsigned int trg_word;
    unsigned int trg_in_word;
    struct offlen sector[24]; /* sector contributions, offset and length */
} ; /* almost the same as L3_SECP */ 


#endif
