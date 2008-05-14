#ifndef _ICCP2K_H_
#define _ICCP2K_H_

#include <sys/types.h>

// Event Flag Definition

#define CMD2_PING     0x01
#define CMD2_DATA     0x02
#define CMD2_RELEASE  0x03
#define CMD2_RUN_DONE 0x04
#define CMD2_LOG      0x05

#define EVBFLAG_RAW_DATA (1<<0)
#define EVBFLAG_FCF_DATA (1<<1)
#define EVBFLAG_L25ABORT (1<<2)

//#ifdef RTS_LITTLE_ENDIAN
struct iccp2k {   
  u_int words;        // words of the bank to follow (not including this header)
  u_short srcNode;      // source node
  u_short dstNode;      // destination node
  u_char srcTask;
  u_char dstTask;
  u_short token;
  u_char cmd;
  u_char pad1;
  u_short pad2;
};
/* #else */
/* struct iccp2k {    */
/*   u_int words;        // words of the bank to follow (not including this header) */
/*   //----- */
/*   u_short dstNode; */
/*   u_short srcNode;       */
/*   //---- */
/*   u_short token; */
/*   u_char dstTask; */
/*   u_char srcTask; */
/*   //---- */
/*   u_char pad3; */
/*   u_char pad2; */
/*   u_char pad1; */
/*   u_char cmd; */
/* }; */
/* #endif */


struct gbPayload {
  u_int eventDesc[10] ;   // take from data!  // big endian

  // The rest is all little endian...
  u_int L3summary[4] ;
  u_int L2summary[2];
  u_int L1summary[2];
  u_int rtsDetMask;
  u_int eventNumber;
  u_int sec;
  u_int usec;
  u_int flags;            // bit 0 set, tpc raw data inside
  u_int evp;
  u_int token;
};

struct evpPayload {
  u_int run;
  u_int type;
  u_int seq;
};

struct tapeMsg {
  int cmd;
  int tevt;
  int stream;
};

// network byte order...
struct evtDoneMsg {
  u_short token;
  u_short status;
};


#endif
