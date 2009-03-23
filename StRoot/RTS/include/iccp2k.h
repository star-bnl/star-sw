#ifndef _ICCP2K_H_
#define _ICCP2K_H_

#include <sys/types.h>
#include <daqFormats.h>

// Event Flag Definition

#define CMD2_PING     0x01
#define CMD2_DATA     0x02
#define CMD2_RELEASE  0x03
#define CMD2_RUN_DONE 0x04
#define CMD2_LOG      0x05

#define EVBFLAG_RAW_DATA (1<<0)
#define EVBFLAG_FCF_DATA (1<<1)
#define EVBFLAG_L25ABORT (1<<2)
#define EVBFLAG_L25TIMEOUT (1<<3)

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


// Versioning for gbPayload:
//    
// if (GB_PAYLOAD_VERSION & 0xff000000) != 0xDA000000
//    then version = gbPayload_0x01
// else 
//    version = gbPayloadVersion...

#define GB_PAYLOAD_VERSION 0xDA000002


// 2008 run, pre-new TCU:   gbPayloadVersion=0xDA000002, TrgDataFmtVer=0x40
struct gbPayload {
  u_int gbPayloadVersion;

  union {
    EventDescriptor2008a EventDescriptor ;   // take from data!  // big endian
    u_int eventDesc[sizeof(EventDescriptor2008a)/4];
  };

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


////////////// Historic gbPayload Versions ////////////////

// 2008 run, before adding format but after TrgDataFmtVer-->0x40
struct gbPayload_0x01a {         
  // big endian
  union {
    EventDescriptor2008a EventDescriptor ;   // take from data! 
    u_int eventDesc[sizeof(EventDescriptor2008a)/4];
  };

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

// TrgDataFmtVer<0x40
struct gbPayload_0x01 {         // for 2007 run
  // big endian
  union {
    EventDescriptor2007 EventDescriptor ;   // take from data! 
    u_int eventDesc[sizeof(EventDescriptor2007)/4];
  };

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

/////////////////////////////////////////////////////////



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
