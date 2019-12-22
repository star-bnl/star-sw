#ifndef _ICCP2K_H_
#define _ICCP2K_H_

#include <sys/types.h>
#include <daqFormats.h>
// Event Flag Definition

typedef unsigned int u_int;
typedef unsigned short u_short;
typedef unsigned char u_char;
typedef unsigned long long int UINT64;

#define CMD2_PING     0x01
#define CMD2_DATA     0x02
#define CMD2_RELEASE  0x03
#define CMD2_RUN_DONE 0x04
#define CMD2_LOG      0x05
#define CMD2_L3_RELEASE 0x06
#define CMD2_FUTURE_DATA 0x07
#define CMD2_FUTURE_RELEASE 0x08
#define CMD2_EMULATE_EVENT 0x09
#define CMD2_L4_DATA  0x10
#define CMD2_STREAMING_TRIGGER 0x11

// set by daq100decision
#define EVBFLAG_RAW_DATA (1<<0)
#define EVBFLAG_FCF_DATA (1<<1)
#define EVBFLAG_HLT      (1<<2)

// Used in EVB
#define EVBFLAG_DET_RELEASE (1<<5)              // Released by DET
#define EVBFLAG_TIMEOUT     (1<<6)              // Timedout by EVB
#define EVBFLAG_BADBUILD    (1<<7)
#define EVBFLAG_FUTURE_RELEASE (1<<8)
#define EVBFLAG_L4_PROCESSED (1<<9)

// Appended by prepare_gb_payload
#define EVBFLAG_L1ABORT (1<<11)
#define EVBFLAG_L25ABORT (1<<12)
#define EVBFLAG_L25TIMEOUT (1<<13)
#define EVBFLAG_L3ABORT (1<<14)

#pragma pack(1)

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
  u_short payload_words;   // normally 0.  Number of words of payload.
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


#define GB_PAYLOAD_VERSION 0xDA000003
// 2019 run,  expand the rtsDetMask
struct gbPayload {
    u_int gbPayloadVersion;
    
    union {
	EvtDescData EventDescriptor ;   // take from data!  // big endian
	u_int eventDesc[sizeof(EvtDescData)/4];
    };
    
    // The rest is all little endian...
    u_int L3summary[4] ;
    u_int L2summary[2];
    u_int L1summary[2];
    UINT64 rtsDetMask;
    u_int eventNumber;
    u_int sec;
    u_int usec;
    u_int flags;            // bit 0 set, tpc raw data inside
    u_int evp;
    u_int token;
};

// #define GB_PAYLOAD_VERSION 0xDA000002
// 2008 run, pre-new TCU:   gbPayloadVersion=0xDA000002, TrgDataFmtVer=0x40
struct gbPayload_0x02 {
  u_int gbPayloadVersion;

  union {
    EvtDescData EventDescriptor ;   // take from data!  // big endian
    u_int eventDesc[sizeof(EvtDescData)/4];
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

// 2008 run, pre-new TCU:   gbPayloadVersion=0xDA000002, TrgDataFmtVer=0x40
struct gbPayload_pre2016 {
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

#define EVB_SUMMARY_VERSION 2

struct EvbSummary_v2 {
    u_int version;
    u_int sz;
    UINT64 detectorsInRun;
};

struct EvbSummary_v1 {
  u_int version;  // version
  u_int sz;       // size of structure
  u_int detectorsInRun;
};

typedef EvbSummary_v2 EvbSummary;

#pragma pack()

#endif
