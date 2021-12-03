#ifndef _ICCP2K_H_
#define _ICCP2K_H_

#include <stdint.h>
#include <daqFormats.h>
// Event Flag Definition

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
  uint32_t words;        // words of the bank to follow (not including this header)
  uint16_t srcNode;      // source node
  uint16_t dstNode;      // destination node
  uint8_t srcTask;
  uint8_t dstTask;
  uint16_t token;
  uint8_t cmd;
  uint8_t pad1;
  uint16_t payload_words;   // normally 0.  Number of words of payload.
};
/* #else */
/* struct iccp2k {    */
/*   uint32_t words;        // words of the bank to follow (not including this header) */
/*   //----- */
/*   uint16_t dstNode; */
/*   uint16_t srcNode;       */
/*   //---- */
/*   uint16_t token; */
/*   uint8_t dstTask; */
/*   uint8_t srcTask; */
/*   //---- */
/*   uint8_t pad3; */
/*   uint8_t pad2; */
/*   uint8_t pad1; */
/*   uint8_t cmd; */
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
    uint32_t gbPayloadVersion;
    
    union {
	EvtDescData EventDescriptor ;   // take from data!  // big endian
	uint32_t eventDesc[sizeof(EvtDescData)/4];
    };
    
    // The rest is all little endian...
    uint32_t L3summary[4] ;
    uint32_t L2summary[2];
    uint32_t L1summary[2];
    UINT64 rtsDetMask;
    uint32_t eventNumber;
    uint32_t sec;
    uint32_t usec;
    uint32_t flags;            // bit 0 set, tpc raw data inside
    uint32_t evp;
    uint32_t token;
};

// #define GB_PAYLOAD_VERSION 0xDA000002
// 2008 run, pre-new TCU:   gbPayloadVersion=0xDA000002, TrgDataFmtVer=0x40
struct gbPayload_0x02 {
  uint32_t gbPayloadVersion;

  union {
    EvtDescData EventDescriptor ;   // take from data!  // big endian
    uint32_t eventDesc[sizeof(EvtDescData)/4];
  };

  // The rest is all little endian...
  uint32_t L3summary[4] ;
  uint32_t L2summary[2];
  uint32_t L1summary[2];
  uint32_t rtsDetMask;
  uint32_t eventNumber;
  uint32_t sec;
  uint32_t usec;
  uint32_t flags;            // bit 0 set, tpc raw data inside
  uint32_t evp;
  uint32_t token;
};

// 2008 run, pre-new TCU:   gbPayloadVersion=0xDA000002, TrgDataFmtVer=0x40
struct gbPayload_pre2016 {
  uint32_t gbPayloadVersion;

  union {
    EventDescriptor2008a EventDescriptor ;   // take from data!  // big endian
    uint32_t eventDesc[sizeof(EventDescriptor2008a)/4];
  };

  // The rest is all little endian...
  uint32_t L3summary[4] ;
  uint32_t L2summary[2];
  uint32_t L1summary[2];
  uint32_t rtsDetMask;
  uint32_t eventNumber;
  uint32_t sec;
  uint32_t usec;
  uint32_t flags;            // bit 0 set, tpc raw data inside
  uint32_t evp;
  uint32_t token;
};


////////////// Historic gbPayload Versions ////////////////

// 2008 run, before adding format but after TrgDataFmtVer-->0x40
struct gbPayload_0x01a {         
  // big endian
  union {
    EventDescriptor2008a EventDescriptor ;   // take from data! 
    uint32_t eventDesc[sizeof(EventDescriptor2008a)/4];
  };

  // The rest is all little endian...
  uint32_t L3summary[4] ;
  uint32_t L2summary[2];
  uint32_t L1summary[2];
  uint32_t rtsDetMask;
  uint32_t eventNumber;
  uint32_t sec;
  uint32_t usec;
  uint32_t flags;            // bit 0 set, tpc raw data inside
  uint32_t evp;
  uint32_t token;
};

// TrgDataFmtVer<0x40
struct gbPayload_0x01 {         // for 2007 run
  // big endian
  union {
    EventDescriptor2007 EventDescriptor ;   // take from data! 
    uint32_t eventDesc[sizeof(EventDescriptor2007)/4];
  };

  // The rest is all little endian...
  uint32_t L3summary[4] ;
  uint32_t L2summary[2];
  uint32_t L1summary[2];
  uint32_t rtsDetMask;
  uint32_t eventNumber;
  uint32_t sec;
  uint32_t usec;
  uint32_t flags;            // bit 0 set, tpc raw data inside
  uint32_t evp;
  uint32_t token;
};




/////////////////////////////////////////////////////////



struct evpPayload {
  uint32_t run;
  uint32_t type;
  uint32_t seq;
};

struct tapeMsg {
  int cmd;
  int tevt;
  int stream;
};

// network byte order...
struct evtDoneMsg {
  uint16_t token;
  uint16_t status;
};

#define EVB_SUMMARY_VERSION 3

struct EvbSummary_v3 {
    uint32_t version;
    uint32_t sz;
    UINT64 detectorsInRun;
    float triggerFrequency;
    uint32_t runNumber;
};

struct EvbSummary_v2 {
    uint32_t version;
    uint32_t sz;
    UINT64 detectorsInRun;
};

struct EvbSummary_v1 {
  uint32_t version;  // version
  uint32_t sz;       // size of structure
  uint32_t detectorsInRun;
};

typedef EvbSummary_v3 EvbSummary;

#pragma pack()

#endif
