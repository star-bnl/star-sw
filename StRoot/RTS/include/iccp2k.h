#ifndef _ICCP2K_H_
#define _ICCP2K_H_

#include <sys/types.h>

// Event Flag Definition

#define CMD2_PING     0x01
#define CMD2_DATA     0x02
#define CMD2_RELEASE  0x03
#define CMD2_RUN_DONE 0x04
#define CMD2_LOG      0x05

struct iccp2k {   
  u_int words;        // words of the bank to follow (not including this header)
  u_short srcNode;      // source node
  u_short dstNode;      // destination node
  u_char srcTask;
  u_char dstTask;
  u_short token;
  u_char cmd;
  u_char pad1;
  u_char pad2;
  u_char pad3;
};


struct gbPayload {
  uint eventDesc[10] ;   // take from data!  // big endian

  // The rest is all little endian...
  uint L3summary[4] ;
  uint L2summary[2];
  uint L1summary[2];
  uint rtsDetMask;
  uint eventNumber;
  uint sec;
  uint usec;
  uint flags;            // bit 0 set, tpc raw data inside
  uint evp;
  uint token;
};

struct evpPayload {
  uint run;
  uint type;
  uint seq;
};

struct tapeMsg {
  int cmd;
  int tevt;
  int stream;
};



#endif
