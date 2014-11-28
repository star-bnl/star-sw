#ifndef _DAQ1000FORMATS_H_
#define _DAQ1000FORMATS_H_

// Tonko's data format for now...
//
// rdo data as follows:
// ------------------------
// | rdoHeader            |
// ------------------------
// | altroData[] fee 1    |
// | altroData[] fee 2    |
// | ...                  |
// | altroData[] fee n    |
// ------------------------
// | rdoTrailer           |
// ------------------------
//
// For the ALTRO data format see the altro user manual.  The data from the altro
// is packed into 64 bit data words.  4 10 bit altro words are packed into the low 40 bits
// of each data word.  The high 24 bits are used for status information
// 
// The offset to the end of the altro data block is obtained is obtained from the rdoHeader
// There is way to navigate through the altro data without decoding each banks starting
// at the end
//


struct rdoHeader {
  u_int bytes ;         // bytes including header and trailer, minus 4 bytes.
  u_int w[2] ;          // (each has same format as trailer? but fifo status 0xa?
  union {
    u_int deadface;     // currently 0xdeadface
    char specifier[4];  // future could be "DTA\0" / "LOG\0" etc... for easy browsing
  };
};

struct altroData {
  // Packing is altro10[x] = (w>>(x*10)) & 0x3ff
  unsigned long long w;
  
};

struct rdoTrailer {
  union {
    u_int status;
    struct {
      unsigned evt:20;
      unsigned err:8;              // 0x08 good event
      unsigned fifoStatus:4;       // 0xb
    };
  };
};




#endif

