/* $Id: pxl_decoder.h,v 1.1 2014/04/17 19:05:15 jschamba Exp $ */
#include <bitset>
using namespace std;

//#define PRINT_DEBUG 1
//#define PRINT_STATUS 1
#define CRC_CALC 1

#ifdef PRINT_DEBUG
#define DEBUGP(...) fprintf(stdout, __VA_ARGS__)
#else
#define DEBUGP(...)
#endif

// some constants for PXL RDO
const int NSENSOR = 40;
const int NCOL = 960;
const int NROW = 928;

const int PXLERR_HEADER = 0x001;
const int PXLERR_HITLEN = 0x002;
const int PXLERR_SENSORNUM = 0x004;
const int PXLERR_DUPLICATE = 0x008;
const int PXLERR_ENDER = 0x010;
const int PXLERR_RANGE = 0x020;
const int PXLERR_OUTOFORDER = 0x040;
const int PXLERR_UNKNOWNROW = 0x080;
const int PXLERR_CRC = 0x100;
const int PXLERR_LENGTH = 0x200;

struct _pxlHeaderInfo {
  unsigned short tcdWord;
  unsigned int rhicStrobeCtr;
  unsigned short temperature[4][2];
};

int pxl_decoder(const u_int *d, 
		const int wordLength, 
		bitset<NCOL> **bs, 
		int *OVF,
		struct _pxlHeaderInfo *pxlHeaderInfo,
		float *ave_runlength,
		int *error_cnt);

