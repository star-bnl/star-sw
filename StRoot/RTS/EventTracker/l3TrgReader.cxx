#include "l3TrgReader.h"
#include <rtsLog.h>
#include <stdio.h>

#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include "daqFormats.h"
//#include "TRG/trgStructures.h"

#include <string.h>

int l3TrgReader::readL3P(L3_P *l3p)
{
    if (l3p == NULL) return -1;

    if (l3p->trig.off == 0) return -1;

    char *trgBuf = (char*)l3p + l3p->trig.off*4;


    unsigned char version;

    if (l3p->bh.format_number <= 7) {
	version = ((unsigned char *)trgBuf)[0];

	if (version > 0x20) {
	    LOG(ERR,"L3_P fn=%d should not contain TRG=0x%x",
		  l3p->bh.format_number,version,0,0,0);
	    return -1;
	}
    } else {
	version = ((TrgDataType*)trgBuf)->EvtDesc.TrgDataFmtVer;

	if (version <= 0x20) {
	  LOG(ERR,"L3_P fn=%d should not contain TRG=0x%x",
		  l3p->bh.format_number,version,0,0,0);
	    return -1;
	}
    }


    switch (version) {

    case 0x12:
    case 0x13:
	return readV12(trgBuf);

    case 0x20:
	return readV20(trgBuf);

    case 0x21:
	return readV21(trgBuf);

    default:
      LOG(ERR,"l3Trg: Unknown trigger format: 0x%x (0x%08x) %s\n", 
	  version, *((long*)trgBuf), (char*)trgBuf,0,0,0);
	return -1;
    }


}


int l3TrgReader::read(void *buffer)
{
  LOG(ERR,"WARNING: l3TrgReader::read is obsolete",0,0,0,0,0);

    unsigned char version = ((unsigned char *)buffer)[0];

    switch (version) {

    case 0x12:
    case 0x13:
	//l3Log("l3Trg: Going into readV12\n");
	return readV12(buffer);

    case 0x20:
	//l3Log("l3Trg: Going into readV20\n");
	return readV20(buffer);

    case 0x21:
	// Starting Jan 2004, we get the full TrgDataType in one block 
	// instead of three chunks for evtDesc, trgSum and rawTrgDet 
	// The reading of this still has to be implemented...

	return readV21(buffer);

    default:
	//l3Log("l3Trg: Unknown trigger format: 0x%x\n", version);
	return -1;
    }
}


void l3TrgReader::reset()
{
    token = 0;
    bunchXing_hi = 0;
    bunchXing_lo = 0;

    physicsWord = 0;
    triggerWord = 0;

    memset(ZDC, 0,  16);
    memset(CTB, 0, 256);
}
