/***************************************************************************
 *
 * $Id: StTriggerData.cxx,v 2.3 2004/08/03 17:22:16 ullrich Exp $
 *
 * Author: Akio Ogawa, Feb 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerData.cxx,v $
 * Revision 2.3  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.2  2004/02/11 01:39:49  ullrich
 * Use enumeration StBeamDirector for east/west. Add member for ZDC vertex.
 *
 * Revision 2.1  2003/04/16 17:47:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StTriggerData.h"

static const char rcsid[] = "$Id: StTriggerData.cxx,v 2.3 2004/08/03 17:22:16 ullrich Exp $";

ClassImp(StTriggerData)

StTriggerData::StTriggerData() : mYear(0), mZdcVertexZ(-999) { /* noop */ }

StTriggerData::~StTriggerData() { /* noop */}

int StTriggerData::prepostAddress(int prepost) const
{ 
    if(prepost == 0) return 0;
    int npre = -numberOfPreXing();
    if(prepost < 0 && -prepost <= npre) return 1+npre+prepost;
    int npost = -numberOfPostXing();
    if(prepost > 0 &&  prepost <= npost) return npre+prepost;
    return -1;
}

unsigned short StTriggerData::decodeEmc12bit(const int dsm, const int channel, const unsigned char *raw) const
{
  static const int dsmmap[16]={7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  const unsigned char *crate_p=raw+dsm*16; // 16 bytes=128 bits per dsm
  
  if (channel%2) {  
    // channel is odd, take highest four bits + next byte
    int k=(channel-1)/2*3+1; // position of low byte
    return (crate_p[dsmmap[k+1]]<<4) + (crate_p[dsmmap[k]]>>4);
  }
  else {
    // channel is even, take lower byte + lowest 4 bits of next 
    int k=channel/2*3; // position of low byte
    return ((crate_p[dsmmap[k+1]]&0xF) << 8) + crate_p[dsmmap[k]];
  }
}
