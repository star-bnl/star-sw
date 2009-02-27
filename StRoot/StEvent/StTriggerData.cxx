/***************************************************************************
 *
 * $Id: StTriggerData.cxx,v 2.6 2009/02/27 02:56:52 ullrich Exp $
 *
 * Author: Akio Ogawa, Feb 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTriggerData.cxx,v $
 * Revision 2.6  2009/02/27 02:56:52  ullrich
 * Fixed bug in reading pre/post data.
 *
 * Revision 2.5  2009/01/14 17:54:45  ullrich
 * Modified to cope with necessary changes for 2009.
 *
 * Revision 2.4  2006/09/13 23:59:55  ullrich
 * Added new data member mRun. Removed arg run from ctb(), ctbTraySlat(), zdcSMD()
 *
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

static const char rcsid[] = "$Id: StTriggerData.cxx,v 2.6 2009/02/27 02:56:52 ullrich Exp $";

ClassImp(StTriggerData)

StTriggerData::StTriggerData() : mYear(0), mZdcVertexZ(-999), mRun(0) { /* noop */ }

StTriggerData::~StTriggerData() { /* noop */}

int StTriggerData::prepostAddress(int prepost) const
{ 
    if (prepost == 0) return 0;
    int npre = numberOfPreXing();
    if (prepost < 0 && -prepost <= npre) return 1+npre+prepost;
    int npost = numberOfPostXing();
    if (prepost > 0 &&  prepost <= npost) return npre+prepost;
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

void StTriggerData::decodeQT(unsigned int ndata, unsigned int* data, unsigned short adc[16][32], unsigned char tdc[16][32])
{
    if (ndata==0) return;
    if (ndata>MaxQTData)         { printf("QT data length %d is too long (max is %d)\n",ndata,MaxQTData); return;}
    if (data[ndata-1] != 0xac10) { printf("Wrong QT data last word %x (should be 0xAC10)\n",data[ndata-1]); return;}
    int header=1;
    unsigned int crate,addr,ch,nline,oldch;
    for (unsigned int i=0; i<ndata-1; i++){
        unsigned int d = data[i];
        if (header==1){
            crate =  (d & 0xff000000) >> 24;
            addr  = ((d & 0x00ff0000) >> 16) - 0x10;
            nline =  (d & 0x000000ff);
            oldch = 0;
            if(nline>0) header=0;
            //printf("i=%3d  crate=%3d  addr=%3d  nline=%3d\n",i,crate,addr,nline);
        }
        else {
            ch = (d & 0xf8000000) >> 27;
            adc[addr][ch] = (unsigned short)  (d & 0x00000fff);
            tdc[addr][ch] = (unsigned char)  ((d & 0x0001f000) >> 12);
            //printf("i=%3d  crate=%3d  addr=%3d  nline=%3d  ch=%3d  adc=%5d  tdc=%5d\n",i,crate,addr,nline,ch,adc[addr][ch],tdc[addr][ch]);
            //if(adc[addr][ch]==0) printf("ADC = 0  problem : i=%3d  crate=%3d  addr=%3d  nline=%3d  ch=%3d  adc=%5d  tdc=%5d\n",i,crate,addr,nline,ch,adc[addr][ch],tdc[addr][ch]);
            //if(ch<=oldch)      printf("Ch Order problem : i=%3d  crate=%3d  addr=%3d  nline=%3d  ch=%3d  adc=%5d  tdc=%5d\n",i,crate,addr,nline,ch,adc[addr][ch],tdc[addr][ch]);
            oldch=ch;
            nline--;
            if (nline==0) header=1;
        }    
    }
}

