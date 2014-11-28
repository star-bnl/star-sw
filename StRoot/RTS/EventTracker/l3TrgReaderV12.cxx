
#include "l3TrgReader.h"
//#include "daqFormats.h"
#include "TRG/trgStructures-Jan2002.h"

#include "l3Swap.h"
//#include "l3Log.h"

#include <string.h>

int l3TrgReader::readV12(void *buffer)
{
    EvtDescData ed;
    TrgSumData sumData;
    RawTrgDet trgDet;

    // Need to swap byte order
    l3SwapBuffer(&ed, buffer, sizeof(EvtDescData)/4);
    l3SwapBuffer(&sumData, 
	       (char*)buffer + sizeof(EvtDescData), 
	       sizeof(TrgSumData)/4);
    memcpy(&trgDet, 
	       (char*)buffer + sizeof(EvtDescData) + sizeof(TrgSumData), 
	       sizeof(RawTrgDet));


    // Everything is prepared, let's get the data
    bunchXing_hi = swap32(ed.bunchXing_hi);
    bunchXing_lo = swap32(ed.bunchXing_lo);

    //token        = swap16(ed.TCU1.FIFO1.TrgToken);
    token        = (swap32(ed.TCU1.fifo1) & 0xFFFF0000)>>16;
    //triggerWord  = swap16(ed.TCU3.FIFO3.TriggerWd);
    physicsWord  = 0; // Comes in v20

    memcpy(ZDC, sumData.DSM.ZDC, 16);
    memcpy(CTB, trgDet.CTB, 256);
    
    return 0;
}
