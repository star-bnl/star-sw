#include "l3TrgReader.h"
#include "trgStructures.h"

#include "l3Swap.h"

#include <string.h>

int l3TrgReader::readV20(void *buffer)
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

    // Everything is prepared, let's read the data
    bunchXing_hi = swap32(ed.bunchXing_hi);
    bunchXing_lo = swap32(ed.bunchXing_lo);

    token        = swap16(ed.TrgToken);
    triggerWord  = swap16(ed.TriggerWord);
    physicsWord  = swap16(ed.physicsWord);

    memcpy(ZDC, trgDet.ZDC, 16);
    memcpy(CTB, trgDet.CTB, 256);

    l2Result = sumData.L2Sum[0];
    
    return 0;
}
