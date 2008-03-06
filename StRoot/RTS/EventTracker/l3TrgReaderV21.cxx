#include "l3TrgReader.h"
#include "trgStructures.h"

#include "l3Swap.h"

#include <string.h>

int l3TrgReader::readV21(void *buffer)
{
    TrgDataType *trg = (TrgDataType *)buffer;

    bunchXing_hi = swap32(trg->EvtDesc.bunchXing_hi);
    bunchXing_lo = swap32(trg->EvtDesc.bunchXing_lo);

    token = swap16(trg->EvtDesc.TrgToken);
    triggerWord = swap16(trg->EvtDesc.TriggerWord);
    physicsWord = swap16(trg->EvtDesc.physicsWord);

    memcpy(ZDC, trg->rawTriggerDet[0].ZDC, 16);
    memcpy(CTB, trg->rawTriggerDet[0].CTB, 256);

    l2Result = swap32(trg->TrgSum.L2Sum[0]);

    return 0;
}
