#include "StChargedPionHeader.h"

ClassImp(StChargedPionHeader)

StChargedPionHeader::StChargedPionHeader() { /* no-op */ }

StChargedPionHeader::~StChargedPionHeader() { /* no-op */ }

float StChargedPionHeader::prescale(int trigId) {
    map<unsigned int, float>::const_iterator it = mTriggerPrescales.find(trigId);
    if(it == mTriggerPrescales.end()) return -1.0;
    return it->second;
}