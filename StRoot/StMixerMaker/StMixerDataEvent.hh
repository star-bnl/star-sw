/***************************************************************************
 *
 * $Id: StMixerDataEvent.hh,v 1.1 2000/02/16 21:02:08 pfachini Exp $
 *
 * Author: Patricia Fachini
 *
 ***************************************************************************
 *
 * Description: Contains a complete event of raw data
 *
 ***************************************************************************
 *
 **************************************************************************/
#ifndef ST_MIXER_DATA_EVENT_HH
#define ST_MIXER_DATA_EVENT_HH

#include <vector>

// FROM SCL
#include "StTpcRawDataEvent.hh"

// FROM Mixer
#include "StMixerDigitalSector.hh"

class StMixerDataEvent : public StTpcRawDataEvent {
public:
    StMixerDataEvent(int);
    virtual ~StMixerDataEvent();
    
    unsigned long size();
    
    void          clear();
    
public:
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StMixerDigitalSector*> mSectors;
#else
    vector<StMixerDigitalSector*, allocator<StMixerDigitalSector*> > mSectors;
#endif
};

#endif
