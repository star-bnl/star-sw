/***************************************************************************
 *
 * $Id: StMixerOstream.hh,v 1.1 2000/02/16 21:02:10 pfachini Exp $
 *
 * Author: 
 * 
 ***************************************************************************
 *
 * Description: Ostream class for
 *              writeing TRS data.
 ***************************************************************************
 *
 **************************************************************************/
#ifndef ST_MIXER_OSTREAM_HH
#define ST_MIXER_OSTREAM_HH

#include "StMixerIos.hh"
class StMixerDataEvent;
class StTpcGeometry;

class StMixerOstream : public StMixerIos {

public:
    StMixerOstream(string, int, StTpcGeometry*);
    ~StMixerOstream();

    void writeMixerEvent(StMixerDataEvent* EventData);
    
private:
    
    ofstream ofs;
    
};
#endif
