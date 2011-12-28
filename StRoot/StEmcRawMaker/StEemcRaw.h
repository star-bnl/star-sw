/*!
  \class StEEmcDataMaker
  \author Jan Balewski
  \date   2003,2004
 
  This maker reads the raw EEmc DAQ data and uses the StEmcRawHit
  class to save the hit information. Later handling of hits in 
  StEvent is made by the Emc classes (HitCollection etc ...)
 
*/

#ifndef STAR_StEemcRaw
#define STAR_StEemcRaw

#include "TObject.h"

class StEEmcDb;
class StEEMCReader ;
class TH1F;
class StEvent;
class StEmcRawMaker;

class StEemcRaw :  public TObject
{
private:

    StEEmcDb *mDb;
    TH1F *hs[8];
    Bool_t   copyRawData(StEEMCReader *eeReader, StEmcRawData *raw);
    Bool_t copyRawData(StEmcRawMaker* maker, StEmcRawData *raw);
    Bool_t headersAreSick(StEmcRawMaker* maker, StEmcRawData *raw, int token, int runId, int time);
    Bool_t   headersAreSick(StEEMCReader *eeReader, StEmcRawData *raw, int token, int runId, int time);
    Bool_t   towerDataAreSick(StEmcRawData* raw);
    void     raw2pixels(StEvent* mEvent);

protected:
public:
    StEemcRaw();
    ~StEemcRaw();
    Bool_t make(StEEMCReader *eeReader,StEvent* mEvent);
    Bool_t make(StEmcRawMaker* maker, StEvent* mEvent);
    void initHisto();

    void setDb(StEEmcDb *aa)
    {
        mDb=aa;
    } ///< DB-reader must exist

    ClassDef(StEemcRaw,0)
};

#endif

// $Id: StEemcRaw.h,v 1.8 2011/01/04 19:04:08 stevens4 Exp $

/*
 * $Log: StEemcRaw.h,v $
 * Revision 1.8  2011/01/04 19:04:08  stevens4
 * added event time to EEMC header check
 *
 * Revision 1.7  2009/02/04 21:05:42  kocolosk
 * Refactor StEEmcDb(Maker), new location for StEmcDecoder. Fixes RT #1388.
 *
 * Revision 1.6  2009/01/27 19:58:36  mattheww
 * Updates to StEmcRawMaker to be compatible with 2009 DAQ Format
 *
 * Revision 1.5  2008/01/10 20:49:59  balewski
 * now more warnings if ESMD is not in the run, thanks Pibero
 *
 * Revision 1.4  2006/01/16 11:12:00  suaide
 * tower map bug fixed and astyle run
 *
 * Revision 1.3  2005/02/03 02:35:11  balewski
 * accomodate MAPMT firmware change in 2005
 *
 * Revision 1.2  2004/10/21 00:01:50  suaide
 * small changes in histogramming and messages for BEMC
 * Complete version for EEMC done by Jan Balewski
 *
 * Revision 1.1  2004/10/19 23:48:49  suaide
 * Initial implementation of the endcap detector done by Jan Balewski
 *
 *
 */

