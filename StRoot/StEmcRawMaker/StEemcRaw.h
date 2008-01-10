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

class StEEmcDbMaker;
class StEEMCReader ;
class TH1F;
class StEvent;

class StEemcRaw :  public TObject
{
private:

    StEEmcDbMaker *mDb;
    TH1F *hs[8];
    Bool_t   copyRawData(StEEMCReader *eeReader, StEmcRawData *raw);
    Bool_t   headersAreSick(StEEMCReader *eeReader, StEmcRawData *raw, int token, int runId);
    Bool_t   towerDataAreSick(StEmcRawData* raw);
    void     raw2pixels(StEvent* mEvent);

protected:
public:
    StEemcRaw();
    ~StEemcRaw();
    Bool_t make(StEEMCReader *eeReader,StEvent* mEvent);
    void initHisto();

    void setDb(StEEmcDbMaker *aa)
    {
        mDb=aa;
    } ///< DB-reader must exist

    ClassDef(StEemcRaw,0)
};

#endif

// $Id: StEemcRaw.h,v 1.5 2008/01/10 20:49:59 balewski Exp $

/*
 * $Log: StEemcRaw.h,v $
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

