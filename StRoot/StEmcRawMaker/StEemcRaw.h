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
//class StEmcRawData;
class TH1;
class StEvent;

class StEemcRaw :  public TObject {
 private:

  StEEmcDbMaker *mDb;
  //TH1F *hs[8];
  Bool_t   copyRawData(StEEMCReader *eeReader, StEmcRawData *raw);
  Bool_t   headersAreSick( StEmcRawData *raw, int token);
  Bool_t  towerDataAreSick(StEmcRawData* raw);
  void    raw2pixels(StEvent* mEvent);

 protected:
 public: 
  StEemcRaw();
  ~StEemcRaw();
  Bool_t make(StEEMCReader *eeReader,StEvent* mEvent);


  void setDb(StEEmcDbMaker *aa){mDb=aa;} ///< DB-reader must exist
  
  ClassDef(StEemcRaw,0) 
};

#endif

// $Id: StEemcRaw.h,v 1.1 2004/10/19 23:48:49 suaide Exp $

/*
 * $Log: StEemcRaw.h,v $
 * Revision 1.1  2004/10/19 23:48:49  suaide
 * Initial implementation of the endcap detector done by Jan Balewski
 *
 *
 */

