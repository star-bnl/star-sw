/*!
  \class StEEmcDataMaker
  \author Jan Balewski
  \date   2003

  This maker reads the raw EEmc DAQ data and uses the StEmcRawHit
  class to save the hit information. Later handling of sabing in 
  StEvent is made by the Emc classes (HitCollection etc ...)

*/

#ifndef STAR_StEEmcDataMaker
#define STAR_StEEmcDataMaker


#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEEmcDbMaker;
class StEvent;
class TH1;

class StEEmcDataMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: StEEmcDataMaker.h,v 1.4 2004/04/03 06:32:45 balewski Exp $";
  
  StEEmcDbMaker * mDb; ///< to assess DB
  TH1F *hs[8];
 
  int   copyRawData(StEvent* mEvent);
  int   headersAreSick(StEvent* mEvent);
  int   towerDataAreSick(StEvent* mEvent);
  void  raw2pixels(StEvent* mEvent);

 protected:
 public: 
  StEEmcDataMaker(const char *name="St2eemcFeeRaw");
  virtual       ~StEEmcDataMaker();

  void setDb(StEEmcDbMaker *aa){mDb=aa;} ///< DB-reader must exist
  virtual Int_t Init();
  virtual Int_t InitRun  (int runumber);///< to change time stamp in TTree
  virtual Int_t  Make();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcDataMaker.h,v 1.4 2004/04/03 06:32:45 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEEmcDataMaker,0) 
};

#endif

// $Id: StEEmcDataMaker.h,v 1.4 2004/04/03 06:32:45 balewski Exp $
// $Log: StEEmcDataMaker.h,v $
// Revision 1.4  2004/04/03 06:32:45  balewski
// firts attempt to store EEMC hits in StEvent & muDst,
// Implemented useIt for fibers in Db
// problems: - tower sector 12 is missing
// - no pres & msd in smd
//
// Revision 1.3  2004/04/02 06:38:44  balewski
// abort on any error in any header
//
// Revision 1.2  2003/09/10 19:47:07  perev
// ansi corrs
//
// Revision 1.1  2003/04/25 14:15:59  jeromel
// Reshaped Jan's code
//
// Revision 1.1  2003/03/25 18:30:14  balewski
// towards EEMC daq reader
//


/*
 * $Log: StEEmcDataMaker.h,v $
 * Revision 1.4  2004/04/03 06:32:45  balewski
 * firts attempt to store EEMC hits in StEvent & muDst,
 * Implemented useIt for fibers in Db
 * problems: - tower sector 12 is missing
 * - no pres & msd in smd
 *
 * Revision 1.3  2004/04/02 06:38:44  balewski
 * abort on any error in any header
 *
 */

