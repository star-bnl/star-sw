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

class StEEmcDataMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: StEEmcDataMaker.h,v 1.1 2003/04/25 14:15:59 jeromel Exp $";
  
  StEEmcDbMaker * mDb; ///< to assess DB
  
 protected:
 public: 
  StEEmcDataMaker(const char *name="St2eemcFeeRaw");
  virtual       ~StEEmcDataMaker();

  void setDb(StEEmcDbMaker *aa){mDb=aa;} ///< DB-reader must exist
  virtual Int_t Init();
  virtual Int_t InitRun  (int runumber);///< to change time stamp in TTree
  virtual Int_t  Make();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEmcDataMaker.h,v 1.1 2003/04/25 14:15:59 jeromel Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  ClassDef(StEEmcDataMaker, 1) 
};

#endif

// $Id: StEEmcDataMaker.h,v 1.1 2003/04/25 14:15:59 jeromel Exp $
// $Log: StEEmcDataMaker.h,v $
// Revision 1.1  2003/04/25 14:15:59  jeromel
// Reshaped Jan's code
//
// Revision 1.1  2003/03/25 18:30:14  balewski
// towards EEMC daq reader
//
