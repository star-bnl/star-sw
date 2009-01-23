#ifndef EEMCDB_h
#define EEMCDB_h

// $Id: EEmcDb.h,v 1.2 2009/01/23 00:14:50 ogrebeny Exp $

#include <TObject.h>

#include "StEEmcDbMaker/StEEmcDbMaker.h"

class EEmcDbItem;

class EEmcDb : public StEEmcDbMaker {
 protected:
  unsigned int timeStamp;
  DbFlavor dbFlavor;
  int mfirstSecID, mlastSecID, mNSector;

  int dbg; // dbg level

  // local fast look-up tables
  EEmcDbItem   *byIndex; //!  assess via plain index
  EEmcDbItem   ***byCrate; //! access via crate/chan
  int mxAdcCrate, mxAdcChan; // local copy of dimensions
  int nFound;
  float KsigOverPed; // defines threshold

  virtual void reloadDbConfig(int secID){;}
  virtual void reloadDbOthers(int secID){;}
  virtual void * getDbTable(int secID, const char *nameT){ return 0;} // pull a table out of DB

  void clearItemArray();

 public:
  EEmcDb(const char *name="EEmcDb");
  virtual ~EEmcDb(){;}
  void setDbg(int i){ dbg=i; }
  void setThreshold(float x);// defines threshold for ADCsold
  virtual void setPreferredFlavor(const char *flavor, const char *nameMask){;}

  virtual void requestDataBase(unsigned int timeStamp,int sec1=5, int sec2=8, const char *dbName="Calibrations_eemc", const char *dbVer="Ver2004c"){;}  // get the database tables

  void readAsciiDataBase(const char *fname,int sec1=5, int sec2=8); // get the database from an ascii file

  const  EEmcDbItem* getByIndex(int ikey); ///< returns full DB info for one pixel
  const  EEmcDbItem* getByCrate(int crateID, int channel); // full DB info, crateID counts from 1, channel from 0  

  int getFirstSecID(){ return mfirstSecID;}
  int getLastSecID(){ return mlastSecID;}

  void exportAscii(const char *fname="fixMe") const;
  virtual int scaleGain(const char* pixel, float factor){return 0;}

};

#endif

// $Log: EEmcDb.h,v $
// Revision 1.2  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.1  2009/01/18 01:01:28  ogrebeny
// Better separate EMC histogramming from OnlinePlots infrastructure
//
// Revision 1.2  2009/01/13 00:39:07  fine
// Add rootrc parameters
//
// Revision 1.1  2007/02/27 15:44:37  laue
// Initial version
//
// Revision 1.1  2006/10/04 20:31:44  laue
// Initial Version
//
// Revision 1.7  2004/02/26 04:21:43  balewski
// reading ASCII dump
//
// Revision 1.6  2003/12/10 04:42:33  balewski
// *** empty log message ***
//
// Revision 1.5  2003/12/08 22:19:21  zolnie
// allowed for gain
//
// Revision 1.4  2003/12/01 05:01:27  balewski
// DB & SMD
//
// Revision 1.3  2003/11/22 05:35:16  balewski
// *** empty log message ***
//
// Revision 1.2  2003/11/20 22:59:36  balewski
// cleanup
//
// Revision 1.1  2003/11/20 16:08:53  balewski
// start
//





