/***************************************************************************
 *
 * $Id: StSvtDbReader.hh,v 1.7 2004/07/31 00:50:27 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbReader.hh,v $
 * Revision 1.7  2004/07/31 00:50:27  munhoz
 * adding anode drift veloc correction factor
 *
 * Revision 1.6  2004/07/26 00:06:08  munhoz
 * read drift curve
 *
 * Revision 1.5  2004/03/30 21:16:18  caines
 * Get daq parameters
 *
 * Revision 1.4  2004/01/30 07:22:07  munhoz
 * adding rms and daq parameters reading
 *
 * Revision 1.3  2003/04/14 15:51:49  munhoz
 * reading t0 from DB
 *
 * Revision 1.2  2002/02/15 22:45:43  munhoz
 * introducing drift velocity reading capability
 *
 * Revision 1.1  2001/10/29 18:53:13  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#ifndef STSVTDBREADER_H
#define STSVTDBREADER_H

#include "StDbLib/StDbDefs.hh"                         //

#ifdef __ROOT__
#include "TROOT.h"                         //
#endif

class StDbManager;
class StDbConfigNode;

class StSvtHybridCollection;
class StSvtHybridDriftVelocity;
class StSvtConfig;
class StSvtGeometry;
class StSvtT0;
class StSvtDaq;

class StSvtDbReader 
{
 private:
  char *mTimeStamp;             //!
  int   mUnixTimeStamp;

  StDbManager* mDbMgr;          //!

  StDbConfigNode* mConfigCalib; //!
  StDbConfigNode* mConfigGeom;  //!
  StDbConfigNode* mConfigCond;  //!

  StSvtConfig* mSvtConfig;      //!

 protected:

 public: 
  StSvtDbReader(Text_t *timestamp);
  StSvtDbReader(Int_t timestamp = 0);
  virtual ~StSvtDbReader();

  void setDbManager();
  void setTimeStamp(char *timestamp) {mTimeStamp = timestamp;}
  void setTimeStamp(int timestamp) {mUnixTimeStamp = timestamp;}

  StSvtConfig* getConfiguration();
  StSvtHybridCollection* getDriftVelocity();
  StSvtHybridCollection* getDriftCurve();
  StSvtHybridCollection* getAnodeDriftCorr();
  StSvtHybridCollection* getPedestals();
  StSvtHybridCollection* getRms();
  StSvtHybridCollection* getBadAnodes();
  StSvtGeometry* getGeometry();
  StSvtT0* getT0(){return 0;}
  StSvtDaq* getDaqParameters(){return 0;}

#ifdef __ROOT__
  ClassDef(StSvtDbReader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


