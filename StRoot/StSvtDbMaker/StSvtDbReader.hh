/***************************************************************************
 *
 * $Id: StSvtDbReader.hh,v 1.2 2002/02/15 22:45:43 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbReader.hh,v $
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
  StSvtHybridCollection* getPedestals();
  StSvtHybridCollection* getBadAnodes();
  StSvtGeometry* getGeometry();

#ifdef __ROOT__
  ClassDef(StSvtDbReader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


