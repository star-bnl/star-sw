/***************************************************************************
 *
 * $Id: StSvtDbWriter.hh,v 1.1 2001/10/29 18:53:14 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbWriter.hh,v $
 * Revision 1.1  2001/10/29 18:53:14  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#ifndef STSVTDBWRITER_H
#define STSVTDBWRITER_H

#include "StDbLib/StDbDefs.hh"                         //

#ifdef __ROOT__
#include "TROOT.h"                         //
#endif

class StDbManager;
class StDbConfigNode;

class StSvtHybridCollection;
class StSvtHybridDriftVelocity;

class StSvtDbWriter
{
 private:
  Text_t *mTimeStamp;            //!
  Int_t   mUnixTimeStamp;

  StDbManager* mDbMgr;           //!

  StDbConfigNode* mConfigCalib;  //!
  StDbConfigNode* mConfigGeom;   //!
  StDbConfigNode* mConfigCond;   //!

 protected:

 public: 
  StSvtDbWriter(Text_t *timestamp);
  StSvtDbWriter(Int_t timestamp = 0);
  virtual ~StSvtDbWriter();

  void setDbManager();
  void setTimeStamp(Text_t *timestamp) {mTimeStamp = timestamp;}
  void setTimeStamp(Int_t timestamp) {mUnixTimeStamp = timestamp;}

  void addDriftVelocity(StSvtHybridCollection* svtDriftVeloc);
  void addPedestals(StSvtHybridCollection* svtPed);
  void addConfiguration();

#ifdef __ROOT__
  ClassDef(StSvtDbWriter, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


