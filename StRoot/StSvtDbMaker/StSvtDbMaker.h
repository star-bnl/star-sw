/***************************************************************************
 *
 * $Id: StSvtDbMaker.h,v 1.1 2001/10/29 18:53:13 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbMaker.h,v $
 * Revision 1.1  2001/10/29 18:53:13  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#ifndef STSVTDBMAKER_H
#define STSVTDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StChain;

class St_SvtDb_Reader;
class StSvtDbReader;
class StSvtDbWriter;
class StSvtHybridCollection;
class StSvtGeometry;

class StSvtDbMaker : public StMaker {
 private:
  Text_t *mTimeStamp;        //!
  Int_t   mUnixTimeStamp;    //!

  St_SvtDb_Reader *m_Reader; //!
  StSvtDbReader *mReader;    //!
  StSvtDbWriter *mWriter;    //!

 protected:

 public: 
  StSvtDbMaker(const char *name="SvtDb");
  virtual       ~StSvtDbMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);

  void setTimeStamp(Text_t *timestamp) {mTimeStamp = timestamp;}
  void setUnixTimeStamp(Int_t timestamp) {mUnixTimeStamp = timestamp;}
  void setSvtDb_Reader();
  void setSvtDbReader(Text_t *timestamp);
  void setSvtDbReader(int unixTime);
  void setSvtDbWriter(Text_t *timestamp);
  void setSvtDbWriter(int unixTime);

  void readSvtConfig();
  void readSvtDriftVelocity();
  void readSvtPedestals();
  void readSvtGeometry();

  void writeSvtDriftVelocity(StSvtHybridCollection* driftVeloc=0);
  void writeSvtPedestals(StSvtHybridCollection* pedestals=0);

  St_SvtDb_Reader* get_SvtDb_Reader(){return m_Reader;}
  StSvtDbReader* getSvtDbReader(){return mReader;}
  StSvtDbWriter* getSvtDbWriter(){return mWriter;}  

  ClassDef(StSvtDbMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


