/***************************************************************************
 *
 * $Id: St_SvtDb_Reader.hh,v 1.3 2003/04/14 15:51:55 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: St_SvtDb_Reader.hh,v $
 * Revision 1.3  2003/04/14 15:51:55  munhoz
 * reading t0 from DB
 *
 * Revision 1.2  2002/02/15 22:45:43  munhoz
 * introducing drift velocity reading capability
 *
 * Revision 1.1  2001/10/29 18:53:14  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#ifndef ST_SVTDB_READER_H
#define ST_SVTDB_READER_H

#include "StSvtClassLibrary/StSvtEnumerations.hh"

#ifdef __ROOT__
#include "TROOT.h"                         //
#endif

//class St_DataSet;
#include "St_DataSet.h"

class StSvtHybridCollection;
class StSvtHybridDriftVelocity;
class StSvtConfig;
class StSvtGeometry;
class StSvtT0;

class St_SvtDb_Reader 
{
 private:
  St_DataSet* svtDb[3];        //!

  StSvtConfig* mSvtConfig;      //!

 protected:

 public: 
  St_SvtDb_Reader();
  virtual ~St_SvtDb_Reader();

  void setDataBase(St_DataSet* input, dbSvtType type);

  StSvtConfig* getConfiguration();

  StSvtHybridCollection* getDriftVelocity();
  StSvtHybridDriftVelocity* getDriftVelocity(int barrel, int ladder, int wafer, int hybrid);
  void getDriftVelocityAverage(StSvtHybridCollection* svtColl);
  StSvtHybridCollection* getPedestals();
  StSvtGeometry* getGeometry();
  StSvtHybridCollection* getBadAnodes();
  int getElectronics();
  StSvtT0* getT0();

#ifdef __ROOT__
  ClassDef(St_SvtDb_Reader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


