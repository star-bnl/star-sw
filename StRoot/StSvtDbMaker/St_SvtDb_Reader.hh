/***************************************************************************
 *
 * $Id: St_SvtDb_Reader.hh,v 1.1 2001/10/29 18:53:14 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: St_SvtDb_Reader.hh,v $
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
  StSvtHybridCollection* getPedestals();
  StSvtGeometry* getGeometry();

#ifdef __ROOT__
  ClassDef(St_SvtDb_Reader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


