/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#ifndef ST_SSDDB_READER_H
#define ST_SSDDB_READER_H

#include "StSsdUtil/StSsdEnumerations.hh"

#ifdef __ROOT__
#include "TROOT.h"                         //
#endif
#include "St_DataSet.h"

class StSsdConfig;
class StSsdGeometry;

class St_ssdWafersPosition;
class St_ssdConfiguration;
class St_ssdDimensions;
class TString;

class St_SsdDb_Reader 
{
 private:
  St_DataSet* ssdDb[2];        //!

  StSsdConfig* mSsdConfig;      //!
  StSsdGeometry* mSsdGeom;      //!
  St_ssdWafersPosition* mWafersPosition ;
  St_ssdConfiguration* mSsdConfiguration ;
  St_ssdDimensions* mSsdDimensions ;
  
 protected:

 public: 
  St_SsdDb_Reader();
  virtual ~St_SsdDb_Reader();

  void setDataBase(St_DataSet* input, dbSsdType type);
  void setWafersPosition(St_ssdWafersPosition* mssdWafersPosition);
  St_ssdWafersPosition* getWafersPosition();
  void setSsdConfiguration(St_ssdConfiguration* mSsdConfiguration);
  St_ssdConfiguration* getSsdConfiguration();
  void setSsdDimensions(St_ssdDimensions* mSsdDimensions);
  St_ssdDimensions* getSsdDimensions();

  StSsdConfig* getConfiguration();
  StSsdConfig* getConfiguration(St_ssdConfiguration* ssdConfiguration);
  StSsdGeometry* getGeometry();
  StSsdGeometry* getGeometry(St_ssdWafersPosition* wafersPosition);
  StSsdGeometry* getDimensions(St_ssdDimensions* ssdDimensions);

#ifdef __ROOT__
  ClassDef(St_SsdDb_Reader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


