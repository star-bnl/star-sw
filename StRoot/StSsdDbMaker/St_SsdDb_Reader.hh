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

class ssdConfiguration_st;
class ssdWafersPosition_st;
class ssdDimensions_st;
class St_ssdWafersPosition;
class St_ssdConfiguration;
class St_ssdDimensions;
class TString;

class St_SsdDb_Reader 
{
 private:
  St_DataSet* ssdDb[2];        //!
  Text_t *mTimeStamp;            //!
  Int_t   mUnixTimeStamp;
  StSsdConfig* mSsdConfig;      //!
  StSsdGeometry* mSsdGeom;      //!
  St_ssdWafersPosition* mWafersPosition ;
  St_ssdConfiguration* mSsdConfiguration ;
  St_ssdDimensions* mSsdDimensions ;

  ssdWafersPosition_st* mWP ;
  ssdConfiguration_st* mSC ;
  ssdDimensions_st* mSD ;


  
 protected:

 public: 
  St_SsdDb_Reader();
  virtual ~St_SsdDb_Reader();

  void setDataBase(St_DataSet* input, dbSsdType type);
  void setWafersPosition(ssdWafersPosition_st *geom);
  void setWafersPosition(St_ssdWafersPosition* mssdWafersPosition);
  ssdWafersPosition_st* getWafersPosition();
  void setSsdConfiguration(St_ssdConfiguration* mSsdConfiguration);
  void setSsdConfiguration(ssdConfiguration_st *config);
  ssdConfiguration_st* getSsdConfiguration();
  void setSsdDimensions(St_ssdDimensions* mSsdDimensions);
  void setSsdDimensions(ssdDimensions_st *dimensions);
  ssdDimensions_st* getSsdDimensions();

  StSsdConfig* getConfiguration(ssdConfiguration_st *config);
  StSsdConfig* getConfiguration(St_ssdConfiguration* ssdConfiguration);
  StSsdGeometry* getGeometry(ssdWafersPosition_st *geom);
  StSsdGeometry* getGeometry(St_ssdWafersPosition* wafersPosition);
  StSsdGeometry* getDimensions(ssdDimensions_st *dimensions);
  StSsdGeometry* getDimensions(St_ssdDimensions* ssdDimensions);

#ifdef __ROOT__
  ClassDef(St_SsdDb_Reader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


