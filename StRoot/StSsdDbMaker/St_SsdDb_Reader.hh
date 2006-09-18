// $Id: St_SsdDb_Reader.hh,v 1.6 2006/09/18 16:40:14 fisyak Exp $
//
// $Log: St_SsdDb_Reader.hh,v $
// Revision 1.6  2006/09/18 16:40:14  fisyak
// Add sim flag for ssdWafersPosition
//
// Revision 1.5  2005/06/20 14:21:39  lmartin
// CVS tags added
//

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
#include "TDataSet.h"

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
  TDataSet *ssdDb[2];        //!
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

  void setDataBase(TDataSet *input, int number);

  StSsdConfig* getConfiguration();
  StSsdGeometry* getGeometry();
  StSsdGeometry* getDimensions();

#ifdef __ROOT__
  ClassDef(St_SsdDb_Reader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


