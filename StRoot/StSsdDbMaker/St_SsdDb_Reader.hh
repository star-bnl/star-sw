/***************************************************************************
 *
 *  St_SsdDb_Reader.hh,v 1.3 
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD DB access Maker
 *
 **************************************************************************/

#ifndef ST_SSDDB_READER_H
#define ST_SSDDB_READER_H

#include "StSsdUtil/StSsdEnumerations.hh"

#ifdef __ROOT__
#include "TROOT.h"                         //
#endif
#include "St_DataSet.h"

class StSsdHybridCollection;
class StSsdConfig;
class StSsdGeometry;

class St_svg_geom;
class svg_geom_st;
class TString;

class St_SsdDb_Reader 
{
 private:
  St_DataSet* ssdDb[2];        //!

  StSsdConfig* mSsdConfig;      //!
  StSsdGeometry* mSsdGeom;      //!
  St_svg_geom* mSvgGeom ;

 protected:

 public: 
  St_SsdDb_Reader();
  virtual ~St_SsdDb_Reader();

  void setDataBase(St_DataSet* input, dbSsdType type);
  void setSvgGeom(St_svg_geom* aSvgGeom);
  St_svg_geom* getSvgGeom();

  StSsdHybridCollection* getPedestals();
  StSsdConfig* getConfiguration();
  StSsdGeometry* getGeometry(St_svg_geom* SsdGeom);
  StSsdGeometry* getGeometry();

#ifdef __ROOT__
  ClassDef(St_SsdDb_Reader, 1)   //StAF chain virtual base class for Makers
#endif
};

#endif


