/***************************************************************************
 *
 * $Id: StDbCalibFactory.hh,v 1.2 1999/09/30 02:06:02 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Calibration tables
 *
 ***************************************************************************
 *
 * $Log: StDbCalibFactory.hh,v $
 * Revision 1.2  1999/09/30 02:06:02  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBCalIBFACTORY_HH
#define STDBCalIBFACTORY_HH

#include "StDbFactoryI.hh"

class StDbCalibFactory : public StDbFactoryI {

protected:

  StDbCalibFactory(){ mdbType = Calibrations; };
  virtual void initIDList();

static StDbCalibFactory* mInstance;

public:

  static StDbCalibFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbCalibFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbCalibFactory(){ deleteIDList(); };

};

#endif









