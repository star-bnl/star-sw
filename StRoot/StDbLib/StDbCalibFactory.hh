/***************************************************************************
 *
 * $Id: StDbCalibFactory.hh,v 1.3 2000/01/10 20:37:53 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Calibration tables
 *
 ***************************************************************************
 *
 * $Log: StDbCalibFactory.hh,v $
 * Revision 1.3  2000/01/10 20:37:53  porter
 * expanded functionality based on planned additions or feedback from Online work.
 * update includes:
 * 	1. basis for real transaction model with roll-back
 * 	2. limited SQL access via the manager for run-log & tagDb
 * 	3. balance obtained between enumerated & string access to databases
 * 	4. 3-levels of diagnostic output: Quiet, Normal, Verbose
 * 	5. restructured Node model for better XML support
 *
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

  StDbCalibFactory(){ mdbType = dbCalibrations; };
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









