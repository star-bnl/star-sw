/***************************************************************************
 *
 * $Id: StDbRunParamsFactory.hh,v 1.2 1999/09/30 02:06:08 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  StDbTable ctor & list holder for Run Params tables
 *
 ***************************************************************************
 *
 * $Log: StDbRunParamsFactory.hh,v $
 * Revision 1.2  1999/09/30 02:06:08  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBRunParamsFACTORY_HH
#define STDBRunParamsFACTORY_HH

#include "StDbFactoryI.hh"

class StDbRunParamsFactory : public StDbFactoryI {

protected:

  StDbRunParamsFactory(){ mdbType = RunParams; };
  void initIDList();

static StDbRunParamsFactory* mInstance;

public:

  static StDbRunParamsFactory* Instance(){
    if(!mInstance){
      mInstance = new StDbRunParamsFactory;
    }
  return mInstance;
  }
  
  virtual ~StDbRunParamsFactory(){ deleteIDList(); };

};

#endif









