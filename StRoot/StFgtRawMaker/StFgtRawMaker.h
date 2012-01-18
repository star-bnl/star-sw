// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id: StFgtRawMaker.h,v 1.17 2012/01/18 03:10:51 avossen Exp $
//  $Log: StFgtRawMaker.h,v $
//  Revision 1.17  2012/01/18 03:10:51  avossen
//  added db access to the raw maker
//
//  Revision 1.16  2011/11/01 18:45:32  sgliske
//  Updated to correspond with StEvent containers, take 2.
//  Note: new FGT containers (and StEvent access) no longer
//  motivate the use of a common base class
//
//  Revision 1.15  2011/10/26 20:57:49  avossen
//  hopefully made cosmic and raw maker compatible with bfc (again), added clear in make. Unnecessary if member fkt clear() is called after every event
//
//  Revision 1.14  2011/09/21 17:49:34  sgliske
//  alternate base class with more
//   functionality and not an StMaker
//
//  Revision 1.11  2011/09/20 15:53:09  sgliske
//  Update so that everything compiles nicely
//  and so that one can execute the macro/simpleTestStandTest.C file
//
//
//
//
#ifndef STAR_StFgtRawMaker_HH
#define STAR_StFgtRawMaker_HH

#include <math.h>

#include <TStopwatch.h>
#include <TString.h>

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"
#include "StRoot/StChain/StRTSBaseMaker.h"

class StFgtCollection;
class StFgtDb;
//#include "StRoot/StEvent/StEventTypes.h"
//#include <StDaqLib/GENERIC/EventReader.hh>
//#include <StDAQMaker/StDAQReader.h>
//#include "StRoot/StFgtUtil/database/StFgtDb.h"


class StFgtRawMaker : public StRTSBaseMaker
{
 public: 
  StFgtRawMaker(const Char_t* name="FgtRaw");
  virtual ~StFgtRawMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );
  virtual Int_t FillHits();
  virtual Int_t PrepareEnvironment();

  void setFgtDb(StFgtDb *x) {fgtDb=x;}

 protected:
  StFgtCollection *mFgtCollectionPtr;

 private:
  StFgtDb *fgtDb;
  ClassDef(StFgtRawMaker,1);
};

#endif
