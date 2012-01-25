// $Id: StFgtDbMaker.h,v 1.8 2012/01/25 03:56:56 balewski Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske

*/

#ifndef STFGTDBMAKER_H
#define STFGTDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

//#include "database/fgtGain.h"
//#include "StFgtUtil/database/fgtMapping.h"
//#include "StFgtUtil/database/fgtPedestal.h"
//#include "StFgtUtil/database/fgtStatus.h"
#include "StFgtDb.h"
#include "StFgtDbImpl.h"
#include "StFgtDbNaiveImpl.h"
#include "StFgtUtil/geometry/StFgtGeom.h"

class fgtElosCutoff_st;

class StFgtDbMaker : public StMaker {
 private:
  fgtElosCutoff_st *mLossTab;
  StFgtGeom *geom;
  StFgtDb * m_tables;
  fgtMapping_st * m_rmap;
  bool	    m_isIdeal;

 public: 
  StFgtDbMaker(const char *name="FgtDb");
  virtual       ~StFgtDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual StFgtDb * getDbTables() { return m_tables; }
  virtual void   Clear(const char *opt);
  void  printFgtDumpCSV(TString fname) { m_tables->printFgtDumpCSV1(fname,GetDate(), GetTime());  }

  virtual void SetFlavor( const char *flav, const char *tabname );

  Float_t eLossTab(int bin); //  built from BichselELossProbHighBG.dat used to reject very high and unrealistic loss value
  StFgtGeom *getFgtGeom(){ return geom;} 

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtDbMaker.h,v 1.8 2012/01/25 03:56:56 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StFgtDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif

// $Log: StFgtDbMaker.h,v $
// Revision 1.8  2012/01/25 03:56:56  balewski
// printing out DB dump
//
// Revision 1.7  2011/12/01 23:09:15  avossen
// moved StFgtUtil/database to StFgtDbMaker
//
// Revision 1.6  2011/11/13 23:51:49  wwitzke
// Modified StFgtDbMaker to pull calibration data from the database.
//
// Revision 1.5  2011/10/26 19:32:34  balewski
// now fgt-geom is owned by fgtDb-maker
//
// Revision 1.4  2011/10/06 19:03:58  balewski
// access Elos table from STAR DB
//
// Revision 1.2  2011/10/04 02:59:34  balewski
// added guestimates of gains, grid absorption, charge sharing
//
