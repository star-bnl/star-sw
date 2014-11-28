// $Id: StFgtDbMaker.h,v 1.19 2014/08/06 11:43:09 jeromel Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske

*/

#ifndef STFGTDBMAKER_H
#define STFGTDBMAKER_H


#include "StMaker.h"
#include "StFgtDb.h"
#include "StFgtUtil/geometry/StFgtGeom.h"

class fgtElosCutoff_st;
class fgtSimuParams_st;

class StFgtDbMaker : public StMaker {
 
 public: 
  StFgtDbMaker(const char *name="fgtDb");
  ~StFgtDbMaker();
  Int_t  Init();
  Int_t  InitRun(Int_t runNumber);
  Int_t  Make();
  Int_t  Finish();
  StFgtDb * getDbTables() { return m_tables; }
  void   Clear(const char *opt);
  void  printFgtDumpCSV1(TString fname) { m_tables->printFgtDumpCSV1(fname,GetDate(), GetTime());  }

  map<string, pair<string, string> > mValidRanges;
  void displayBeginEndTime(TTable* table);

  //  You should call getDbTables after calling this with "ideal".  If you do
  //  not, then your StFgtDb object will be out of date.
  void setFlavor( const char *flav, const char *tabname );

  Float_t eLossTab(int bin); //  built from BichselELossProbHighBG.dat used to reject very high and unrealistic loss value
  Double_t simuParams(int bin); // initialization of fgt-slow-simulator
 
  StFgtGeom *getFgtGeom(){ return m_geom;} 

private:

  fgtElosCutoff_st *m_LossTab;
  fgtSimuParams_st *m_simuParTab;
  StFgtGeom        *m_geom;
  StFgtDb          *m_tables;
  fgtMapping_st    *m_rmap;
  fgtAlignment_st  *m_alignment;

 public:

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtDbMaker.h,v 1.19 2014/08/06 11:43:09 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StFgtDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif

// $Log: StFgtDbMaker.h,v $
// Revision 1.19  2014/08/06 11:43:09  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.18  2013/01/31 15:42:19  akio
// Adding Alignment table and getStarXYZ()
//
// Revision 1.17  2012/07/31 18:24:30  jeromel
// Removed virtual + fixed name
//
// Revision 1.16  2012/06/03 16:52:18  balewski
// added access to fgtSimuParam table, all I/O .C code is saved in macros
//
// Revision 1.15  2012/03/19 01:19:20  rfatemi
// modified for removal of StFgtDbImpl and StFgtIdealDbImpl
//
// Revision 1.14  2012/03/12 16:21:16  rfatemi
// make sure getCVS() is public
//
// Revision 1.13  2012/03/10 01:59:29  rfatemi
// Review comments
//
// Revision 1.12  2012/02/24 16:20:31  rfatemi
// Correct mistype in printFgtDumpCSV1
//
// Revision 1.11  2012/02/22 20:07:44  rfatemi
// Changed name from updateValidity to displayBeginEndTime
//
// Revision 1.10  2012/02/22 04:04:29  rfatemi
// Added beginTimes for each table
//
// Revision 1.9  2012/01/31 21:07:53  rfatemi
// changes for StFgtDbIdealImpl.h
//
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
