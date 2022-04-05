/***************************************************************************
 *
 * $Id: StSvtDbMaker.h,v 1.15 2014/08/06 11:43:45 jeromel Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT DB access Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDbMaker.h,v $
 * Revision 1.15  2014/08/06 11:43:45  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.14  2007/07/31 16:38:11  fisyak
 * Make request for SvtGeometry from GetRotations
 *
 * Revision 1.13  2007/07/12 20:07:49  fisyak
 * Move to access on demand of Db tables
 *
 * Revision 1.12  2007/03/21 17:23:24  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.11  2004/07/31 00:50:23  munhoz
 * adding anode drift veloc correction factor
 *
 * Revision 1.10  2004/07/26 00:06:08  munhoz
 * read drift curve
 *
 * Revision 1.9  2004/03/30 21:16:18  caines
 * Get daq parameters
 *
 * Revision 1.8  2004/01/30 07:22:07  munhoz
 * adding rms and daq parameters reading
 *
 * Revision 1.7  2003/09/10 19:47:36  perev
 * ansi corrs
 *
 * Revision 1.6  2003/04/14 15:51:45  munhoz
 * reading t0 from DB
 *
 * Revision 1.5  2003/02/19 11:03:59  munhoz
 * adding getCVS line
 *
 * Revision 1.4  2003/01/28 20:20:10  munhoz
 * including InitRun()
 *
 * Revision 1.3  2002/02/20 17:10:10  caines
 * Added fortran2c code from StDbUtilities so library depedancies removed
 *
 * Revision 1.2  2002/02/15 22:45:43  munhoz
 * introducing drift velocity reading capability
 *
 * Revision 1.1  2001/10/29 18:53:13  munhoz
 * starting SVT Data base
 *
 *
 **************************************************************************/

#ifndef STSVTDBMAKER_H
#define STSVTDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StChain;

class StSvtHybridCollection;
class StSvtHybridDriftVelocity;
class StSvtHybridDriftCurve;
class StSvtConfig;
class StSvtGeometry;
class StSvtT0;
class StSvtDaq;
#include "THashList.h"
class TGeoHMatrix;


#ifndef __CINT__
#include "StarCallf77.h"
#define SvtLtoG_ F77_NAME(svtltog,SVTLTOG)
#define SvtGtoL_ F77_NAME(svtgtol,SVTGTOL)
extern "C" {
int type_of_call SvtLtoG_(float *x, float *xp, int *index);
int type_of_call SvtGtoL_(float *x, float *xp,  int *index);

}
#endif
class StSvtDbMaker : public StMaker {
 private:
  static THashList *fRotList;
 public: 
  StSvtDbMaker(const char *name="SvtDb");
  virtual       ~StSvtDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(int runumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);
  virtual THashList *GetRotations();
  StSvtHybridDriftVelocity* getDriftVelocity(int barrel, int ladder, int wafer, int hybrid);
  StSvtConfig*           getConfiguration();
  StSvtHybridCollection* getDriftVelocity();
  void                   getDriftVelocityAverage(StSvtHybridCollection* svtColl);
  StSvtHybridCollection* getDriftCurve();
  StSvtHybridCollection* getAnodeDriftCorr();
  StSvtHybridCollection* getPedestals();
  StSvtHybridCollection* getRms();
  StSvtGeometry*         getGeometry();
  StSvtHybridCollection* getBadAnodes();
  int                    getElectronics();
  StSvtT0*               getT0();
  StSvtDaq*              getDaqParameters();
  static THashList *RotMatrices() {return fRotList;}
 protected:
  virtual TDataSet  *FindDataSet (const char* logInput,
				  const StMaker *uppMk=0,
				  const StMaker *dowMk=0) const ;

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSvtDbMaker.h,v 1.15 2014/08/06 11:43:45 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  ClassDef(StSvtDbMaker,0)   //StAF chain virtual base class for Makers
};

// Global pointers:
R__EXTERN StSvtDbMaker* gStSvtDbMaker;

#endif


