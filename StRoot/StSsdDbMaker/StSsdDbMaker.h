/***************************************************************************
 *
 * $Id: StSsdDbMaker.h,v 1.1 2004/03/12 04:56:55 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD DB access Maker
 *
 **************************************************************************/

#ifndef STSSDDBMAKER_H
#define STSSDDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StChain;

class St_SsdDb_Reader;
class StSsdDbReader; 
class StSsdHybridCollection;
class StSsdGeometry;
class svg_geom_st;

/* #ifndef __CINT__ */
/* #include "StarCallf77.h" */
/* #define SsdLtoG_ F77_NAME(ssdltog,SSDLTOG) */ 
/* #define SsdGtoL_ F77_NAME(ssdgtol,SSDGTOL) */ 
/* extern "C" { */
/* int type_of_call SsdLtoG_(float *x, float *xp, int *index); */
/* int type_of_call SsdGtoL_(float *x, float *xp,  int *index); */
/* } */
/* #endif */

class StSsdDbMaker : public StMaker {
 private:
  Text_t *mTimeStamp;        //!
  Int_t   mUnixTimeStamp;    //!

  St_SsdDb_Reader *m_Reader; //!
  //  StSsdDbReader *mReader;    //! 

 protected:

 public: 
  StSsdDbMaker(const char *name="SsdDb");
  virtual       ~StSsdDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(int runumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);

  void setTimeStamp(Text_t *timestamp) {mTimeStamp = timestamp;}
  void setUnixTimeStamp(Int_t timestamp) {mUnixTimeStamp = timestamp;}
  void setSsdDb_Reader();

  void setSsdConfig();
  void readSsdConfig();
  void setSsdGeometry();
  void readSsdGeometry();
  void setSsdPedestals();
  void readSsdPedestals();

  St_SsdDb_Reader* get_SsdDb_Reader(){return m_Reader;}
  //  StSsdDbReader* getSsdDbReader(){return mReader;}

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSsdDbMaker.h,v 1.1 2004/03/12 04:56:55 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StSsdDbMaker,0)   //StAF chain virtual base class for Makers
};

// Global pointers:
R__EXTERN StSsdDbMaker* gStSsdDbMaker;

#endif


