// $Id: StFtpcLaserTrafo.hh,v 1.2 2006/03/15 15:13:57 jcs Exp $
//
// $Log: StFtpcLaserTrafo.hh,v $
// Revision 1.2  2006/03/15 15:13:57  jcs
// add lines for listing CVS update info
//

#ifndef STAR_StFtpcLaserTrafo
#define STAR_StFtpcLaserTrafo

#include <Stiostream.h>
#include <fstream>
#include <cmath>

#include "StFtpcClusterMaker/StFtpcDbReader.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"
#include "TVector.h"
//#include "StThreeVector.hh"

#include "TObject.h"

class StFtpcLaserTrafo 
{

 private:
  
  Double_t *pradius; 
  Double_t *pdeflection;
  StFtpcDbReader *mDb;
  StFtpcParamReader *mParam;
  Float_t deltat0;
  Float_t deltagas;
  Float_t micropertime;
  Double_t deltap;
  Float_t mBField;
  Float_t tZero;
  
  // *********************************************
  // * corr. nur bei fullfield (+- geanuer !???) *
  // *********************************************

  Double_t vd_gas_slope(float rad);
  Double_t vd_gas_y(float rad);
  Double_t lor_gas_slope(float rad);
  Double_t lor_gas_y(float rad);
  
  //Double_t lor_gas(float rad);
  //Double_t vd_gas(float rad);

 protected:
   
 public: 
 
  StFtpcLaserTrafo(StFtpcDbReader *getdb,StFtpcParamReader *getparam,float getdeltat0,float getdeltagas, float getmicropertime, float getdeltap, float getmbfield, float getTZero);
  StFtpcLaserTrafo();
  virtual ~StFtpcLaserTrafo();

  //Double_t vd_gas(float rad, float gasmix);
  //Double_t lor_gas(float rad, float gasmix);
  Double_t vd_gas(float rad);
  Double_t lor_gas(float rad);
  virtual int calcpadtrans();
  virtual int padtrans(int iRow,int iSec,float timepos, float padpos,float *x1,float *y1);
};
#endif
