/***************************************************************************
 *
 * $Id: StSvtT0.hh,v 1.1 2003/04/14 15:48:25 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT t0 object. It makes the link with the Data Base
 *
 ***************************************************************************
 *
 * $Log: StSvtT0.hh,v $
 * Revision 1.1  2003/04/14 15:48:25  munhoz
 * adding t0 object
 *
 *
 **************************************************************************/

#ifndef STSVTT0_HH
#define STSVTT0_HH

#include "StObject.h"

class StSvtConfig;
class svg_geom_st;
class svg_shape_st;
class srs_srspar_st;

class StSvtT0: public StObject
{
public:
  StSvtT0();
  virtual ~StSvtT0();

  StSvtT0(const StSvtT0&);
  StSvtT0& operator = (const StSvtT0&);

  void setT0(double x, int readoutBox){t0[readoutBox-1]=x;}
  void setFsca(double x){fsca=x;}

  double getT0(int readoutBox=1){return t0[readoutBox-1];}
  double getFsca(){return fsca;}

private:

  double t0[24];     //cm 
  double fsca;

  ClassDef(StSvtT0,1)
};

#endif
