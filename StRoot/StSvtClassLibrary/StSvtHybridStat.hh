/***************************************************************************
 *
 * $Id: StSvtHybridStat.hh,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pixel Statistic class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridStat.hh,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDSTAT_HH
#define STSVTHYBRIDSTAT_HH

#include "StSvtHybridObject.hh"

class StSvtHybridPixels;

class StSvtHybridStat: public StSvtHybridObject
{
public:
  StSvtHybridStat(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtHybridStat();

  StSvtHybridStat& operator = (const StSvtHybridStat&);

  StSvtHybridStat* addStat(StSvtHybridStat*);
  void setMoms(StSvtHybridPixels* h1, StSvtHybridPixels* h2, int weight = 1);
  void setMoms(float* x1, float* x2, int weight = 1);

  StSvtHybridPixels* get0thMom() {return m0;}
  StSvtHybridPixels* get1stMom() {return m1;}
  StSvtHybridPixels* get2ndMom() {return m2;}

  float getMean(int anode, int time);
  float getRMS(int anode, int time);

  void fillMomAllPixels(int x);
  void fillMom(int x, int anode, int time);

  void reset();

protected:

  StSvtHybridPixels* m0;  // zeroth order statistical momentum
  StSvtHybridPixels* m1;  // first order statistical momentum
  StSvtHybridPixels* m2;  // second order statistical momentum
  
  ClassDef(StSvtHybridStat,1)
};

#endif
