/***************************************************************************
 *
 * $Id: StSvtHybridStat2.hh,v 1.2 2000/07/03 02:07:54 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pixel Statistic class used to calculate 2nd order pedestal correction
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridStat2.hh,v $
 * Revision 1.2  2000/07/03 02:07:54  perev
 * StEvent: vector<TObject*>
 *
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDSTAT2_HH
#define STSVTHYBRIDSTAT2_HH

#include "StSvtHybridObject.hh"

class StSvtHybridPixels;
class StObjArray;

class StSvtHybridStat2: public StSvtHybridObject
{
public:
  StSvtHybridStat2(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtHybridStat2();

  StSvtHybridStat2& operator = (const StSvtHybridStat2&);

  StSvtHybridPixels* get0thMom(int time2) {return (StSvtHybridPixels*)m0->at(time2);}
  StSvtHybridPixels* get1stMom(int time2) {return (StSvtHybridPixels*)m1->at(time2);}
  StSvtHybridPixels* get2ndMom(int time2) {return (StSvtHybridPixels*)m2->at(time2);}

  float getMean(int anode, int time, int time2);
  float getRMS(int anode, int time, int time2);

  void fillMom(int x, int anode, int time, int time2);

  void reset();

protected:

  int mNumberOfCapacitors;  // Number of Capacitors in the SCA (same as time bins)

  StObjArray* m0;  // array of zeroth order statistical momentum
  StObjArray* m1;  // array of first order statistical momentum
  StObjArray* m2;  // array of second order statistical momentum
  
  ClassDef(StSvtHybridStat2,1)
};

#endif
