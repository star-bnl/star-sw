 /***************************************************************************
 *
 * $Id: StSvtHybridStat2.cc,v 1.3 2003/09/02 17:59:06 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pixel Statistic class used to calculate 2nd order pedestal correction
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridStat2.cc,v $
 * Revision 1.3  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2000/07/03 02:07:54  perev
 * StEvent: vector<TObject*>
 *
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// Used to calculate the statistics (mean and RMS) of pixel quantities    //
// given a SCA capacitor number.                                          //
// It is used to calculate the 2nd order pedestals correction of a hybrid.//
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include "StArray.h"
#include "StSvtHybridPixels.hh"
#include "StSvtHybridStat2.hh"

ClassImp(StSvtHybridStat2)

StSvtHybridStat2::StSvtHybridStat2(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  // The same as StSvtHybrid. Set number of capacitors to 128.

  mNumberOfCapacitors = 128;

  StSvtHybridPixels *m0PerCapacitor;
  StSvtHybridPixels *m1PerCapacitor;
  StSvtHybridPixels *m2PerCapacitor;

  m0 = new StObjArray(mNumberOfCapacitors);
  m1 = new StObjArray(mNumberOfCapacitors);
  m2 = new StObjArray(mNumberOfCapacitors);

  for (int i = 0; i < mNumberOfCapacitors; i++) {

    m0PerCapacitor = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
    m1PerCapacitor = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
    m2PerCapacitor = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);

    m0->push_back(m0PerCapacitor);
    m1->push_back(m1PerCapacitor);
    m2->push_back(m2PerCapacitor);
  }
}

StSvtHybridStat2::~StSvtHybridStat2()
{
  delete m0;
  delete m1;
  delete m2;
}

StSvtHybridStat2& StSvtHybridStat2::operator = (const StSvtHybridStat2& h)
{
  m0 = h.m0;
  m1 = h.m1;
  m2 = h.m2;
  return *this;
}

float StSvtHybridStat2::getMean(int anode, int time, int capacitor) 
{
  // Returns the mean value for pixel (anode,time) and capacitor

  StSvtHybridPixels* m0PerCapacitor = (StSvtHybridPixels*)m0->at(capacitor);
  StSvtHybridPixels* m1PerCapacitor = (StSvtHybridPixels*)m1->at(capacitor);

  int index = m0PerCapacitor->getPixelIndex(anode,time);
  int n = (int)m0PerCapacitor->At(index);
  int sum = (int)m1PerCapacitor->At(index);

  float mean;

  if (n)
    mean = (float)sum/(float)n;
  else
    return 0;

  return mean;
}

float StSvtHybridStat2::getRMS(int anode, int time, int capacitor) 
{
  // Returns the RMS for pixel (anode,time) and capacitor

  StSvtHybridPixels* m0PerCapacitor = (StSvtHybridPixels*)m0->at(capacitor);
  StSvtHybridPixels* m1PerCapacitor = (StSvtHybridPixels*)m1->at(capacitor);
  StSvtHybridPixels* m2PerCapacitor = (StSvtHybridPixels*)m2->at(capacitor);

  int index = m0PerCapacitor->getPixelIndex(anode,time);
  int n = (int)m0PerCapacitor->At(index);
  int sum = (int)m1PerCapacitor->At(index);
  int sumSQ = (int)m2PerCapacitor->At(index);

  float mean;
  float meanSQ;

  if (n) {
    mean = (float)sum/(float)n;
    meanSQ = (float)sumSQ/(float)n;
  }
  else 
    return 0;

  float rms = ::sqrt(meanSQ - mean*mean);

  return rms;
}

void StSvtHybridStat2::fillMom(int x, int anode, int time, int capacitor)
{
  // Fills the 0th, 1st and 2nd order momenta of pixel (anode,time) and capacitor with the value x

  StSvtHybridPixels* m0PerCapacitor = (StSvtHybridPixels*)m0->at(capacitor);
  StSvtHybridPixels* m1PerCapacitor = (StSvtHybridPixels*)m1->at(capacitor);
  StSvtHybridPixels* m2PerCapacitor = (StSvtHybridPixels*)m2->at(capacitor);

  int previousValue;
  int index = m0PerCapacitor->getPixelIndex(anode,time);

  previousValue = (int)m0PerCapacitor->At(index);
  m0PerCapacitor->AddAt(1+previousValue,index);

  previousValue = (int)m1PerCapacitor->At(index);
  m1PerCapacitor->AddAt(x+previousValue,index);

  previousValue = (int)m2PerCapacitor->At(index);
  m2PerCapacitor->AddAt(x*x+previousValue,index);
}

void StSvtHybridStat2::reset()
{
  m0 = NULL;
  m1 = NULL;
  m2 = NULL;
}
