 /***************************************************************************
 *
 * $Id: StSvtHybridStat.cc,v 1.4 2003/09/02 17:59:06 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pixel Statistic class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridStat.cc,v $
 * Revision 1.4  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// Used to calculate the statistics (mean and RMS) of pixel quantities.   //
// For instance, it can be used to calculate the pedestals of a hybrid.   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include "StSvtHybridPixels.hh"
#include "StSvtHybridStat.hh"

ClassImp(StSvtHybridStat)

StSvtHybridStat::StSvtHybridStat(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  // The same as StSvtHybrid. Initialize the statiscal momenta

  m0 = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
  m1 = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
  m2 = new StSvtHybridPixels(barrel, ladder, wafer, hybrid);
}

StSvtHybridStat::~StSvtHybridStat()
{
  delete m0;
  delete m1;
  delete m2;
}

StSvtHybridStat& StSvtHybridStat::operator = (const StSvtHybridStat& h)
{
  m0 = h.m0;
  m1 = h.m1;
  m2 = h.m2;
  return *this;
}

StSvtHybridStat* StSvtHybridStat::addStat(StSvtHybridStat* h)
{
  float x0, x1, x2;

  for (int i=0;i<m0->getTotalNumberOfPixels();i++) {
    x0 = m0->At(i) + h->get0thMom()->At(i);
    x1 = m1->At(i) + h->get1stMom()->At(i);
    x2 = m2->At(i) + h->get2ndMom()->At(i);

    m0->AddAt(x0,i);
    m1->AddAt(x1,i);
    m2->AddAt(x2,i);
  }

  return this;
}

void StSvtHybridStat::setMoms(StSvtHybridPixels* h1, StSvtHybridPixels* h2, int weight) 
{
  for (int i=0;i<m0->getTotalNumberOfPixels();i++) 
    m0->AddAt(weight,i);

  m1 = h1;
  m2 = h2;
} 

void StSvtHybridStat::setMoms(float* x1, float* x2, int weight) 
{
  for (int i=0;i<m0->getTotalNumberOfPixels();i++) 
    m0->AddAt(weight,i);

  m1->Set(m1->getTotalNumberOfPixels(),x1);
  m2->Set(m2->getTotalNumberOfPixels(),x2);
} 

float StSvtHybridStat::getMean(int anode, int time) 
{
  // Returns the mean value for pixel (anode,time)

  int index = m0->getPixelIndex(anode,time);
  int n = (int)m0->At(index);
  int sum = (int)m1->At(index);

  float mean;

  if (n)
    mean = (float)sum/(float)n;
  else
    return 0;

  return mean;
}

float StSvtHybridStat::getRMS(int anode, int time) 
{
  // Returns the RMS for pixel (anode,time)

  int index = m0->getPixelIndex(anode,time);
  int n = (int)m0->At(index);
  int sum = (int)m1->At(index);
  int sumSQ = (int)m2->At(index);

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

void StSvtHybridStat::fillMom(int x, int anode, int time)
{
  // Fills the 0th, 1st and 2nd order momenta of pixel (anode,time) with the value x

  int previousValue;
  int index = m0->getPixelIndex(anode,time);

  previousValue = (int)m0->At(index);
  m0->AddAt(1+previousValue,index);

  previousValue = (int)m1->At(index);
  m1->AddAt(x+previousValue,index);

  previousValue = (int)m2->At(index);
  m2->AddAt(x*x+previousValue,index);
}

void StSvtHybridStat::fillMomAllPixels(int x)
{
  for (int anode=1;anode<=m0->getNumberOfAnodes();anode++) {
    for (int time=0;time<m0->getNumberOfTimeBins();time++) {
      fillMom(x,anode,time);
    }
  }
}

void StSvtHybridStat::reset()
{
  if (m0)
    m0->reset();
  if (m1)
    m1->reset();
  if (m2)
    m2->reset();
}
