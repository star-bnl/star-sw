/***************************************************************************
 *
 * $Id: StSvtHybridNoise.hh,v 1.1 2000/11/30 20:47:49 caines Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridNoise.hh,v $
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#ifndef STSVTHYBRIDNOISE_HH
#define STSVTHYBRIDNOISE_HH

#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"

class StSvtHybridNoise:public StSvtHybridPixels
{
public:
  StSvtHybridNoise(int barrel, int ladder, int wafer, int hybrid);

  double makeGausDev(double sigma,double mean = 0);
  double prob(double sigma,  double threshold);
  double maxDistValue(double sigma ,double threshold);
  double countAboveThreshold(double sigma, double randNum);

protected:

  ClassDef(StSvtHybridNoise,1)
};

#endif
