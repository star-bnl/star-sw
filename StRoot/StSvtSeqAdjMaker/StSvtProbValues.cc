/***************************************************************************
 *
 * $Id: StSvtProbValues.cc,v 1.4 2001/04/25 18:59:45 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtProbValues.cc,v $
 * Revision 1.4  2001/04/25 18:59:45  perev
 * HPcorrs
 *
 * Revision 1.3  2000/11/30 20:45:56  caines
 * Dynamically calc prob values, use database
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/


#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdlib.h>
#include "StSvtProbValues.hh"
#include "StMessMgr.h"


StSvtProbValues::StSvtProbValues()
{

  //Initialize the various variables
 mSigma = 2.1; //the standared deviation
 
 for(int i = 0; i < MAX_ADC_COUNTS; i++)
   mProb[i] = 0;
}

StSvtProbValues::~StSvtProbValues()
{}

void StSvtProbValues::SetProbValue(float sigma)
{
  double num = 0;

  if (!sigma){
    gMessMgr->Warning() << "StSvtSeqAdjMaker: No sigma set using default" << endm;
    sigma = mSigma;
  }
 for(int i = 0; i < MAX_ADC_COUNTS; i++)
  {
   num = double(i)/(M_SQRT2*sigma);
   mProb[i] = 0.5*(1 - erf(num));
   if (mProb[i] < 1e-20)
     mProb[i] = 1e-20;
   mProb[i]= 1/mProb[i];
  }
}
