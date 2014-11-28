/***************************************************************************
 *
 * $Id: StSvtHybridAnodeDriftCorr.cc,v 1.1 2004/07/31 00:46:56 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 *
 ***************************************************************************
 *
 * Description: correction factor for drift velocity as a function of anode 
 *
 ***************************************************************************/

#include "StSvtHybridAnodeDriftCorr.hh"

StSvtHybridAnodeDriftCorr::StSvtHybridAnodeDriftCorr()
{

  for (int i=0;i<MAX_NUMBER_OF_ANODES;i++) {
    mDriftCorr[i] = 0;
  }
}

StSvtHybridAnodeDriftCorr::StSvtHybridAnodeDriftCorr(int barrel, int ladder, int wafer, int hybrid) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  for (int i=0;i<MAX_NUMBER_OF_ANODES;i++) {
    mDriftCorr[i] = 0;
  }
}

StSvtHybridAnodeDriftCorr::~StSvtHybridAnodeDriftCorr()
{}

