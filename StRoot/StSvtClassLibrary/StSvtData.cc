/***************************************************************************
 *
 * $Id: StSvtData.cc,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtData.cc,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is a collection of StSvtHybridData objects.                         // 
// It represents the entire SVT data.                                     //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "StSvtData.hh"
#include "StSvtHybridData.hh"

ClassImp(StSvtData)

StSvtData::StSvtData(char* config, int run, int event, int trigger) : 
  StSvtHybridCollection(config)
{
  // The Same as StSvtHybridCollection, with some additional parameters. 
  mRunNumber = run;
  mEventNumber = event;
  mTriggerWord = trigger;
  mSCAZero = 0;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = 0;
}

StSvtData::~StSvtData()
{}

StSvtData::StSvtData(const StSvtData& data)
{
  // Copy Constructor
  mRunNumber = data.mRunNumber;
  mEventNumber = data.mEventNumber;
  mTriggerWord = data.mTriggerWord;
  mSCAZero = data.mSCAZero;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = data.mTimeZero[i];
}

StSvtData& StSvtData::operator = (const StSvtData& data)
{
  mRunNumber = data.mRunNumber;
  mEventNumber = data.mEventNumber;
  mTriggerWord = data.mTriggerWord;
  mSCAZero = data.mSCAZero;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = data.mTimeZero[i];

  return *this;
}
