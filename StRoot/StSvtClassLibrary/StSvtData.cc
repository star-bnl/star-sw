/***************************************************************************
 *
 * $Id: StSvtData.cc,v 1.3 2000/11/30 20:39:12 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtData.cc,v $
 * Revision 1.3  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
 * Revision 1.2  2000/08/23 12:48:38  munhoz
 * add reset method
 *
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

StSvtData::StSvtData() : 
  StSvtHybridCollection()
{}

StSvtData::StSvtData(const char* config, int run, int event, int trigger, int time) : 
  StSvtHybridCollection(config)
{
  // The Same as StSvtHybridCollection, with some additional parameters. 
  mRunNumber = run;
  mEventNumber = event;
  mTriggerWord = trigger;
  mSCAZero = 0;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = 0;
  mUnixTime = time;
}

StSvtData::StSvtData(StSvtConfig* config, int run, int event, int trigger, int time) : 
  StSvtHybridCollection(config)
{
  // The Same as StSvtHybridCollection, with some additional parameters. 
  mRunNumber = run;
  mEventNumber = event;
  mTriggerWord = trigger;
  mSCAZero = 0;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = 0;
  mUnixTime = time;
}

StSvtData::~StSvtData()
{}

StSvtData::StSvtData(const StSvtData& data):StSvtHybridCollection(data.mConfig)
{
  // Copy Constructor
  mRunNumber = data.mRunNumber;
  mEventNumber = data.mEventNumber;
  mTriggerWord = data.mTriggerWord;
  mSCAZero = data.mSCAZero;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = data.mTimeZero[i];
  mUnixTime =data.mUnixTime;
}

StSvtData& StSvtData::operator = (const StSvtData& data)
{
  mRunNumber = data.mRunNumber;
  mEventNumber = data.mEventNumber;
  mTriggerWord = data.mTriggerWord;
  mSCAZero = data.mSCAZero;
  for (int i = 0; i < N_SECTORS; i++)
    mTimeZero[i] = data.mTimeZero[i];
  mUnixTime =data.mUnixTime;

  return *this;
}
