/***************************************************************************
 *
 * $Id: StSvtData.cc,v 1.7 2004/01/26 23:13:28 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtData.cc,v $
 * Revision 1.7  2004/01/26 23:13:28  perev
 * Leak off
 *
 * Revision 1.6  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2002/01/15 01:01:42  caines
 * Fix memory leak when deleting this class - have to delete what it points to too
 *
 * Revision 1.4  2001/10/24 16:48:50  munhoz
 * adding capability to retrieve t0 and first SCA
 *
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

#include <Stiostream.h>
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
  mUnixTime = time;
}

StSvtData::StSvtData(StSvtConfig* config, int run, int event, int trigger, int time) : 
  StSvtHybridCollection(config)
{
  // The Same as StSvtHybridCollection, with some additional parameters. 
  mRunNumber = run;
  mEventNumber = event;
  mTriggerWord = trigger;
  mUnixTime = time;
}

StSvtData::~StSvtData()
{
}

StSvtData::StSvtData(const StSvtData& data):StSvtHybridCollection(data.mConfig)
{
  // Copy Constructor
  mRunNumber = data.mRunNumber;
  mEventNumber = data.mEventNumber;
  mTriggerWord = data.mTriggerWord;
  mUnixTime =data.mUnixTime;
}

StSvtData& StSvtData::operator = (const StSvtData& data)
{
  mRunNumber = data.mRunNumber;
  mEventNumber = data.mEventNumber;
  mTriggerWord = data.mTriggerWord;
  mUnixTime =data.mUnixTime;

  return *this;
}
