/***************************************************************************
 *
 * $Id: StSvtData.hh,v 1.6 2004/01/26 23:14:14 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtData.hh,v $
 * Revision 1.6  2004/01/26 23:14:14  perev
 * Leak off
 *
 * Revision 1.5  2001/10/24 16:48:50  munhoz
 * adding capability to retrieve t0 and first SCA
 *
 * Revision 1.4  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
 * Revision 1.3  2000/08/23 12:48:44  munhoz
 * add reset method
 *
 * Revision 1.2  2000/07/06 03:47:31  caines
 * Add PedOffset to class so can calc. sum adc
 *
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTDATA_HH
#define STSVTDATA_HH

#include "StSvtHybridCollection.hh"

#define N_SECTORS 24

class StSvtHybridData;
class StSVTReader;
class StSvtAsciiUnpacker;

class StSvtData: public StSvtHybridCollection
{
public:
  StSvtData();
  StSvtData(const char* config, int run=0, int event=0, int trigger=0, int time=0);
  StSvtData(StSvtConfig* config, int run=0, int event=0, int trigger=0, int time=0);
  virtual ~StSvtData();

  StSvtData(const StSvtData&);
  StSvtData& operator = (const StSvtData&);

  void setRunNumber(int run)   { mRunNumber = run;}
  void setEventNumber(int evt) { mEventNumber = evt;}
  void setTrigWord(int trigger) { mTriggerWord = trigger;}
  void setPedOffset(int offset){mPedOffset = offset;}
  void setUnixTime(int time) {mUnixTime = time;}

  int getRunNumber()   {return mRunNumber;}
  int getEventNumber() {return mEventNumber;}
  int getTrigWord() {return mTriggerWord;}
  int getPedOffset(){return mPedOffset;}
  int getUnixTime() {return mUnixTime;}

protected:

  int mRunNumber;   // Run Number
  int mEventNumber; // Event Number
  int mTriggerWord; // Trigger Type
  int mPedOffset; //Pedestal offset added so can see zero
  int mUnixTime;  // Unix time

  ClassDef(StSvtData,1)
};

#endif
