/***************************************************************************
 *
 * $Id: StSvtData.hh,v 1.2 2000/07/06 03:47:31 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtData.hh,v $
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
  StSvtData(char* config=0, int run=0, int event=0, int trigger=0);
  virtual ~StSvtData();

  StSvtData(const StSvtData&);
  StSvtData& operator = (const StSvtData&);

  void setRunNumber(int run)   { mRunNumber = run;}
  void setEventNumber(int evt) { mEventNumber = evt;}
  void setTrigWord(int trigger) { mTriggerWord = trigger;}
  void setSCAZero(int SCAZero){ mSCAZero = SCAZero;}
  void setTimeZero(int t0, int sector){ mTimeZero[sector-1] = t0;}
  void setPedOffset(int offset){mPedOffset = offset;}

  int getRunNumber()   {return mRunNumber;}
  int getEventNumber() {return mEventNumber;}
  int getTrigWord() {return mTriggerWord;}
  int getSCAZero(){return mSCAZero;}
  int getTimeZero(int sector){return mTimeZero[sector-1];}
  int getPedOffset(){return mPedOffset;}

protected:
  StSvtHybridData* mData; // Hybrid Data Object

  int mRunNumber;   // Run Number
  int mEventNumber; // Event Number
  int mTriggerWord; // Trigger Type
  int mSCAZero;     // Number of SCA capacitor for time bucket zero
  int mTimeZero[N_SECTORS];    // Time zero given by each read out box (fiber header)
  int mPedOffset; //Pedestal offset added so can see zero

  ClassDef(StSvtData,1)
};

#endif
