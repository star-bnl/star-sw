/***************************************************************************
 *
 * $Id: StSvtDaqData.cc,v 1.9 2004/01/27 02:36:02 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqData.cc,v $
 * Revision 1.9  2004/01/27 02:36:02  perev
 * LeakOff
 *
 * Revision 1.8  2003/09/02 17:59:08  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2002/05/10 21:12:36  caines
 * Dont swap ladders 15 and 16 for pp sort this out properly late
 *
 * Revision 1.6  2002/05/06 00:35:31  munhoz
 * correct hybrid swapping
 *
 * Revision 1.5  2001/10/24 16:49:42  munhoz
 * adding capability to retrieve t0 and first SCA
 *
 * Revision 1.4  2001/07/11 23:29:47  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 * Revision 1.2  2000/07/03 02:07:55  perev
 * StEvent: vector<TObject*>
 *
 * Revision 1.1  2000/06/13 20:42:05  caines
 * StRoot/StSvtDaqMaker
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is a collection of StSvtHybridData objects.                         // 
// It represents the entire SVT data.                                     //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include "StSvtHybridDaqData.hh"
#include "StSvtDaqData.hh"

ClassImp(StSvtDaqData)

StSvtDaqData::StSvtDaqData() : 
  StSvtData()
{}

StSvtDaqData::StSvtDaqData(const char* config, StSVTReader* reader, char* option, int run, int event, int trigger) : 
  StSvtData(config, run, event, trigger)
{
  // The Same as StSvtHybridCollection, with two additional parameters: 
  //    event number and trigger type

  mRunNumber = run;
  mEventNumber = event;
  mTriggerWord = trigger;

  if (reader)
    setData(reader,option);
}

StSvtDaqData::StSvtDaqData(StSvtConfig* config, StSVTReader* reader, char* option, int run, int event, int trigger) : 
  StSvtData(config, run, event, trigger)
{
  // The Same as StSvtHybridCollection, with two additional parameters: 
  //    event number and trigger type

  mRunNumber = run;
  mEventNumber = event;
  mTriggerWord = trigger;

  if (reader)
    setData(reader,option);
}

int StSvtDaqData::setData(StSVTReader* reader, char* option)
{
  int status, index;

  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  index = getHybridIndex(barrel,ladder,wafer,hybrid);
	  if (index < 0) continue;
	  
	  //printf("StSvtDaqMaker::barrel = %d, ladder = %d, wafer = %d, hybrid = %d\n",barrel,ladder,wafer,hybrid);

	  // have to swap the hybrids in collection due to hardware swapping for Y1
	  if ( !strncmp(getConfiguration(), "Y1L", strlen("Y1L")) ) {
	    if (hybrid == 1)
	      index = getHybridIndex(barrel, ladder, wafer, 2);
	    else if (hybrid == 2)
	      index = getHybridIndex(barrel, ladder, wafer, 1);
	  }
	  else {
	    if( (barrel == 2) && (ladder == 1) && (wafer > 3)){
	      if( hybrid ==1) index++;
	      if( hybrid ==2) index --;
	    }
	    else if( (barrel == 2) && (ladder == 8) && (wafer < 4)){
	      if( hybrid ==1) index++;
	      if( hybrid ==2) index --;
	    }
	    else if( (barrel == 3) && (ladder == 16) && (wafer > 4)){
	      //index -= 14;
	    }
	    else if( (barrel == 3) && (ladder == 15) && (wafer > 4)){
	      //index += 14;
	    }
	  }

	  StSvtHybridDaqData* mData = (StSvtHybridDaqData*)at(index);

	  if (mData) {
	    delete mData;
	    put_at(0,index);
	  }

	  //if (mData)
	  //  mData->reset();
	  //else
	  mData = new StSvtHybridDaqData(barrel, ladder, wafer, hybrid);

	  status = ((StSvtHybridDaqData*)mData)->setHybridData(reader, option);

	  if ((barrel == 1) && (ladder == 1) && (wafer == 2)) {
	    ((StSvtHybridDaqData*)at(getHybridIndex(1, 1, 1, 1)))->setSCAZero(((StSvtHybridDaqData*)mData)->getSCAZero());
	    ((StSvtHybridDaqData*)at(getHybridIndex(1, 1, 1, 2)))->setSCAZero(((StSvtHybridDaqData*)mData)->getSCAZero());
	    ((StSvtHybridDaqData*)at(getHybridIndex(1, 1, 1, 1)))->setTimeZero(((StSvtHybridDaqData*)mData)->getTimeZero());
	    ((StSvtHybridDaqData*)at(getHybridIndex(1, 1, 1, 2)))->setTimeZero(((StSvtHybridDaqData*)mData)->getTimeZero());
	  }

	  if (status >= 0) {
	    put_at(mData, index);	  
	  }
	  else
	    delete mData;
	}
      }
    }
  }	  
  return 0;
}


