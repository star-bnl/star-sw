/***************************************************************************
 *
 * $Id: StSvtDaqData.cc,v 1.2 2000/07/03 02:07:55 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqData.cc,v $
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

#include <iostream.h>
#include "StSvtHybridDaqData.hh"
#include "StSvtDaqData.hh"

ClassImp(StSvtDaqData)

StSvtDaqData::StSvtDaqData(char* config, StSVTReader* reader, char* option, int run, int event, int trigger) : 
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
  int status;

  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {
  
	  if (getHybridIndex(barrel,ladder,wafer,hybrid) < 0) continue;
	  
	  //printf("StSvtDaqMaker::barrel = %d, ladder = %d, wafer = %d, hybrid = %d\n",barrel,ladder,wafer,hybrid);

	  // have to swap the hybrids in collection due to hardware swapping for Y1
	  if ( !strncmp(getConfiguration(), "Y1L", strlen("Y1L")) ) {
	    if (hybrid == 1)
	      mData = (StSvtHybridDaqData*)at(getHybridIndex(barrel, ladder, wafer, 2));
	    else if (hybrid == 2)
	      mData = (StSvtHybridDaqData*)at(getHybridIndex(barrel, ladder, wafer, 1));
	  }
	  else
	    mData = (StSvtHybridDaqData*)at(getHybridIndex(barrel, ladder, wafer, hybrid));

	  if (mData)
	    delete mData;
 
	  mData = new StSvtHybridDaqData(barrel, ladder, wafer, hybrid);

	  status = ((StSvtHybridDaqData*)mData)->setHybridData(reader, option);

	  if (status == 1) {

	    // have to swap the hybrids in collection due to hardware swapping for Y1
	    if ( !strncmp(getConfiguration(), "Y1L", strlen("Y1L")) ) {
	      if (hybrid == 1 || hybrid == 2 )
		put_at(mData, getHybridIndex(barrel,ladder,wafer,3-hybrid));	  
	    }
	    else
	      put_at(mData, getHybridIndex(barrel,ladder,wafer,hybrid));	  
	  }
	  else
	    delete mData;
	}
      }
    }
  }	  
  return 0;
}


