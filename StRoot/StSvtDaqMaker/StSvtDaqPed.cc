/***************************************************************************
 *
 * $Id: StSvtDaqPed.cc,v 1.2 2003/09/02 17:59:08 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqPed.cc,v $
 * Revision 1.2  2003/09/02 17:59:08  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/07/11 23:29:48  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is a collection of StSvtHybridData objects.                         // 
// It represents the entire SVT data.                                     //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include "StSvtHybridDaqPed.hh"
#include "StSvtDaqPed.hh"
#include "StDAQMaker/StSVTReader.h"

ClassImp(StSvtDaqPed)

StSvtDaqPed::StSvtDaqPed() : 
  StSvtHybridCollection()
{}

StSvtDaqPed::StSvtDaqPed(const char* config, StSVTReader* reader, int run) : 
  StSvtHybridCollection(config)
{
  // The Same as StSvtHybridCollection, with two additional parameters: 
  //    event number and trigger type

  mRunNumber = run;

  if (reader)
    setPed(reader);
}

StSvtDaqPed::StSvtDaqPed(StSvtConfig* config, StSVTReader* reader, int run) : 
  StSvtHybridCollection(config)
{
  // The Same as StSvtHybridCollection, with two additional parameters: 
  //    event number and trigger type

  mRunNumber = run;

  if (reader)
    setPed(reader);
}

int StSvtDaqPed::setPed(StSVTReader* reader, const char* type)
{
  int status=-1;

  for (int barrel = 1;barrel <= getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= getNumberOfHybrids();hybrid++) {

	  if (getHybridIndex(barrel,ladder,wafer,hybrid) < 0) continue;
	      
	  //printf("StSvtDaqMaker::barrel = %d, ladder = %d, wafer = %d, hybrid = %d\n",barrel,ladder,wafer,hybrid);

	  // have to swap the hybrids in collection due to hardware swapping for Y1
	  if ( !strncmp(getConfiguration(), "Y1L", strlen("Y1L")) ) {
	    if (hybrid == 1)
	      mPed = (StSvtHybridDaqPed*)at(getHybridIndex(barrel, ladder, wafer, 2));
	    else if (hybrid == 2)
	      mPed = (StSvtHybridDaqPed*)at(getHybridIndex(barrel, ladder, wafer, 1));
	  }
	  else
	    mPed = (StSvtHybridDaqPed*)at(getHybridIndex(barrel, ladder, wafer, hybrid));

	  //if (mPed)
	  //  delete mPed;

	  //cout << "mPed = " << mPed << endl;
 
	  //if (mPed) {
	  //  delete mPed;
	  //  put_at(NULL, getHybridIndex(barrel,ladder,wafer,hybrid));
	  //}

	  if (!mPed)
	    mPed = new StSvtHybridDaqPed(barrel, ladder, wafer, hybrid);

	  if ( !strncmp(type, "PED", strlen("PED")) )
	    status = mPed->setHybridPed(reader);
	  else if ( !strncmp(type, "RMS", strlen("RMS")) )
	    status = mPed->setHybridRMSPed(reader);

	  if (status < 0)
	    mPed->reset();

	  // have to swap the hybrids in collection due to hardware swapping for Y1
	  if ( !strncmp(getConfiguration(), "Y1L", strlen("Y1L")) ) {
	    if (hybrid == 1 || hybrid == 2 )
	      put_at(mPed, getHybridIndex(barrel,ladder,wafer,3-hybrid));	  
	  }
	  else
	    put_at(mPed, getHybridIndex(barrel,ladder,wafer,hybrid));
	}
      }
    }
  }	  
  return 0;
}


