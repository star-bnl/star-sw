/***************************************************************************
 *
 * $Id: StSvtPedSub.cc,v 1.8 2003/09/02 17:59:08 perev Exp $
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: SVT Pedestal Subtraction Code
 *
 ***************************************************************************
 *
 * $Log: StSvtPedSub.cc,v $
 * Revision 1.8  2003/09/02 17:59:08  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2001/07/22 20:31:28  caines
 * Better tuning for real data. Common mode noise calc and sub. Avoid overlapping seq. Fill histograms only in debug
 *
 * Revision 1.6  2000/11/30 20:45:56  caines
 * Dynamically calc prob values, use database
 *
 * Revision 1.5  2000/08/28 22:12:39  caines
 * Error accessing timebucket in Ped. subtraction
 *
 * Revision 1.4  2000/08/21 12:57:30  caines
 * Now opens and reads in ped using CalibMaker
 *
 * Revision 1.3  2000/07/16 22:32:23  caines
 * Now also saves RAW data
 *
 * Revision 1.2  2000/07/03 02:07:56  perev
 * StEvent: vector<TObject*>
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 *
 **************************************************************************/
#include "StSvtPedSub.h"
#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StMessMgr.h"
#include <Stiostream.h>

StSvtPedSub::StSvtPedSub( StSvtHybridCollection *PedPointer)
{
  mPed = NULL;
  mSvtPed = PedPointer;
}



//_____________________________________________________________________________


int StSvtPedSub::SubtractPed( StSvtHybridData* fData, int Index, int PedOffset)
{


  int nAnodes, anodeID, nSeq, iseq, time, newAdc, status;
  StSequence* Seq;
  int* anodeList;
	  
  anodeList = NULL;

  nAnodes = fData->getAnodeList(anodeList);
  mPed = (StSvtHybridPed *) mSvtPed->at(Index);

  if (!mPed) return 0;
  
  for (int ianode=0;ianode<nAnodes;ianode++) {
    
    anodeID = anodeList[ianode];
    Seq = NULL;
    nSeq = 0;
    
    status = fData->getSequences(anodeID,nSeq,Seq);
    for (iseq=0;iseq<nSeq;iseq++) {	  	  
      for (time=0; time<Seq[iseq].length; time++) {

	//	if ((anodeID == 180) && (time == 64) )
	  //gMessMgr->Debug() << "ped = " << mPed->getPixelContent(anodeID,Seq[iseq].startTimeBin+time) << endm;
	
	// Actually subtract the pedestal per pixel. PedOffset  
	//allows undershoot to be seen 
	newAdc= (int)Seq[iseq].firstAdc[time]-
	  (int) mPed->getPixelContent(anodeID,Seq[iseq].startTimeBin+time)
	  +PedOffset;

	//Check adc hasn't gone -ve
	if( newAdc < 0) newAdc=0;
	else if( newAdc >= 256) newAdc=255;
	Seq[iseq].firstAdc[time]= newAdc;
	
      }
    }
  }
  
  return 0;
}
  

//_____________________________________________________________________________


