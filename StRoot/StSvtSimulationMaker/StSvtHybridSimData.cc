/***************************************************************************
 *
 * $Id: StSvtHybridSimData.cc,v 1.7 2003/11/13 16:24:59 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridSimData.cc,v $
 * Revision 1.7  2003/11/13 16:24:59  caines
 * Further improvements to get simulator looking like reality
 *
 * Revision 1.4  2003/07/31 19:18:10  caines
 * Petrs improved simulation code
 *
 * Revision 1.3  2001/11/06 20:12:06  caines
 * Add include for new compiler
 *
 * Revision 1.2  2001/05/10 04:29:52  caines
 * Change pedestal offset to match real raw data
 *
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This is the class to access the data from each hybrid.                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "Stiostream.h"
#include "StSvtHybridSimData.hh"
#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsC.hh"
#include "StMessMgr.h"

ClassImp(StSvtHybridSimData)

StSvtHybridSimData::StSvtHybridSimData(int barrel, int ladder, int wafer, int hybrid,StSvtHybridPixelsC* mSimDataPixels):StSvtHybridData(barrel, ladder, wafer,hybrid)
{
  mPedOffset=0;
  if (mSimDataPixels)
    setSimHybridData(mSimDataPixels);
}

int StSvtHybridSimData::setSimHybridData(StSvtHybridPixelsC* mSimDataPixels)
{
  nAnodes=0;  //number of anodes with some sequences

  if (mSimDataPixels==NULL){
    gMessMgr->Info() <<"Error:StSvtHybridSimData::setSimHybridData mSimDataPixels is NULL!!"<<endm;
    return 0;
  }

  //this not optimized for minimal memory size
  nSeq=new int[240];        
  seq = new (StSequence*)[240];
  anodeList=new int[240];
  
 
  Char_t  *mAdcArray=mSimDataPixels->GetArray(); //// array of [128*240]
 
  mPedOffset = mSimDataPixels->getPedOffset();
 
  StSequence tmpSeq[128];  //buffer for sequences on one anode
  for (int ianode=0;ianode<240;ianode++)
    {  
     
      int seqCount=0; //number of sequences on current anode
     
      int pixCount=0; ///number of pixels in current sequence
      for(int tim = 0; tim <= 128; tim++)
	{//loop over time bins in one anode
	  unsigned char adc; 
	  if (tim==128)  adc=0; // make an artificial end of time sequence
	  else adc= (unsigned char)mAdcArray[ianode*128 + tim];
	  
	  if (adc>0)
	    {
	      if (pixCount==0){ //starting new sequence
		tmpSeq[seqCount].startTimeBin = tim;
		tmpSeq[seqCount].firstAdc=(unsigned char*)(mAdcArray+ianode*128 + tim);
	      }
	      pixCount++;
	    }
	  else
	    {
	      if(pixCount>0){//end of sequence
		tmpSeq[seqCount].length = pixCount;
		seqCount++;
		pixCount=0;
	      }
	    }
	  
	  
	}
   
      if(seqCount>0)
	{ //save seq list     
	  anodeList[nAnodes]=ianode+1;
	  nSeq[nAnodes]=seqCount;
	  seq[nAnodes] = new StSequence[seqCount];
	  for (int i=0;i<seqCount;i++) seq[nAnodes][i]=tmpSeq[i];
	  nAnodes++;
	}
    }
    
  return 0;
}
