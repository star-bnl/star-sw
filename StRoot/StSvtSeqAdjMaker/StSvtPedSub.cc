/***************************************************************************
 *
 * $Id: StSvtPedSub.cc,v 1.2 2000/07/03 02:07:56 perev Exp $
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: SVT Pedestal Subtraction Code
 *
 ***************************************************************************
 *
 * $Log: StSvtPedSub.cc,v $
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
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "TFile.h"
#include "StChain.h"
#include "StMessMgr.h"
#include <iostream.h>


StSvtPedSub::StSvtPedSub()
{
  mPed = NULL;
  mSvtPed = NULL;
}

//_________________________________________________________________________

StSvtPedSub::~StSvtPedSub()
{
  delete mPed;
}

//_________________________________________________________________________
int StSvtPedSub::ReadFromFile(char* fileName, StSvtData* fSvtData)
{
  TFile *file = new TFile(fileName);
  char name[20];

  mSvtPed = new StSvtHybridCollection(fSvtData->getConfiguration());


  for (int barrel = 1;barrel <= fSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= fSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= fSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= fSvtData->getNumberOfHybrids();hybrid++) {

	  if (fSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  sprintf(name,"Ped_%d_%d_%d_%d",barrel, ladder, wafer, hybrid);
	  mPed = (StSvtHybridPed*)file->Get(name);
	  	  
	  if (mPed)
	    (*mSvtPed)[mSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid)] = mPed;
	}
      }
    }
  }

  file->Close();

  return kStOK;
}

//_____________________________________________________________________________


int StSvtPedSub::SubtractPed( StSvtHybridData* fData, int Index, int PedOffset)
{


  int nAnodes, anodeID, nSeq, iseq, time, newAdc, status;
  StSequence* Seq;
  int* anodeList;
	  
  anodeList = NULL;

  nAnodes = fData->getAnodeList(anodeList);

  
  for (int ianode=0;ianode<nAnodes;ianode++) {
    
    anodeID = anodeList[ianode];
    Seq = NULL;
    nSeq = 0;
    
    status = fData->getSequences(anodeID,nSeq,Seq);
    mPed = (StSvtHybridPed *) mSvtPed->at(Index);
    for (iseq=0;iseq<nSeq;iseq++) {	  	  
      for (time=Seq[iseq].startTimeBin; time<Seq[iseq].startTimeBin+Seq[iseq].length; time++) {
	
	// Actually subtract the pedestal per pixel. PedOffset  
	//allows undershoot to be seen 
		newAdc= (int)Seq[iseq].firstAdc[time]-
		  (int) mPed->getPixelContent(anodeID,time)
		  +PedOffset;
	//Check adc hasn't gone -ve
	if( newAdc < 0) newAdc=0;
	else if( newAdc >= 256) newAdc=255;
	Seq[iseq].firstAdc[time]= newAdc;
	
      }
    }
  }
  
  return kStOK;
}
  

//_____________________________________________________________________________
int StSvtPedSub::Clear()
{

  if (mSvtPed) {
    mSvtPed->Delete();
    mSvtPed = NULL;
    mPed = NULL;
  }

  return kStOK;
}


