//==================================================================================================
// Writes Embedded data into FTPC SlowSimulator Sequences (as in StFtpcSlowSimMaker/StFtpcRawWriter.cc)
//
// Author: Frank Simon (fsimon@bnl.gov)
//==================================================================================================

//! FTPC Sequencer, creates Simulator sequences from ADC values
/*! \class StFtpcSequencer
    \author  Frank Simon (fsimon@bnl.gov)

    This class is part of the FTPC embedding framework. StFtpcSequencer is called by 
    StFtpcMixerMaker to create Ftpc SlowSimulator sequences that can be read by the
    StFTPCReader from an array of ADC values.
    The sequences are created analogous to StFtpcSlowSimMaker/StFtpcRawWriter, with
    the correct use of ASIC parameters for zero suppression
*/


#include "StFtpcSequencer.hh"
#include "Stiostream.h"
#include "StChain.h"

// Message System
#include "StMessMgr.h"

//! constructor, checks the existence of output sequences
StFtpcSequencer::StFtpcSequencer(St_fcl_ftpcndx *ftpcndxIn,
				 St_fcl_ftpcsqndx *ftpcsqndxIn,
				 St_fcl_ftpcadc *ftpcadcIn)
{
  // make sure the sequences exist
  if ((!ftpcndxIn)||(!ftpcsqndxIn)||(!ftpcadcIn)) {
    LOG_WARN << "StFtpcSequencer: Output sequences missing, bailing out!" << endm;
    return;
  }

  ftpcndx=ftpcndxIn;
  ftpcsqndx=ftpcsqndxIn;
  ftpcadc=ftpcadcIn;

  ndx=ftpcndx->GetTable();
  numNdx=ftpcndx->GetNRows();
  maxNdx=ftpcndx->GetTableSize();
  sqndx=ftpcsqndx->GetTable();
  numSqndx=ftpcsqndx->GetNRows();
  maxSqndx=ftpcsqndx->GetTableSize();
  adc=ftpcadc->GetTable();
  numAdc=ftpcadc->GetNRows();
  maxAdc=ftpcadc->GetTableSize();
}


//! destructor, sets size of sequence arrays
StFtpcSequencer::~StFtpcSequencer()
{
    ftpcndx->SetNRows(numNdx);
    ftpcsqndx->SetNRows(numSqndx);
    ftpcadc->SetNRows(numAdc); 
     
    LOG_INFO << "FTPC Sequencer finishing: Setting NRows ndx " << numNdx << " sqndx " << numSqndx << " adc " << numAdc << endm; 

}

//! writeArray method, fills zero-suppressed sequences from ADC array 
int StFtpcSequencer::writeArray(const int *cArray, 
				const int numberPadrows, 
				const int numberSectors, 
				const int numberPads, 
				const int numberTimebins) 
{

  // make sure the array that is supposed to be written out exists
  if (!cArray) {
    LOG_WARN << "FtpcSequencer: ADC array does not exist, bailing out!" << endm;
    return kStOk;
  }
  if ((!ftpcndx)||(!ftpcsqndx)||(!ftpcadc)) {
    LOG_WARN << "StFtpcSequencer: Output sequences missing, bailing out!" << endm;
    return kStOk;
  }


  int pixelPerSeq=0;
  int startPixel=0;
  int seqIndex=0;
  int adcIndex=0;
  int seqFlag=0;
  int padFlag;
  int lastPad;
  

  // set ASIC parameters for zero suppression
  Double_t thHigh = 5;
  Double_t thLow = 2; 
  Int_t minAboveLow = 2;


	
  Int_t validSeq = 0;
  Int_t nAboveLow = 0;
  Int_t highReached = 0;
  pixelPerSeq=0;
  startPixel=0;
  seqIndex=0;
  adcIndex=0;
  seqFlag=0;

  LOG_INFO << "FtpcSequencer  using threshold high:" << thHigh << " and threshold low "<< thLow<<endm;
	  

  // Fill sequences

  ndx[0].index=0;
  for (int row=0; row<numberPadrows; row++) { 
    for (int sec=0; sec<numberSectors; sec++) {
      lastPad=-100;
      for (int pad=0; pad<numberPads; pad++) {
	padFlag=0;
	
	for (int bin=0; bin<numberTimebins; bin++) {
	  int i=bin
	    +numberTimebins*pad
	    +numberTimebins*numberPads*sec
	    +numberTimebins*numberPads*numberSectors*row;
	  
	  if((int) cArray[i] < thLow) {
	    validSeq = 0; 
	    nAboveLow = 0;
	    highReached = 0;
	  }
	  
	  if((int) cArray[i] >= thLow) {
	    
	    // Check if sequence is valid, apply ASIC parameters
	    if (!validSeq){
	      for (int t = bin; t < numberTimebins; t ++) {
		int s = t
		  +numberTimebins*pad
		  +numberTimebins*numberPads*sec
		  +numberTimebins*numberPads*numberSectors*row;
		if (cArray[s]>= thLow) nAboveLow++;
		if (cArray[s]>= thHigh) highReached = 1;
		if (highReached && (nAboveLow >= minAboveLow)) {
		  validSeq = 1;
		  t = numberTimebins;
		}
		if (cArray[s]< thLow) {
		  validSeq=0;
		  bin = t;
		  t = numberTimebins;
		}
	      }
	      
	    }


	    if (!validSeq) continue;

	    // beginning of second FTPC?
	    if(row>=10 && ndx[1].index==0) {
	      if(maxNdx<2) {
		LOG_WARN << "ndx overflow!" << endm;
	      }
	      else {
		// set index for second FTPC
		ndx[1].index=seqIndex;
	      }
	    }

	    // beginning of new sequence?
	    if(seqFlag == 0 || pixelPerSeq == 32) {
	      // is there a previous sequence?
	      if(pixelPerSeq > 0) {
		// set index for previous sequence
		sqndx[seqIndex++].index = (pixelPerSeq-1) + (startPixel<<6);
		if(seqIndex >= maxSqndx) {
		  // reset overflow
		  seqIndex=maxSqndx -1;
 		  LOG_WARN << "sqndx overflow!" << endm;
		}
	
		pixelPerSeq=0;
	
	      }

	      // set beginning of this sequence
	      startPixel=bin;

	      // first sequence on pad?
	      if(padFlag == 0) {
		// make previous sequence last sequence on pad
                if(seqIndex>0) {
		  sqndx[seqIndex-1].index += 32;
                }
		if(pad != lastPad+1) {
		  // set index for new pad
		  sqndx[seqIndex++].index = 32768 + pad + 256*(6*row + sec);
		  if(seqIndex >= maxSqndx) {
		    // reset overflow
		    seqIndex=maxSqndx;
		    LOG_WARN << "sqndx overflow!" << endm;
		  }
		}
		lastPad=pad;
	      }
	    }
	   
	    
	    adc[adcIndex++].data=(char) cArray[i];
	    if(adcIndex >= maxAdc) {
	      // reset overflow
	      adcIndex=maxAdc -1;
	      LOG_WARN << "adc overflow!" << endm;
	    }
	    pixelPerSeq++;
	    seqFlag=1;
	    padFlag=1;
	  }
	  else {
	    seqFlag=0;
	  }
	}
      }
    }
  }

 // is there a sequence?
  if(pixelPerSeq > 0) {
    // set index for last sequence
    sqndx[seqIndex++].index = (pixelPerSeq-1) + (startPixel<<6)+32;
    
    // set table lengths
    numNdx = 2;
    numSqndx = seqIndex;
    numAdc = adcIndex;
  }
  else {
    LOG_WARN << "Error! No sequences filled!" << endm;
  }

  LOG_INFO << "FtpcSequencer done, getting table sizes:  NRows ndx " << numNdx << " sqndx " << numSqndx << " adc " << numAdc << endm; 


  return kStOk;
}

 /***************************************************************************
 *
 * $Id: StFtpcSequencer.cc,v 1.4 2007/01/15 15:02:12 jcs Exp $
 *
 * $Log: StFtpcSequencer.cc,v $
 * Revision 1.4  2007/01/15 15:02:12  jcs
 * replace printf, cout and gMesMgr with Logger
 *
 * Revision 1.3  2003/09/22 13:13:39  fsimon
 * Fixed code to eliminate compiler warning
 *
 * Revision 1.2  2003/09/02 17:58:15  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2003/02/14 18:11:25  fsimon
 * Initial commit of FTPC embedding code
 *
 *
 ***************************************************************************/
