// $Id: StFtpcRawWriter.cc,v 1.10 2009/11/14 13:18:33 jcs Exp $
// $Log: StFtpcRawWriter.cc,v $
// Revision 1.10  2009/11/14 13:18:33  jcs
// change LOG_INFO messages to LOG_DEBUG messages
//
// Revision 1.9  2007/01/15 15:02:19  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.8  2003/09/22 13:14:18  fsimon
// Fixed code to eliminate compiler warning
//
// Revision 1.7  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2003/01/29 12:06:34  fsimon
// Include switch to enable/disable turning of pad order for ASIC 2 in FTPC E
// Was an error on Y2001/2002 DAQ mapping
//
// Revision 1.5  2002/11/26 02:20:39  perev
// iostream added
//
// Revision 1.4  2002/09/13 13:36:17  fsimon
// Include correction for wrong pad numbering in central FEE Card in
// each sector of the East FTPC
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
// interface to write out ftpc raw data
// used by StFtpcSlowSimulator to write simulated data to tables


#include <Stiostream.h>
#include "StFtpcRawWriter.hh"
#include "StMessMgr.h"

StFtpcRawWriter::StFtpcRawWriter(St_fcl_ftpcndx *ftpcndxIn,
				 St_fcl_ftpcsqndx *ftpcsqndxIn,
				 St_fcl_ftpcadc *ftpcadcIn,
				 const int inAsic2EastNotInverted)
{
  ftpcndx=ftpcndxIn;
  ftpcsqndx=ftpcsqndxIn;
  ftpcadc=ftpcadcIn;
  mAsic2EastNotInverted = inAsic2EastNotInverted;

  ndx=ftpcndx->GetTable();
  numNdx=ftpcndx->GetNRows();
  maxNdx=ftpcndx->GetTableSize();
  sqndx=ftpcsqndx->GetTable();
  numSqndx=ftpcsqndx->GetNRows();
  maxSqndx=ftpcsqndx->GetTableSize();
  adc=ftpcadc->GetTable();
  numAdc=ftpcadc->GetNRows();
  maxAdc=ftpcadc->GetTableSize();
  LOG_INFO << "FTPC RawWriter created with Asic2EastNotInverted = " << mAsic2EastNotInverted <<endm;
}

StFtpcRawWriter::~StFtpcRawWriter()
{
    ftpcndx->SetNRows(numNdx);
    ftpcsqndx->SetNRows(numSqndx);
    ftpcadc->SetNRows(numAdc); 
     
  
    //ftpcsqndx->Print(0, ftpcsqndx->GetNRows());
    //ftpcadc->Print(0, ftpcadc->GetNRows());
}

int StFtpcRawWriter::writeArray(float *array, 
				int numberPadrows, 
				int numberSectors, 
				int numberPads, 
				int numberTimebins,
				int threshold) 
{
  int pixel_per_seq=0;
  int start_pixel=0;
  int seq_index=0;
  int adc_index=0;
  int seq_flag=0;
  int pad_flag;
  int last_pad;
  
 
  float *cArray = new float[numberPadrows
			    *numberSectors
			    *numberPads
			    *numberTimebins];      
  

  // LOG_DEBUG << " Creating copy array "<<numberPadrows<<"; "<<numberSectors<<"; "<<numberPads<<"; "<<numberTimebins<<endm;


  // Correct different pad & sector numbering scheme for east & west FTPC

  for (int row=0; row<numberPadrows; row++) { 
    for (int sec=0; sec<numberSectors; sec++) {
      for (int pad=0; pad<numberPads; pad++) {
	for (int bin=0; bin<numberTimebins; bin++) {
	  int newi;
	  int i=bin
	    +numberTimebins*pad
	    +numberTimebins*numberPads*sec
	    +numberTimebins*numberPads*numberSectors*row;
	  if (row>=10)
	    if (mAsic2EastNotInverted && (pad>63)&&(pad<96)) // no turning for center FEE card in each sector for old data (prior to 2003)
	      newi=bin              // Turning for simulator db access in StFtpcSlowSimReadout::GetHardPad
 		+numberTimebins*pad
		+numberTimebins*numberPads*sec
		+numberTimebins*numberPads*numberSectors*row; 
	    else
	      newi=bin
		+numberTimebins*(numberPads-pad-1)
		+numberTimebins*numberPads*sec
		+numberTimebins*numberPads*numberSectors*row;
	  else
	    newi=bin
	      +numberTimebins*(numberPads-pad-1)
	      +numberTimebins*numberPads*(numberSectors-sec-1)
	      +numberTimebins*numberPads*numberSectors*row;
	  cArray[newi]=array[i];
	  //if (array[i] >0.1) LOG_DEBUG << "Copying value "<< array[i] <<" for pad "<<pad << " Sec: " << sec <<" Row: "<< row <<endm; 
	}
      }
    }
  }

  Double_t th_high = 5;
  Double_t th_low = 2; 
  Int_t minAboveLow = 2;


	
  Int_t valid_seq = 0;
  Int_t nAboveLow = 0;
  Int_t highReached = 0;
  pixel_per_seq=0;
  start_pixel=0;
  seq_index=0;
  adc_index=0;
  seq_flag=0;

  //LOG_DEBUG << "RawWriter using threshold high:" << th_high << " and threshold low "<< th_low<<endm;
	  

  // Fill sequences

  ndx[0].index=0;
  for (int row=0; row<numberPadrows; row++) { 
    for (int sec=0; sec<numberSectors; sec++) {
      last_pad=-100;
      for (int pad=0; pad<numberPads; pad++) {
	pad_flag=0;
	
	for (int bin=0; bin<numberTimebins; bin++) {
	  int i=bin
	    +numberTimebins*pad
	    +numberTimebins*numberPads*sec
	    +numberTimebins*numberPads*numberSectors*row;
	  //if (cArray[i] >1) LOG_DEBUG << "Table value "<< cArray[i] <<" for pad "<<pad << " Sec: " << sec <<" Row: "<< row <<endm; 

	  if((int) cArray[i] < th_low) {
	    valid_seq = 0; 
	    nAboveLow = 0;
	    highReached = 0;
	  }
	  
	  if((int) cArray[i] >= th_low) {
	    
	    //LOG_DEBUG <<"Over threshold, seq_index = "<<seq_index <<endm;
	    
	    // Check if sequence is valid, apply ASIC parameters
	    if (!valid_seq){
	      for (int t = bin; t < numberTimebins; t ++) {
		int s = t
		  +numberTimebins*pad
		  +numberTimebins*numberPads*sec
		  +numberTimebins*numberPads*numberSectors*row;
		if (cArray[s]>= th_low) nAboveLow++;
		if (cArray[s]>= th_high) highReached = 1;
		if (highReached && (nAboveLow >= minAboveLow)) {
		  valid_seq = 1;
		  t = numberTimebins;
		}
		if (cArray[s]< th_low) {
		  valid_seq=0;
		  bin = t;
		  t = numberTimebins;
		}
	      }
	      //if (valid_seq) LOG_DEBUG << "Sequence "<<seq_index<< " validated! Pad " << pad << " nAboveLow "<<nAboveLow <<endm;
	    }


	    if (!valid_seq) continue;

	    // beginning of second FTPC?
	    if(row>=10 && ndx[1].index==0) {
	      if(maxNdx<2) {
		LOG_WARN << "ndx overflow!" << endm;
	      }
	      else {
		// set index for second FTPC
		ndx[1].index=seq_index;
	      }
	    }

	    // beginning of new sequence?
	    if(seq_flag == 0 || pixel_per_seq == 32) {
	      // is there a previous sequence?
	      if(pixel_per_seq > 0) {
		// set index for previous sequence
		sqndx[seq_index++].index = (pixel_per_seq-1) + (start_pixel<<6);
		//LOG_DEBUG << "sqndx[" << seq_index-1 << "].index=" << sqndx[seq_index-1].index << endm;
		//LOG_DEBUG << "pixel_per_seq=" << pixel_per_seq << " start_pixel=" << start_pixel << endm;
		if(seq_index >= maxSqndx) {
		  // reset overflow
		  seq_index=maxSqndx -1;
 		  LOG_WARN << "sqndx overflow!" << endm;
		}
	
		pixel_per_seq=0;
	
	      }

	      // set beginning of this sequence
	      start_pixel=bin;

	      // first sequence on pad?
	      if(pad_flag == 0) {
		// make previous sequence last sequence on pad
                if(seq_index>0) {
		  sqndx[seq_index-1].index += 32;
		  //LOG_DEBUG << "changed: sqndx[" << seq_index-1 << "].index=" << sqndx[seq_index-1].index << endm;
                }
		if(pad != last_pad+1) {
		  // set index for new pad
		  sqndx[seq_index++].index = 32768 + pad + 256*(6*row + sec);
		  //LOG_DEBUG << "sqndx[" << seq_index-1 << "].index=" << sqndx[seq_index-1].index << endm;
		  if(seq_index >= maxSqndx) {
		    // reset overflow
		    seq_index=maxSqndx;
		    LOG_WARN << "sqndx overflow!" << endm;
		  }
		}
		last_pad=pad;
	      }
	    }
	   
	    
	    adc[adc_index++].data=(char) cArray[i];
	    //LOG_DEBUG << "adc[" << adc_index-1 << "].data=" << (int) adc[adc_index-1].data << " at " <<row << " " << sec << " " << pad << " " << bin << endm;
	    if(adc_index >= maxAdc) {
	      // reset overflow
	      adc_index=maxAdc -1;
	      LOG_WARN << "adc overflow!" << endm;
	    }
	    pixel_per_seq++;
	    seq_flag=1;
	    pad_flag=1;
	  }
	  else {
	    seq_flag=0;
	  }
	}
      }
    }
  }

  // Delete copy array
  delete[] cArray;
  
 
 // is there a sequence?
  if(pixel_per_seq > 0) {
    // set index for last sequence
    sqndx[seq_index++].index = (pixel_per_seq-1) + (start_pixel<<6)+32;
    
    // set table lengths
    numNdx = 2;
    numSqndx = seq_index;
    numAdc = adc_index;
  }
  else {
    LOG_ERROR << "Error! No sequences filled!" << endm;
  }

  return 1;
}

 
