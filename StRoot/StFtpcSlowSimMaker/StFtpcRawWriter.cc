// $Id: StFtpcRawWriter.cc,v 1.2 2002/04/19 22:24:12 perev Exp $
// $Log: StFtpcRawWriter.cc,v $
// Revision 1.2  2002/04/19 22:24:12  perev
// fixes for ROOT/3.02.07
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
// interface to write out ftpc raw data
// used by StFtpcSlowSimulator to write simulated data to tables


#include <iostream.h>
#include "StFtpcRawWriter.hh"

StFtpcRawWriter::StFtpcRawWriter(St_fcl_ftpcndx *ftpcndxIn,
				 St_fcl_ftpcsqndx *ftpcsqndxIn,
				 St_fcl_ftpcadc *ftpcadcIn)
{
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

StFtpcRawWriter::~StFtpcRawWriter()
{
    ftpcndx->SetNRows(numNdx);
    ftpcsqndx->SetNRows(numSqndx);
    ftpcadc->SetNRows(numAdc);    
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
	  
	  if((int) array[i] >= threshold) {
	    	    
	    // beginning of second FTPC?
	    if(row>=10 && ndx[1].index==0) {
	      if(maxNdx<2) {
		cout << "ndx overflow!" << endl;
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
		//cout << "sqndx[" << seq_index-1 << "].index=" << sqndx[seq_index-1].index << endl;
		//cout << "pixel_per_seq=" << pixel_per_seq << " start_pixel=" << start_pixel << endl;
		if(seq_index >= maxSqndx) {
		  // reset overflow
		  seq_index=maxSqndx -1;
 		  cout << "sqndx overflow!" << endl;
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
		  //cout << "changed: sqndx[" << seq_index-1 << "].index=" << sqndx[seq_index-1].index << endl;
                }
		if(pad != last_pad+1) {
		  // set index for new pad
		  sqndx[seq_index++].index = 32768 + pad + 256*(6*row + sec);
		  //cout << "sqndx[" << seq_index-1 << "].index=" << sqndx[seq_index-1].index << endl;
		  if(seq_index >= maxSqndx) {
		    // reset overflow
		    seq_index=maxSqndx;
		    cout << "sqndx overflow!" << endl;
		  }
		}
		last_pad=pad;
	      }
	    }

	    adc[adc_index++].data=(char) array[i];
	    //cout << "adc[" << adc_index-1 << "].data=" << (int) adc[adc_index-1].data << " at " <<row << " " << sec << " " << pad << " " << bin << endl;
	    if(adc_index >= maxAdc) {
	      // reset overflow
	      adc_index=maxAdc -1;
	      cout << "adc overflow!" << endl;
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
    cout << "Error! No sequences filled!" << endl;
  }

  return 1;
}

