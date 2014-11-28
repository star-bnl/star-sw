#include "StFssSectorReader.hh"

#include <Stiostream.h>
#include "StMessMgr.h"

StFssSectorReader::StFssSectorReader(int sector,
				     unsigned short *fcl_ftpcsqndx, int nSeq,
				     char *fcl_ftpcadc, int nAdc)
{
  mSector=sector;
  m_ftpcsqndx=fcl_ftpcsqndx;
  m_ftpcadc=fcl_ftpcadc;
  m_numSqndx=nSeq;
  m_numAdc=nAdc;

}

StFssSectorReader::~StFssSectorReader()
{
  //free memory allocated for Sequence arrays
  for (int row=0; row<FTP_PADROWS; row++) 
    {
      for (int pad=0; pad<FTP_MAXPADS; pad++) 
	{
	  void *memaddr = Pad_array[row][pad].seq;
	  if (memaddr) free(memaddr);
	}
    }
}

int StFssSectorReader::getPadList(int PadRow, unsigned char **padList)
{
  // Construct the padlist array for this PadRow
  int pad;
  if (PadRow == 0 || PadRow > FTP_PADROWS) return -1;
  
  // Fill in padrows
  int npad=0;
  for(pad=1; pad<=FTP_MAXPADS; pad++)
    {
      if (Pad_array[PadRow-1][pad-1].nseq) 
	{
	  padlist[PadRow-1][npad++] = pad;
	}
    }
  // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  
  return npad;
}

int StFssSectorReader::getSequences(int PadRow, int Pad, int *nSeq, Sequence **SeqData)
{
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
}

int StFssSectorReader::initialize()
{
  int i;
  unsigned short *seqTable=m_ftpcsqndx;
  char *adcTable=m_ftpcadc;

  // find position in software coordinates
  int softSec=(mSector-1)%6;
  int softRow=2*(int)((mSector-1)/6);

  // initialize pad arrays
  int row, thisSoftSec=0, thisSoftRow=0;
  for (row=0; row<FTP_PADROWS; row++) 
    {
      Row_array[row].pad = &Pad_array[row][0];
      Row_array[row].npads = 0;   // have to fill in PadRow.npads as we go.
      for (int pad=0; pad<FTP_MAXPADS; pad++) 
	{
	  Pad_array[row][pad].nseq=0;
	  Pad_array[row][pad].seq= (Sequence *)0;
	}
    }

  // search through the  SEQD banks to build our tables of what's where

  int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
  // go through the SEQD twice. First time around just count the sequences
  // so we can malloc the Sequence arrays as needed
  for (i=0; i<m_numSqndx; i++) 
    {
      int thisEntry=seqTable[i];
      if ((thisEntry & 32768) == 32768) //padrow, pad header
 	{ 
	  thisSoftSec = ((thisEntry >>8) & 127);
	  thisSoftRow = (int) thisSoftSec / 6;
	  thisSoftSec -= 6 * thisSoftRow;
	  pad = (thisEntry & 255)+1;
	  // sequence in this sector ?
	  if(thisSoftSec==softSec && 
	     (thisSoftRow==softRow || thisSoftRow==softRow+1))
	    {
	      padrow = (thisSoftRow % 2) + 1;
	      Row_array[padrow-1].npads++; //increment #pads in padrow
	      oldstart = 0;
	    }
	}
      else
	{
	  // sequence in this sector ?
	  if(thisSoftSec==softSec && 
	     (thisSoftRow==softRow || thisSoftRow==softRow+1))
	    {
	      int start = (thisEntry>>6) & 511;
	      int len = (thisEntry & 31) +1;
	      if (start >= oldstart) 
		{ // still on same pad
		  if (start>lastbin+1)  
		    Pad_array[padrow-1][pad-1].nseq++;
		  // don't increment nseq if sequences are adjacent!
		  lastbin = start+len-1;
		  oldstart = start;
		  if ((thisEntry & 32)==32) 
		    {//last sequence ?
		      pad++; // default to next pad in this padrow
		      Row_array[padrow-1].npads++; //increment #pads in padrow
		      lastbin = -2; // set lastbin for new pad
		      oldstart=0;
		    }
		}
	      else 
		{     // starting new pad without bit 5 set!
		  LOG_WARN << "1: new pad detected with bit 5 clear!" << endm;
		  LOG_WARN << start << " < " << oldstart << endm;
		  return FALSE;
		}
	    }
	}
    }

  //allocate memory for Sequence arrays
  for (row=0; row<FTP_PADROWS; row++) 
    {
      //    int npads = Row_array[row].npads;
      for (int pad=0; pad<FTP_MAXPADS; pad++) 
	{
	  int nseq = Pad_array[row][pad].nseq;
	  if (nseq) 
	    { // only if there are sequences on this pad
	      Pad_array[row][pad].seq= (Sequence *)malloc(nseq*sizeof(Sequence));
	      if (Pad_array[row][pad].seq==NULL) 
		{
		  LOG_ERROR << "failed to malloc() Sequence structures " << endm;
		  return FALSE;
		}
	    }
	}
    }

  //second pass
  padrow=-1; 
  pad=-1; 
  lastbin=-2; 
  int pad_seq=0; 
  oldstart = 0;
  
  u_char *adc_locn = (u_char *)(adcTable);
  for (i=0; i<m_numSqndx; i++) 
    {
      int thisEntry=seqTable[i];
      if ((thisEntry & 32768) == 32768) //padrow, pad header
 	{ 
	  thisSoftSec = ((thisEntry >>8) & 127);
	  thisSoftRow = (int) thisSoftSec / 6;
	  thisSoftSec -= 6 * thisSoftRow;
	  pad = (thisEntry & 255)+1;
	  // sequence in this sector ?
	  if(thisSoftSec==softSec && 
	     (thisSoftRow==softRow || thisSoftRow==softRow+1))
	    {
	      padrow = (thisSoftRow % 2) + 1;
	      pad_seq = 0;
	      oldstart=0;
	    }
	}
      else 
	{
	  int start = (thisEntry>>6) & 511;
	  int len = (thisEntry & 31) +1;
	  if (start >= oldstart) 
	    { // still on same pad
	      //is this sequence adjacent to previous one?
	      if (start>lastbin+1)  
		{ //no
		  // sequence in this sector ?
		  if(thisSoftSec==softSec && 
		     (thisSoftRow==softRow || thisSoftRow==softRow+1))
		    {
		      Pad_array[padrow-1][pad-1].seq[pad_seq].startTimeBin = start;
		      Pad_array[padrow-1][pad-1].seq[pad_seq].Length = len;
		      Pad_array[padrow-1][pad-1].seq[pad_seq].FirstAdc = adc_locn;
		      pad_seq++;
		    }
		  adc_locn +=len;
		}
	      else 
		{ // yes: just update the length
		  // sequence in this sector ?
		  if(thisSoftSec==softSec && 
		     (thisSoftRow==softRow || thisSoftRow==softRow+1))
		    {
		      Pad_array[padrow-1][pad-1].seq[pad_seq].Length += len;
		    }
		  adc_locn +=len;
		}
	      lastbin = start+len-1;
	      if ((thisEntry&32)==32) 
		{//last sequence ?
		  pad++;    // default to next pad in this padrow
		  pad_seq = 0;
		  lastbin = -2; // set lastbin for new pad	
		  oldstart=0;
		}
	    }
	  else 
	    {    // starting new pad without bit 5 set!
	      LOG_WARN << "2: new pad detected with bit 5 clear!" << endm;
	      LOG_WARN << start << " < " << oldstart << endm;
	      return FALSE;
	    }
	}
    } // end for loop over sequences

  return TRUE;  
}

