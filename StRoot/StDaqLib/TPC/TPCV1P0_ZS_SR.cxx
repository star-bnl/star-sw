#include <iostream>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV1P0.hh"
#include "fee_pin.h"
// TPC V1.0 Zero Suppressed Reader

// -------------change log ------------------------
// 31-May-99 MJL terminate on encountering pad 255
// 03-Jun-99 MJL added return TRUE to TPCV1P0_ZS_SR::initialize()

TPCV1P0_ZS_SR::TPCV1P0_ZS_SR(int s, TPCV1P0_Reader *det)
{
  cout << "Constructing TPCV1P0_ZS_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)adcd_p, 0, sizeof(adcd_p));
  memset((char *)adcx_p, 0, sizeof(adcx_p));
  memset((char *)seqd_p, 0, sizeof(seqd_p));
}

int TPCV1P0_ZS_SR::initialize()
{


  for (int row=0; row<TPC_PADROWS; row++) {
    Row_array[row].pad = &Pad_array[row][0];
    Row_array[row].npads = 0;   // have to fill in PadRow.npads as we go.
    for (int pad=0; pad<TPC_MAXPADS; pad++) {
      Pad_array[row][pad].nseq=0;
      Pad_array[row][pad].seq= (Sequence *)0;
    }
  }

  // store pointers to the ADCD, ADCX, SEQD banks
  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      adcd_p[rcb][mz] = detector->getBankTPCADCD(sector,rcb,mz);
      if ((void *)adcd_p[rcb][mz] != NULL) {
	printf("found ADCD RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
      adcx_p[rcb][mz] = detector->getBankTPCADCX(sector,rcb,mz);
      if ((void *)adcx_p[rcb][mz] != NULL) {
	printf("found ADCX RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
      seqd_p[rcb][mz] = detector->getBankTPCSEQD(sector,rcb,mz);
      if ((void *)seqd_p[rcb][mz] != NULL) {
	printf("found SEQD RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
    }
  }
   

  // search through the  SEQD banks to build our tables of what's where

  for(int rcb = 0; rcb < 6; rcb++) {
    for(int mz = 0; mz < 3; mz++) {
      if (seqd_p[rcb][mz] == (classname(Bank_TPCSEQD) *)NULL) continue;
      printf("TPCSEQD found sector %d  RB%d MZ%d\n",sector+1,rcb+1,mz+1);
      int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
      int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
      int numseq = (4*len)/sizeof(short); // find # sequences this bank
      // go through the SEQD twice. First time around just count the sequences
      // so we can malloc the Sequence arrays as needed
      for (int i=0; i<numseq; i++) {
	if (seqd_p[rcb][mz]->sequence[i]<0) { //padrow, pad
	  padrow = (seqd_p[rcb][mz]->sequence[i]>>8)& 0x7f;
	  pad = (seqd_p[rcb][mz]->sequence[i])& 0xff;
	  if (pad==255) break; //pad 255 exists only in extraneous last word
	  Row_array[padrow-1].npads++; //increment #pads in padrow
	  oldstart = 0;
	}
	else {
	  if (padrow <0 || pad <0) {
	    printf("encountered bad SEQD bank RB %d, Mezz %d\n", rcb+1, mz+1);
	    return FALSE;
	  }
	  unsigned short work = seqd_p[rcb][mz]->sequence[i];
	  int start = work>>6;
	  int len = work & 0x1f;
	  if (start >= oldstart) { // still on same pad
	    if (start>lastbin+1)  Pad_array[padrow-1][pad-1].nseq++;
	    // don't increment nseq if sequences are adjacent!
	    lastbin = start+len-1;
	    oldstart = start;
	    if (work & 0x20 ) {//last sequence ?
	      pad++; // default to next pad in this padrow
	      Row_array[padrow-1].npads++; //increment #pads in padrow
	      lastbin = -2; // set lastbin for new pad
	      oldstart=0;
	    }
	  }
	  else {     // starting new pad without bit 5 set!
	    printf("new pad detected with bit 5 clear!\n");
	    fflush(stdout);
	    return FALSE;
	  }
	}
      }
    }
  }

      //allocate memory for Sequence arrays
  for (int row=0; row<TPC_PADROWS; row++) {
    int npads = Row_array[row].npads;
    for (int pad=0; pad<TPC_MAXPADS; pad++) {
       int nseq = Pad_array[row][pad].nseq;
       if (nseq) { // only if there are sequences on this pad
 	Pad_array[row][pad].seq= (Sequence *)malloc(nseq*sizeof(Sequence));
 	if (Pad_array[row][pad].seq==NULL) {
 	  cout << "failed to malloc() Sequence structures " << endl;
 	  return FALSE;
 	}
       }
    }
  }
     //second pass
  for(int rcb = 0; rcb < 6; rcb++) {
    for(int mz = 0; mz < 3; mz++) {
      if (seqd_p[rcb][mz] == (classname(Bank_TPCSEQD) *)NULL) continue;
      int padrow=-1, pad=-1, lastbin=-2, pad_seq, oldstart = 0;
      int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
      int numseq = (4*len)/sizeof(short); // find # sequences this bank
      u_char *adc_locn = (u_char *)adcd_p[rcb][mz]->ADC;
      for (int i=0; i<numseq; i++) {
	if (seqd_p[rcb][mz]->sequence[i]<0) { //padrow, pad
	  padrow = (seqd_p[rcb][mz]->sequence[i]>>8)& 0x7f;
	  pad = (seqd_p[rcb][mz]->sequence[i])& 0xff;
	  if (pad==255) break; //pad 255 exists only in extraneous last word
	  pad_seq = 0;
	  oldstart=0;
	}
	else {
	  unsigned short work = seqd_p[rcb][mz]->sequence[i];
	  int start = work>>6;
	  int len = work & 0x1f;
	  if (start >= oldstart) { // still on same pad
	  //is this sequence adjacent to previous one?
	    if (start>lastbin+1)  { //no
	      Pad_array[padrow-1][pad-1].seq[pad_seq].startTimeBin = start;
	      Pad_array[padrow-1][pad-1].seq[pad_seq].Length = len;
	      Pad_array[padrow-1][pad-1].seq[pad_seq].FirstAdc = adc_locn;
	      adc_locn +=len;
	      pad_seq++;
	    }
	    else { // yes: just update the length
	      Pad_array[padrow-1][pad-1].seq[pad_seq].Length += len;
	      adc_locn +=len;
	    }
	    lastbin = start+len-1;
	    if (work & 0x20) {//last sequence ?
	      pad++;    // default to next pad in this padrow
	      pad_seq = 0;
	      lastbin = -2; // set lastbin for new pad	
	      oldstart=0;
	    }
	  }
	  else {    // starting new pad without bit 5 set!
	    printf("new pad detected with bit 5 clear!\n");
	    fflush(stdout);
	    return FALSE;
	  }
	}
      }
    }
  }
  return TRUE;
}

TPCV1P0_ZS_SR::~TPCV1P0_ZS_SR()
{
  cout << "Deleting TPCV1P0_ZS_SR" << endl;
  //free memory allocated for Sequence arrays
  for (int row=0; row<TPC_PADROWS; row++) {
    for (int pad=0; pad<TPC_MAXPADS; pad++) {
      void *memaddr = Pad_array[row][pad].seq;
      if (memaddr) free(memaddr);
    }
  }
}

int TPCV1P0_ZS_SR::getPadList(int PadRow, u_char **padList)
{
    // Construct the padlist array for this PadRow
  int pad;
  if (PadRow == 0 || PadRow > TPC_PADROWS) return -1;

  // Fill in padrows
  int npad=0;
  for(pad=1; pad<=TPC_MAXPADS; pad++)
  {
    if (Pad_array[PadRow-1][pad-1].nseq) padlist[PadRow-1][npad++] = pad;
  }
  // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];

  return npad;
}

int TPCV1P0_ZS_SR::getSequences(int PadRow, int Pad, int *nSeq, 
				Sequence **SeqData)
{
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
}

int TPCV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq, 
				Sequence **SeqData)
{
  unsigned short PadRow = row_vs_fee[Fee][Pin];
  unsigned short Pad = pad_vs_fee[Fee][Pin];
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
}

int TPCV1P0_ZS_SR::MemUsed()
{
  return 0;
}

