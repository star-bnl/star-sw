/***************************************************************************
 * $Id: FTPV1P0_ZS_SR.cxx,v 1.2 2000/01/19 20:31:25 levine Exp $
 * Author: M.J. LeVine, H.Huemmler
 ***************************************************************************
 * Description: FTPC V1.0 Zero Suppressed Reader
 *      
 *
 * -------------change log ------------------------
 * 18-01-00 MJL 
 * 
 ***************************************************************************
 * $Log: FTPV1P0_ZS_SR.cxx,v $
 * Revision 1.2  2000/01/19 20:31:25  levine
 * changed exit() to return -1 in
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,Sequence **SeqData)
 *
 * Revision 1.1  2000/01/18 18:01:19  levine
 * Hummler's implementaiton of FTPC reader. Note that method
 *
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,
 * 				   Sequence **SeqData)
 *
 * causes return -1 since the required #include file has not yet been
 * (correctly) implemented.
 *
 *
 *
 **************************************************************************/
#include <iostream.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "FTPV1P0.hh"
#include "TPC/fee_pin.h"



FTPV1P0_ZS_SR::FTPV1P0_ZS_SR(int s, FTPV1P0_Reader *det)
{
  cout << "Constructing FTPV1P0_ZS_SR" << endl;
  sector = s;
  detector = det;

  // NULLS in banks
  //  memset((char *)adcd_p, 0, sizeof(adcd_p));
  //  memset((char *)adcx_p, 0, sizeof(adcx_p));
  //  memset((char *)seqd_p, 0, sizeof(seqd_p));
  adcd_p=0;
  adcx_p=0;
  seqd_p=0;
}

int FTPV1P0_ZS_SR::initialize()
{
  int i;
  int row;
  for (row=0; row<FTP_PADROWS; row++) {
    Row_array[row].pad = &Pad_array[row][0];
    Row_array[row].npads = 0;   // have to fill in PadRow.npads as we go.
    for (int pad=0; pad<FTP_MAXPADS; pad++) {
      Pad_array[row][pad].nseq=0;
      Pad_array[row][pad].seq= (Sequence *)0;
    }
  }

  //  int rcb;
  // store pointers to the ADCD, ADCX, SEQD banks
  adcd_p = detector->getBankFTPADCD(sector);
  if ((void *)adcd_p != NULL) {
    printf("found ADCD, sector %d\n", sector);
    fflush(stdout);
  }
  adcx_p = detector->getBankFTPADCX(sector);
  if ((void *)adcx_p != NULL) {
    printf("found ADCX\n");
    fflush(stdout);
  }
  seqd_p = detector->getBankFTPSEQD(sector);
  if ((void *)seqd_p != NULL) {
    printf("found SEQD\n");
    fflush(stdout);
  }
   

  // search through the  SEQD banks to build our tables of what's where

  int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
  int len = seqd_p->header.BankLength - (sizeof(Bank_Header)/4);
  int numseq = (4*len)/sizeof(short); // find # sequences this bank
  // go through the SEQD twice. First time around just count the sequences
  // so we can malloc the Sequence arrays as needed
  for (i=0; i<numseq; i++) {
    if (seqd_p->sequence[i]<0) { //padrow, pad
      padrow = (seqd_p->sequence[i]>>8)& 0x7f;
      pad = (seqd_p->sequence[i])& 0xff;
      if (pad==255) break; //pad 255 exists only in extraneous last word
      Row_array[padrow-1].npads++; //increment #pads in padrow
      oldstart = 0;
    }
    else {
      if (padrow <0 || pad <0) {
	printf("encountered bad SEQD bank\n");
	return FALSE;
      }
      unsigned short work = seqd_p->sequence[i];
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
  
  //allocate memory for Sequence arrays
  // This stupid compiler thinks row's scope extends here, so I removed int
  // The original line should go in when we upgrade to a better compiler 
  //  for (int row=0; row<TPC_PADROWS; row++) {
  for (row=0; row<FTP_PADROWS; row++) {
    //    int npads = Row_array[row].npads;
    for (int pad=0; pad<FTP_MAXPADS; pad++) {
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
  padrow=-1; 
  pad=-1; 
  lastbin=-2; 
  int pad_seq; 
  oldstart = 0;
  len = seqd_p->header.BankLength - (sizeof(Bank_Header)/4);
  numseq = (4*len)/sizeof(short); // find # sequences this bank
  u_char *adc_locn = (u_char *)adcd_p->ADC;
  for (i=0; i<numseq; i++) {
    if (seqd_p->sequence[i]<0) { //padrow, pad
      padrow = (seqd_p->sequence[i]>>8)& 0x7f;
      pad = (seqd_p->sequence[i])& 0xff;
      if (pad==255) break; //pad 255 exists only in extraneous last word
      pad_seq = 0;
      oldstart=0;
    }
    else {
      unsigned short work = seqd_p->sequence[i];
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
  return TRUE;
}

FTPV1P0_ZS_SR::~FTPV1P0_ZS_SR()
{
  cout << "Deleting FTPV1P0_ZS_SR" << endl;
  //free memory allocated for Sequence arrays
  for (int row=0; row<FTP_PADROWS; row++) {
    for (int pad=0; pad<FTP_MAXPADS; pad++) {
      void *memaddr = Pad_array[row][pad].seq;
      if (memaddr) free(memaddr);
    }
  }
}

int FTPV1P0_ZS_SR::getPadList(int PadRow, u_char **padList)
{
    // Construct the padlist array for this PadRow
  int pad;
  if (PadRow == 0 || PadRow > FTP_PADROWS) return -1;

  // Fill in padrows
  int npad=0;
  for(pad=1; pad<=FTP_MAXPADS; pad++)
  {
    if (Pad_array[PadRow-1][pad-1].nseq) padlist[PadRow-1][npad++] = pad;
  }
  // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];

  return npad;
}

int FTPV1P0_ZS_SR::getSequences(int PadRow, int Pad, int *nSeq, 
				Sequence **SeqData)
{
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
}

int FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq, 
				   Sequence **SeqData)
{
#ifdef REAL_FEE_PIN_FOR_FTPC
  unsigned short PadRow = row_vs_fee[Fee][Pin];
  unsigned short Pad = pad_vs_fee[Fee][Pin];
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
#else
  printf("There is no table of FEE connections --- I quit\n");
  return -1;
#endif
}
 
// Read the clusters (space points) found in the mezzanine cluster-finder
int FTPV1P0_ZS_SR::getSpacePts(int PadRow, int *nSpacePts, SpacePt **SpacePts)
{
  cout <<"getSpacePoints() method not implemented" <<endl;
  return 0;
}


int FTPV1P0_ZS_SR::MemUsed()
{
  return 0;
}

