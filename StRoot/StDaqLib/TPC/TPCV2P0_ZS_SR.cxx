#include <iostream>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV2P0.hh"
#include "fee_pin.h"
// TPC V2.0 Zero Suppressed Reader
// MJL 28-May-99
// on encountering a missing TPCSEQD bank,
// tries to fill in the Pad_array structs using the RAW banks (if present)

// -------------change log ------------------------
// 31-May-99 MJL terminate on encountering pad 255
// 03-Jun-99 MJL added return TRUE to TPCV2P0_ZS_SR::initialize()
// 10-Jun-99 IS (Iwona Sakrejda) - The compiler we are using currently in Root
// cannot contain the scope of the "for" loop index within parenthesis.
// So if the next loop uses same index, it cannot be Declared again.
// I made 2 for rcg  and marked them both in  the code.

TPCV2P0_ZS_SR::TPCV2P0_ZS_SR(int s, TPCV2P0_Reader *det)
{
  cout << "Constructing TPCV2P0_ZS_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)adcd_p, 0, sizeof(adcd_p));
  memset((char *)adcx_p, 0, sizeof(adcx_p));
  memset((char *)seqd_p, 0, sizeof(seqd_p));
}

int TPCV2P0_ZS_SR::initialize()
{
  for (int row=0; row<TPC_PADROWS; row++) {
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
  // This stupid compiler thinks rcb's scope extends here, so I removed int
  // The original line should go in when we upgrade to a better compiler 
  //  for(int rcb = 0; rcb < 6; rcb++) {
  for(rcb = 0; rcb < 6; rcb++) {
    for(int mz = 0; mz < 3; mz++) {
      if (seqd_p[rcb][mz] == (classname(Bank_TPCSEQD) *)NULL) {
	//TPCSEQD bank doesn't exist
	// set up raw data pointers
	TPCV2P0_PADK_SR *padkr = detector->getPADKReader(sector);
	if (!padkr) return FALSE;
	classname(Bank_TPCADCR) *adcr =
	  detector->getBankTPCADCR(sector,rcb,mz) ; //pointer to ADC raw bank
	if (!adcr) return FALSE;
	classname(Bank_TPCCPPR) *cppr =
	  detector->getBankTPCCPPR(sector,rcb,mz) ; //pointer to CPP raw bank
	if (!cppr) return FALSE;

	// go through the CPPR/ADCR banks
	printf("reconstructing from RAW banks: SEC%d RB%d MZ%d\n",sector,rcb+1,mz+1);
	PADK_entry ent;

	for (int row=1; row<=TPC_PADROWS; row++) {
	  for (int pad=1; pad<=tpcRowLen[row-1]; pad++) {
	    padkr->get(row, pad, &ent);
	    if((ent.mz != mz+1) || (ent.rb != rcb+1)) continue;
	    //	      if (sector==11 && row==12 && pad==3) 
// 	    if (sector==10 && row==5 && pad==115)
// 	      printf("%s::%d  RB%d MZ%d:  row %d pad %d\n",
// 		     __FILE__,__LINE__,ent.rb,ent.mz,row,pad);
	    struct ASIC_Cluster *clusters = 
	      (ASIC_Cluster *)(cppr->entry + 32*ent.offset);

	    int lastbin = -2, len = 0;
	    int i, start, stop, newseq;
	    for (i=0; i<TPC_MXSEQUENCE; i++) { //loop over ASIC sequences
	      start = clusters[i].start_time_bin;
	      if ((start < 0)||(start==511)) break;
	      newseq = (start > lastbin+1) ; // sequence broken in pieces by MZ
	      stop = clusters[i].stop_time_bin;
	      len = stop - start + 1;
	      lastbin = stop;
	      if (newseq) Pad_array[row-1][pad-1].nseq++;
	      //update the cluster counter for this pad
	    }
	    if (i==TPC_MXSEQUENCE) {
	      //did we overflow the ASIC cluster pointer array
	      // do the rest here by hand
	    }
	    int nseq = Pad_array[row-1][pad-1].nseq;
	    if (!nseq) continue;
	    // only if there are sequences on this pad
	    Pad_array[row-1][pad-1].seq= (Sequence *)malloc(nseq*sizeof(Sequence)); 
	    if (Pad_array[row-1][pad-1].seq==NULL) {
	      cout << "failed to malloc() Sequence structures " << endl;
	      return FALSE;
	    }
	    lastbin = -2;
	    len = 0;
	    for (i=0; i<nseq; i++) { //loop over ASIC sequences
	      if (i<TPC_MXSEQUENCE) {
		start = clusters[i].start_time_bin;
		if ((start < 0)||(start==511)) break;
		newseq = (start > lastbin+1) ; // sequence broken in pieces by MZ
		// sequence broken in pieces by MZ
		stop = clusters[i].stop_time_bin;
		len = stop - start + 1;
		lastbin = stop;
		if (newseq) {
		  int offset = ent.offset * padkr->getADCBytes() + start;
		  Pad_array[row-1][pad-1].seq[i].startTimeBin = start;
		  Pad_array[row-1][pad-1].seq[i].Length = len;
		  Pad_array[row-1][pad-1].seq[i].FirstAdc = (u_char *)(adcr->ADC + offset);
		}
		else { // continuation of previous sequence
		  Pad_array[row-1][pad-1].seq[i].Length += len;
		}
	      }
	      else {//did we overflow the ASIC cluster pointer array?
		// do the rest here by hand (TBDL)
	      }	     
	    } 
	  }
	}
      }

      else { //TPCSEQD bank exists
	printf("TPCSEQD found sector %d  RB%d MZ%d\n",sector+1,rcb+1,mz+1);
	int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
	int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
	int numseq = (4*len)/sizeof(short); // find # sequences this bank
	// go through the SEQD twice:
	// First malloc the Sequence arrays needed
	// Second fill in the Sequence structs

	for (int i=0; i<numseq; i++) {
	  if (seqd_p[rcb][mz]->sequence[i]<0) { //padrow, pad
	    padrow = (seqd_p[rcb][mz]->sequence[i]>>8)& 0x7f;
	    pad = (seqd_p[rcb][mz]->sequence[i])& 0xff;
	    if (pad==255) break; 
	    //pad 255 exists only in extraneous last word
	    oldstart = 0;
	  }
	  else { // start|lastseq|len
	    if (padrow <0 || pad <0) {
	      printf("encountered bad SEQD bank RB %d, Mezz %d\n", rcb+1, mz+1);
	      return FALSE;
	    }
// 	    if (sector==10 && padrow==5 && pad==115)
// 	      printf("%s::%d  RB%d MZ%d seq %d:  row %d pad %d\n",
// 		     __FILE__,__LINE__,rcb,mz,i,padrow,pad);

	    unsigned short work = seqd_p[rcb][mz]->sequence[i];
	    int start = work>>6;
	    int len = work & 0x1f;
	    if (start >= oldstart) { // still on same pad
	      if (start>lastbin+1)  Pad_array[padrow-1][pad-1].nseq++;
	      // don't increment nseq if sequences are adjacent!
	      lastbin = start+len-1;
	      oldstart = start;
	      if (work & 0x20 ) {//last sequence ?
		int nseq = Pad_array[padrow-1][pad-1].nseq;
		if (nseq) { // only if there are sequences on this pad
		  //allocate memory for Sequence arrays
		  if (Pad_array[padrow-1][pad-1].seq) // already malloc()ed?
		    printf("ERROR DETECTED: Pad_array[%d][%d] already malloced\n",
			   padrow-1,pad-1);
		  Pad_array[padrow-1][pad-1].seq= 
		    (Sequence *)malloc(nseq*sizeof(Sequence));
		  if (Pad_array[padrow-1][pad-1].seq==NULL) {
		    cout << "failed to malloc() Sequence structures " << endl;
		    return FALSE;
		  }
		}
		pad++; // default to next pad in this padrow
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
	int nseq = Pad_array[padrow-1][pad-1].nseq;
	if (nseq) { // only if there are sequences on this pad
	  // allocate memory for Sequence arrays
	  // make sure we don't do it more than once
	  if (Pad_array[padrow-1][pad-1].seq==NULL) {
	    Pad_array[padrow-1][pad-1].seq= 
	      (Sequence *)malloc(nseq*sizeof(Sequence));
	    if (Pad_array[padrow-1][pad-1].seq==NULL) {
	      cout << "failed to malloc() Sequence structures " << endl;
	      return FALSE;
	    }
	  }
	}
      }
    }
  }

      //second pass ***** this can be incorporated into the first loop !!
  // This stupid compiler thinks rcb's scope extends here, so I removed int
  // The original line should go in when we upgrade to a better compiler 
  // for(int rcb = 0; rcb < 6; rcb++) {
  for(rcb = 0; rcb < 6; rcb++) {
    for(int mz = 0; mz < 3; mz++) {
      if (seqd_p[rcb][mz] == (classname(Bank_TPCSEQD) *)NULL) continue;
      u_char *adc_locn = (u_char *)adcd_p[rcb][mz]->ADC;
      int padrow=-1, pad=-1, lastbin=-2, pad_seq, oldstart = 0;
      int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
      int numseq = (4*len)/sizeof(short); // find # sequences this bank

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
	  //	  if (sector==11 && padrow==12 && pad==3)
// 	  if (sector==10 && padrow==5 && pad==115)
// 	    printf("%s::%d  RB%d MZ%d seq %d:  row %d pad %d\n",
// 		   __FILE__,__LINE__,rcb,mz,i,padrow,pad);
	  if (start >= oldstart) { // still on same pad
	  //is this sequence adjacent to previous one?
	    if (start>lastbin+1)  { //no
	      if (pad_seq>=Pad_array[padrow-1][pad-1].nseq)
		printf("sequence overrun %s %d row %d pad %d seq %d\n",
		       __FILE__,__LINE__,padrow,pad,pad_seq);
	      Pad_array[padrow-1][pad-1].seq[pad_seq].startTimeBin = start;
	      Pad_array[padrow-1][pad-1].seq[pad_seq].Length = len;
	      Pad_array[padrow-1][pad-1].seq[pad_seq].FirstAdc = adc_locn;
	      adc_locn +=len;
	      pad_seq++;
	    }
	    else { // yes: just update the length
	      if (pad_seq>Pad_array[padrow-1][pad-1].nseq)
		printf("sequence overrun %s %d row %d pad %d seq %d\n",
		       __FILE__,__LINE__,padrow,pad,pad_seq);
	      Pad_array[padrow-1][pad-1].seq[pad_seq-1].Length += len;
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

TPCV2P0_ZS_SR::~TPCV2P0_ZS_SR()
{
  cout << "Deleting TPCV2P0_ZS_SR" << endl;
  //free memory allocated for Sequence arrays
  for (int row=0; row<TPC_PADROWS; row++) {
    for (int pad=0; pad<tpcRowLen[row-1]; pad++) {
      void *memaddr = Pad_array[row][pad].seq;
      if (memaddr) free(memaddr);
    }
  }
}

int TPCV2P0_ZS_SR::getPadList(int PadRow, u_char **padList)
{
    // Construct the padlist array for this PadRow
  int pad;
  if (PadRow == 0 || PadRow > TPC_PADROWS) return -1;

  // Fill in padrows
  int npad=0;
  for (pad=1; pad<=tpcRowLen[PadRow-1]; pad++)
  {
    if (Pad_array[PadRow-1][pad-1].nseq) padlist[PadRow-1][npad++] = pad;
  }
  // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];

  return npad;
}

int TPCV2P0_ZS_SR::getSequences(int PadRow, int Pad, int *nSeq, 
				Sequence **SeqData)
{
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
}

int TPCV2P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq, 
				Sequence **SeqData)
{
  unsigned short PadRow = row_vs_fee[Fee][Pin];
  unsigned short Pad = pad_vs_fee[Fee][Pin];
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
}

int TPCV2P0_ZS_SR::MemUsed()
{
  return 0;
}

