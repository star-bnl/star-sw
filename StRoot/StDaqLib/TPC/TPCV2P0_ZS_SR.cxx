/***************************************************************************
 * $Id: TPCV2P0_ZS_SR.cxx,v 1.9 1999/07/27 23:08:08 levine Exp $
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: TPC V2.0 Zero Suppressed Reader
 *      
 *
 *   change log
 * 28-May-99 MJL on encountering a missing TPCSEQD bank,
 *    tries to fill in the Pad_array structs using the RAW banks (if present)
 * 31-May-99 MJL terminate on encountering pad 255
 * 03-Jun-99 MJL added return TRUE to TPCV2P0_ZS_SR::initialize()
 * 10-Jun-99 IS (Iwona Sakrejda) - The compiler we are using currently in Root
 *   cannot contain the scope of the "for" loop index within parenthesis.
 *   So if the next loop uses same index, it cannot be Declared again.
 *   I made 2 for rcg  and marked them both in  the code.
 * 20-Jun-99 MJL corrected Iwona's fix, which depends on the "leak" out of the
 *  'for' scope to define the variable elsewhere. At the very least this makes 
 *   gdb very confused. Now the offending rcb is defined outside the scope of
 *   all for loops
 * 21-Jun-99 MJL change behavior on discovering a "bit 5" error. 
 *   Now skip sequences
 *   until next "switch=1" sequence
 * 23-Jun-99 MJL most output now supressed with EventReader.verbose
 * 07-Jul-99 MJL if no banks found for a given rcb,mz just skip it. 
 *           No longer return FALSE
 * 27-Jul-99 MJL init RowSpacePts = 0 to make destructor robust
 *
 ***************************************************************************
 * $Log: TPCV2P0_ZS_SR.cxx,v $
 * Revision 1.9  1999/07/27 23:08:08  levine
 * bug fix: initialize RowSpacePts[] =0 to make destructor robust
 *
 * Revision 1.8  1999/07/21 21:15:41  levine
 *
 * TPCV2P0_ZS_SR.cxx changed to include the TPCV2P0_ZS_SR::getSpacePts()
 * (cluster-finder reader). TPCV1P0_ZS_SR.cxx changed to include empty
 * version of the same method.
 *
 * Revision 1.7  1999/07/07 19:55:43  levine
 * Now behaves correctly when encountering a partially populated (e.g., one RB) sector
 *
 * Revision 1.6  1999/07/02 04:43:24  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/
#include <iostream>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV2P0.hh"
#include "fee_pin.h"
// 

TPCV2P0_ZS_SR::TPCV2P0_ZS_SR(int s, TPCV2P0_Reader *det)
{
  sector = s-1; // convert the sector into internal representation
  detector = det;
  if (detector->ercpy->verbose) cout << "Constructing TPCV2P0_ZS_SR" << endl;

  // NULLS in banks array
  memset((char *)adcd_p, 0, sizeof(adcd_p));
  memset((char *)adcx_p, 0, sizeof(adcx_p));
  memset((char *)seqd_p, 0, sizeof(seqd_p));

  for(int ii=TPC_PADROWS-1;ii>=0;ii--) RowSpacePts[ii]=0;   

}

int TPCV2P0_ZS_SR::initialize()
{
  for (int row=0; row<TPC_PADROWS; row++) {
    for (int pad=0; pad<TPC_MAXPADS; pad++) {
      Pad_array[row][pad].nseq=0;
      Pad_array[row][pad].seq= (Sequence *)0;
    }
  }
  int rcb; // define for following for loops
  // store pointers to the ADCD, ADCX, SEQD banks
  for(rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      adcd_p[rcb][mz] = detector->getBankTPCADCD(sector,rcb,mz);
      if ((void *)adcd_p[rcb][mz] != NULL) {
	if (detector->ercpy->verbose) printf("found ADCD RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
      adcx_p[rcb][mz] = detector->getBankTPCADCX(sector,rcb,mz);
      if ((void *)adcx_p[rcb][mz] != NULL) {
	if (detector->ercpy->verbose) printf("found ADCX RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
      seqd_p[rcb][mz] = detector->getBankTPCSEQD(sector,rcb,mz);
      if ((void *)seqd_p[rcb][mz] != NULL) {
	if (detector->ercpy->verbose) printf("found SEQD RB%d MZ%d\n",rcb+1,mz+1);
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
	//	if (!padkr) return FALSE;
	if (!padkr) continue;
	classname(Bank_TPCADCR) *adcr =
	  detector->getBankTPCADCR(sector,rcb,mz) ; //pointer to ADC raw bank
	//	if (!adcr) return FALSE;
	if (!adcr) continue;
	classname(Bank_TPCCPPR) *cppr =
	  detector->getBankTPCCPPR(sector,rcb,mz) ; //pointer to CPP raw bank
	//	if (!cppr) return FALSE;
	if (!cppr) continue;

	// go through the CPPR/ADCR banks
	if (detector->ercpy->verbose) 
	  printf("reconstructing from RAW banks: SEC%d RB%d MZ%d\n",sector,rcb+1,mz+1);
	PADK_entry ent;

	for (int row=1; row<=TPC_PADROWS; row++) {
	  for (int pad=1; pad<=tpcRowLen[row-1]; pad++) {
	    padkr->get(row, pad, &ent);
	    if((ent.mz != mz+1) || (ent.rb != rcb+1)) continue;
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
	if (detector->ercpy->verbose) 
	  printf("TPCSEQD found sector %d  RB%d MZ%d\n",sector+1,rcb+1,mz+1);
	int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
	int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
	int numseq = (4*len)/sizeof(short); // find # sequences this bank
	// go through the SEQD twice:
	// First malloc the Sequence arrays needed
	// Second fill in the Sequence structs

	// get a pointer to the CPPr bank for debugging only
	classname(Bank_TPCCPPR) *cppr =
	  detector->getBankTPCCPPR(sector,rcb,mz) ; //pointer to CPP raw bank
	// get a pointer to the PADK bank for debugging only
	//	TPCV2P0_PADK_SR *padkr = detector->getPADKReader(sector);

	int i=0;
	while (i<numseq) {
	  if (seqd_p[rcb][mz]->sequence[i]<0) { //padrow, pad
	    padrow = (seqd_p[rcb][mz]->sequence[i]>>8)& 0x7f;
	    pad = (seqd_p[rcb][mz]->sequence[i])& 0xff;
	    if (pad==255) break; 
	    //pad 255 exists only in extraneous last word
	    oldstart = 0;
	    lastbin = -2;
	    i++;
	  }
	  else { // start|lastseq|len
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
	      i++;
	      continue;
	    }
	    else {     // starting new pad without bit 5 set!
	      if (detector->ercpy->verbose) printf("new pad detected with bit 5 clear!\n");
	      fflush(stdout);
	      // for debug only:
	      // classname(Bank_TPCPADK) *padk = detector->getBankTPCPADK(sector, rcb, mz);

	      while (seqd_p[rcb][mz]->sequence[i]>0 && i<numseq) i++; // skip until "switch=1"
	      //	      return FALSE;
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
	      }  // if (nseq)
	    }    //  else { ...starting new pad without bit 5 set
	  }      //  else { ...start|lastseq|len
	}        // while (i<numseq)
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
  // for(int rcb = 0; rcb < 6; rcb++) { [Iwona]
  for(rcb = 0; rcb < 6; rcb++) {
    for(int mz = 0; mz < 3; mz++) {
      if (seqd_p[rcb][mz] == (classname(Bank_TPCSEQD) *)NULL) continue;
      u_char *adc_locn = (u_char *)adcd_p[rcb][mz]->ADC;
      int padrow=-1, pad=-1, lastbin=-2, pad_seq, oldstart = 0;
      int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
      int numseq = (4*len)/sizeof(short); // find # sequences this bank
      int i=0;
      while  (i<numseq)  {
	if (seqd_p[rcb][mz]->sequence[i]<0) { //padrow, pad
	  padrow = (seqd_p[rcb][mz]->sequence[i]>>8)& 0x7f;
	  pad = (seqd_p[rcb][mz]->sequence[i])& 0xff;
	  if (pad==255) break; //pad 255 exists only in extraneous last word
	  pad_seq = 0;
	  oldstart=0;
	  lastbin = -2;
	  i++;
	}
	else { // (start|lastseq|len)
	  unsigned short work = seqd_p[rcb][mz]->sequence[i];
	  int start = work>>6;
	  int len = work & 0x1f;
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
	    oldstart = start;
	    if (work & 0x20) {//last sequence ?
	      pad++;    // default to next pad in this padrow
	      pad_seq = 0;
	      lastbin = -2; // set lastbin for new pad	
	      oldstart=0;
	    }
	    i++;
	  }
	  else {    // starting new pad without bit 5 set!
	    if (detector->ercpy->verbose) printf("new pad detected with bit 5 clear!\n");
	    fflush(stdout);
	    while (seqd_p[rcb][mz]->sequence[i]>0 && i<numseq) i++; // skip until next "switch=1"
	  }
	}  // (start|lastseq|len)
      }    // while (i<numseq)
    }
  }
  return TRUE;
}

TPCV2P0_ZS_SR::~TPCV2P0_ZS_SR()
{
  if (detector->ercpy->verbose) cout << "Deleting TPCV2P0_ZS_SR" << endl;
  //free memory allocated for Sequence arrays
  for (int row=0; row<TPC_PADROWS; row++) {
    for (int pad=0; pad<tpcRowLen[row-1]; pad++) {
      void *memaddr = Pad_array[row][pad].seq;
      if (memaddr) free(memaddr);
    }
  }
  for (int rrow=0; rrow<TPC_PADROWS; rrow++) {
    if (RowSpacePts[rrow]) free(RowSpacePts[rrow]);
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


// Read the clusters (space points) found in the mezzanine cluster-finder
int TPCV2P0_ZS_SR::getSpacePts(int PadRow, int *nSpacePts, SpacePt **SpacePts)
{
  int rb, row;
  classname(Bank_TPCMZCLD) *cld;
  int numSpPts = 0; // keep running total for malloc

  for (row=0; row<TPC_PADROWS; nsptrow[row++]=0);

  for (rb=0; rb<6; rb++) {
    for (int mz=0; mz<3; mz++) {
      //pointer to TPCMZCLD bank
      cld_p[rb][mz] = detector->getBankTPCMZCLD(sector,rb,mz) ; 
      cld = cld_p[rb][mz];
      if (!cld) continue;
      int *ptr = (int *)&cld->stuff;
      for (int ir=0; ir<cld->NumRows; ir++){
	int row = *ptr++;
	int nsp = *ptr++; // bump pointer to beginning of space points
	nsptrow[row-1] += nsp;  // add num space pts to running total
	ptr += 2*nsp;
      }
    }
  }
  if (detector->ercpy->verbose) cout << "sector "<<sector<<": found " 
				     <<numSpPts<<" space pts" <<endl;
  
  for (row=0; row<TPC_PADROWS; row++) {
    RowSpacePts[row] = (SpacePt *)malloc(nsptrow[row]*sizeof(struct SpacePt));
    if (RowSpacePts[row]==NULL) {
      cout << "failed to malloc() Space Point structures " << endl;
      return FALSE;
    }
  }

  for (rb=0; rb<6; rb++) {
    for (int mz=0; mz<3; mz++) {
      cld = cld_p[rb][mz];  // pointer to TPCMZCLD bank
      if (!cld) continue;
      int *ptr = &cld->stuff[0];
      for (int ir=0; ir<cld->NumRows; ir++){
	int row = *ptr++;
	int nsp = *ptr++;   // bump pointer to beginning of space points
	for (int isp=0; isp<nsp; isp++, ptr+=2) {
	  RowSpacePts[row-1][isp] = *(SpacePt *)ptr;
	}
      }
    }
  }
  *nSpacePts = nsptrow[PadRow-1];
  *SpacePts = RowSpacePts[PadRow-1];
  return TRUE;
}


int TPCV2P0_ZS_SR::MemUsed()
{
  return 0;
}

