/***************************************************************************
 * $Id: TPCV2P0_ZS_SR.cxx,v 1.29 2008/01/20 00:35:42 perev Exp $
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
 * 29-Aug-99 MJL #include <Stiostream.h> for HP platform
 * 07-Feb-00 MJL add diagnostics when bad cluster data encountered
 * 23-Feb-00 MJL fix bug (~line 229) which didn't handle split sequences
 *               correctly. Thanks to Herb Ward.
 * 24-Feb-00 MJL clean up loop structure, add comments everywhere
 * 28-Feb-00 MJL protect against hardware quirks in VRAM
 * 29-Feb-00 MJL change loop logic line 220 (test seqCnt, not i)
 *
 ***************************************************************************
 * $Log: TPCV2P0_ZS_SR.cxx,v $
 * Revision 1.29  2008/01/20 00:35:42  perev
 * Supress redundant print
 *
 * Revision 1.28  2007/12/24 06:04:33  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.27  2004/03/25 21:07:32  ward
 * Suppress more debug messages.
 *
 * Revision 1.26  2004/03/24 18:44:58  ward
 * Suppress debug messages.
 *
 * Revision 1.25  2004/03/10 05:53:08  jeromel
 * Left a debug line in (to remove later)
 *
 * Revision 1.24  2004/03/04 21:51:29  ward
 * Replaced MERGE_SEQUENCES with a StDAQMaker chain parameter, as suggested by Landgraf and Lauret.
 *
 * Revision 1.23  2004/02/03 20:07:18  jml
 * added MERGE_SEQUENCES define
 *
 * Revision 1.22  2003/10/01 20:55:06  ward
 * Protection against empty TPCSEQD banks.
 *
 * Revision 1.21  2003/09/02 17:55:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.20  2000/11/15 21:02:02  ward
 * Another .daq corruption check.  It is on the TPCSEQD bank.
 *
 * Revision 1.19  2000/11/07 16:30:27  ward
 * New check for .daq corruption, with kStErr from St_tpcdaq_Maker.
 *
 * Revision 1.18  2000/07/05 09:17:26  ward
 * Fixed a one-off subscripting err & removed ref to a deleted obj.
 *
 * Revision 1.17  2000/06/22 21:19:16  ward
 * added stop<start to bad cluster criteria
 *
 * Revision 1.16  2000/02/29 20:20:29  levine
 * corrected loop logic line 220
 *
 * Revision 1.15  2000/02/28 22:02:51  levine
 * add protection against illegal CPP sequences - was causing unitialized pointers
 *
 * Revision 1.14  2000/02/08 21:34:06  levine
 * added diagnostics for bad cluster pointers
 *
 * Revision 1.13  2000/01/04 20:55:05  levine
 * Implemented memory-mapped file access in EventReader.cxx. Old method
 * (via seeks) is still possible by setting mmapp=0 in
 *
 * 	getEventReader(fd,offset,(const char *)logfile,mmapp);
 *
 *
 * but memory-mapped access is much more effective.
 *
 * Revision 1.12  1999/12/09 02:50:41  levine
 * #define changed to get rid of unused fee array defs for TPCv?P0_ZS_SR.cxx
 * only
 *
 * Revision 1.11  1999/12/07 23:10:46  levine
 * changes to silence the gcc compiler warnings
 *
 * Revision 1.10  1999/09/02 21:47:12  fisyak
 * HP corrections
 *
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
#include <Stiostream.h>
#include <assert.h>
#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV2P0.hh"

#define MAKE_THE_DAMNED_COMPILER_SILENT
// #define MERGE_SEQUENCES replaced by mMergeSequences, Herb Ward, Mar 4 2004.

#include "fee_pin.h"
// 
using namespace OLDEVP;

TPCV2P0_ZS_SR::TPCV2P0_ZS_SR(int s, TPCV2P0_Reader *det, char mergeSequences)
{
  assert(mergeSequences==0||mergeSequences==1); 
  mMergeSequences=mergeSequences;

  sector   = s-1;               // convert the sector into internal representation
  detector = det;
  if (detector->ercpy->verbose){
    cout << "Constructing TPCV2P0_ZS_SR" << endl
	 << "  MergeSequence is " << (mMergeSequences?"ON":"OFF") << endl;
  }

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

  // *** DEBUG LINE *** COMMENT LATER ***
//VP  cout << "TPCV2P0_ZS_SR::initialize : mMergeSequences=" << (mMergeSequences?"ON":"OFF") << endl;

  int rcb; // define for following for loops
  // store pointers to the ADCD, ADCX, SEQD banks
  for(rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      adcd_p[rcb][mz] = detector->getBankTPCADCD(sector,rcb,mz);
      if ((void *)adcd_p[rcb][mz] != NULL) {
	// if (detector->ercpy->verbose) printf("found ADCD RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
      adcx_p[rcb][mz] = detector->getBankTPCADCX(sector,rcb,mz);
      if ((void *)adcx_p[rcb][mz] != NULL) {
	// if (detector->ercpy->verbose) printf("found ADCX RB%d MZ%d\n",rcb+1,mz+1);
	fflush(stdout);
      }
      seqd_p[rcb][mz] = detector->getBankTPCSEQD(sector,rcb,mz);
      if ((void *)seqd_p[rcb][mz] != NULL) {
	// if (detector->ercpy->verbose) printf("found SEQD RB%d MZ%d\n",rcb+1,mz+1);
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
	    int i, start, stop=-1, newseq;
	    for (i=0; i<TPC_MXSEQUENCE; i++) { //loop over ASIC sequences
	      start = clusters[i].start_time_bin;
    	      if ((start < 0)||(start==511)||(start<=stop)) break;
	      //protect against hardware quirks in VRAM
	      newseq = (start>lastbin+1);//sequence not broken in pieces by MZ
	      stop = clusters[i].stop_time_bin;
	      //MJL: not used:   len = stop - start + 1;
	      // catch bad values of start, stop
	      if ( stop<start || start<0 || start>511 || stop<0 || stop>511) {
		struct EventInfo ei;
		ei = detector->ercpy->getEventInfo();
		printf("%s:%d: bad TPC cluster data detected\n",
		       __FILE__,__LINE__); 
		printf("evt# %d , sector %d, RB %d, MZ %d row %d, pad %d\n", 
			 ei.EventSeqNo,sector+1,rcb+1,mz+1,row,pad);
		printf("cluster %d: start=0x%x   stop=0x%x\n",
		       i,clusters[i].start_time_bin,clusters[i].stop_time_bin);
		break; //stop processing this pad
	      }
	      lastbin = stop;
	      if (newseq) Pad_array[row-1][pad-1].nseq++;
	      //update the cluster counter for this pad
	      // ...nseq now has CORRECT count of distinct ASIC sequences
	    } // loop over ASIC sequences
	    if (i==TPC_MXSEQUENCE) {
	      //did we overflow the ASIC cluster pointer array
	      // do the rest here by hand
	    }

	    // finished scanning for total number of distinct sequences
	    // this scan is only used to determine the number of sequences
	    int nseq = Pad_array[row-1][pad-1].nseq;
	    if (!nseq) continue; // only if there are sequences on this pad
	    Pad_array[row-1][pad-1].seq= 
	      (Sequence *)malloc(nseq*sizeof(Sequence)); 
	    if (Pad_array[row-1][pad-1].seq==NULL) {
	      cout << "failed to malloc() Sequence structures " << endl;
	      return FALSE;
	    }
	    lastbin = -2;
	    len = 0;
	    int seqCnt = 0;   

	    // loop over sequences, filling in structs
	    for (i=0; seqCnt<nseq; i++) { //loop over ASIC sequences
	      if (i<TPC_MXSEQUENCE) {
		start = clusters[i].start_time_bin;
		if ((start < 0)||(start==511)) break;
		newseq = (start>lastbin+1);
		     //sequence not broken in pieces by MZ
		stop = clusters[i].stop_time_bin;
		len = stop - start + 1;
		// catch bad values of start, stop
		if ( stop<start || start<0 || start>511 || stop<0 || stop>511) {
		  struct EventInfo ei;
		  ei = detector->ercpy->getEventInfo();
		  printf("%s:%d: bad TPC cluster data detected\n",
			 __FILE__,__LINE__); 
		  printf("evt# %d , sector %d, RB %d, MZ %d row %d, pad %d\n", 
			 ei.EventSeqNo,sector+1,rcb+1,mz+1,row,pad);
		  printf("cluster %d: start=0x%x   stop=0x%x\n",
			 i,clusters[i].start_time_bin,clusters[i].stop_time_bin);
		  break; //stop processing this pad
		}
		lastbin = stop;
		if (newseq) {
		  int offset = ent.offset * padkr->getADCBytes() + start;
		  Pad_array[row-1][pad-1].seq[seqCnt].startTimeBin = start;
		  Pad_array[row-1][pad-1].seq[seqCnt].Length = len;
		  Pad_array[row-1][pad-1].seq[seqCnt].FirstAdc = 
		             (u_char *)(adcr->ADC + offset);
		  seqCnt++;
		}
		else { // continuation of previous sequence
		  assert( seqCnt>=1 && seqCnt<=nseq );
		  Pad_array[row-1][pad-1].seq[seqCnt-1].Length += len;
		}
	      }
	      else {//did we overflow the ASIC cluster pointer array?
		// do the rest here by hand (TBDL)
	      }	     
	    } // loop on seq
	  }   // loop on pad
	}     // loop on row
      }       // if TPCSEQD doesn't exist 
      // use this loop when SEQD exists:
      else { //TPCSEQD bank exists
	// if (detector->ercpy->verbose) printf("TPCSEQD found sector %d  RB%d MZ%d\n",sector+1,rcb+1,mz+1);

	int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
	int len = seqd_p[rcb][mz]->header.BankLength - (sizeof(Bank_Header)/4);
	int numseq = (4*len)/sizeof(short); // find # sequences this bank
	// go through the SEQD twice:
	// First:  malloc the Sequence arrays needed
	// Second: fill in the Sequence structs

	// get a pointer to the CPPr bank for debugging only
	//	classname(Bank_TPCCPPR) *cppr =
	//detector->getBankTPCCPPR(sector,rcb,mz) ; //pointer to CPP raw bank
	// get a pointer to the PADK bank for debugging only
	//	TPCV2P0_PADK_SR *padkr = detector->getPADKReader(sector);

	int i=0,iOld=-1; // Herb, 02nov2000
	while (i<numseq) { // loop over the #sequences in TPCSEQD
          if(i<=iOld) return FALSE;   // Herb, 02nov2000, as a check for corruption in .daq file
          iOld=i;   // Herb, 02nov2000
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

              if(mMergeSequences) { if (start>lastbin+1)  Pad_array[padrow-1][pad-1].nseq++; }
	      else { Pad_array[padrow-1][pad-1].nseq++;  }

	      // don't increment nseq if sequences are adjacent!
	      lastbin = start+len-1;
	      oldstart = start;
	      if (work & 0x20 ) {//last sequence ?
		assert(padrow>=1&&pad>=1); // Herb_Oct_01_2003, added this assert.
		int nseq = Pad_array[padrow-1][pad-1].nseq;
		if (nseq) { // only if there are sequences on this pad
		  //allocate memory for Sequence arrays
		  if (Pad_array[padrow-1][pad-1].seq) {
		    printf("ERROR DETECTED: Pad_array[%d][%d] already malloced\n",
			   padrow-1,pad-1);
		    return FALSE; // Herb, 15nov2000.  Causes root4star to skip the event.
		  }
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
	      assert(padrow>=1&&pad>=1); // Herb_Oct_01_2003, added this assert.
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
	int nseq;

	if (padrow <1 || pad <1) { // Jerome and Herb, Oct 1 2003.
	  nseq = 0;
	  // if (iOld==-1) (void) printf("Did not loop over bank TPCSEQD");
	  // else          (void) printf("Sequence=%d for SEQD bank\n",numseq);
	} else {
	  nseq = Pad_array[padrow-1][pad-1].nseq;
	}

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
	  }// if Pad_array[padrow-1][pad-1].seq==NULL
	}// if (nseq)

	// process TPCSEQD to fill in the Pad_array[][].seq structs
	u_char *adc_locn = (u_char *)adcd_p[rcb][mz]->ADC;
	padrow=-1; pad=-1; lastbin=-2; oldstart = 0; i=0;
	int pad_seq;

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

              if(mMergeSequences) {  // Replaces an ifdef MERGE_SEQUENCES, Herb Ward, Mar 4 2004
		if (start>lastbin+1)  {   // Not adjoining previous sequence, no merge!
		  if (pad_seq>=Pad_array[padrow-1][pad-1].nseq) {
		    printf("sequence overrun %s %d row %d pad %d seq %d\n",
			   __FILE__,__LINE__,padrow,pad,pad_seq);
		  }

		  Pad_array[padrow-1][pad-1].seq[pad_seq].startTimeBin = start;
		  Pad_array[padrow-1][pad-1].seq[pad_seq].Length = len;
		  Pad_array[padrow-1][pad-1].seq[pad_seq].FirstAdc = adc_locn;
		  adc_locn +=len;
		  pad_seq++;
		}
		else { // Merge sequence!
		  assert ( pad_seq>=1 && pad_seq<=Pad_array[padrow-1][pad-1].nseq);
		  
		  if (pad_seq>Pad_array[padrow-1][pad-1].nseq)
		    printf("sequence overrun %s %d row %d pad %d seq %d\n",
			   __FILE__,__LINE__,padrow,pad,pad_seq);
		  Pad_array[padrow-1][pad-1].seq[pad_seq-1].Length += len;
		  adc_locn +=len;
		}
              } else { // if(mMergeSequences   Herb replaces an #else, Mar 4 2004.

		if (pad_seq>=Pad_array[padrow-1][pad-1].nseq) {
		  printf("sequence overrun %s %d row %d pad %d seq %d\n",
			 __FILE__,__LINE__,padrow,pad,pad_seq);
		}
		
		Pad_array[padrow-1][pad-1].seq[pad_seq].startTimeBin = start;
		Pad_array[padrow-1][pad-1].seq[pad_seq].Length = len;
		Pad_array[padrow-1][pad-1].seq[pad_seq].FirstAdc = adc_locn;
		adc_locn +=len;
		pad_seq++;
              } // if(mMergeSequences)  Herb Ward, Mar 4 2004.
 
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
	      if (detector->ercpy->verbose) {
		printf("new pad detected with bit 5 clear!\n");
		fflush(stdout);
	      }
	       // skip until next "switch=1"
	      while (seqd_p[rcb][mz]->sequence[i]>0 && i<numseq) i++; 
	    }  // new pad starting with bit 5 set
	  }    // (start|lastseq|len)
	}      // while (i<numseq)
      }//TPCSEQD bank exists
    }// loop over mz
  }// loop over rcb

  return TRUE;
}

TPCV2P0_ZS_SR::~TPCV2P0_ZS_SR()
{

  // Herb commented this Jul 4 2000, because Victor 
  // deletes the reader object object before calling this function (see 
  // StDAQReader::readEvent), so that ercpy is not defined.
  // if (detector->ercpy->verbose) cout << "Deleting TPCV2P0_ZS_SR" << endl;


  //free memory allocated for Sequence arrays
  for (int row=0; row<TPC_PADROWS; row++) {
    for (int pad=0; pad<tpcRowLen[row]; pad++) { // Herb changed "row-1" to "row", Jul 4 2000.
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

