/***************************************************************************
 * $Id: FTPV1P0_ZS_SR.cxx,v 1.12 2014/06/25 15:33:16 jeromel Exp $
 * Author: M.J. LeVine, H.Huemmler
 ***************************************************************************
 * Description: FTPC V1.0 Zero Suppressed Reader
 *      
 *
 * -------------change log ------------------------
 * 18-01-00 MJL 
 * 11-07-00 JLK Added fee_pin_FTPC.h include for ZS Reader
 * 
 ***************************************************************************
 * $Log: FTPV1P0_ZS_SR.cxx,v $
 * Revision 1.12  2014/06/25 15:33:16  jeromel
 * Code not used but erradicated use of flush
 *
 * Revision 1.11  2007/12/24 06:04:13  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.10  2003/09/02 17:55:31  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.9  2002/05/24 14:35:30  jcs
 * Use index pad_seq-1 when current sequence adjacent to previous sequence
 *
 * Revision 1.8  2002/03/11 16:40:24  jcs
 * return false if no FTPADCD bank or no FTPADCX bank found for requested sector
 *
 * Revision 1.7  2001/06/30 02:39:47  jcs
 * add forgotten include for assert.h
 *
 * Revision 1.6  2001/06/27 22:08:15  jcs
 * Initialize pad_seq
 *
 * Revision 1.5  2001/06/25 23:01:21  jcs
 * reconstruct from raw data if no FTPSEQD bank
 *
 * Revision 1.4  2001/06/19 20:51:22  jeromel
 * Commited for Janet S.
 *
 * Revision 1.3  2000/08/18 15:38:58  ward
 * New FTPC stuff from JKlay and Hummler.
 *
 * Revision 1.3  2000/07/11 14:44:06  jklay
 * Geometry file included, so call to 
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,Sequence **SeqData)
 * can now be properly implemented
 *
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
#include <Stiostream.h>
#include <assert.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "FTPV1P0.hh"
#include "StDaqLib/FTPC/fee_pin_FTPC.h"

using namespace OLDEVP;

FTPV1P0_ZS_SR::FTPV1P0_ZS_SR(int s, FTPV1P0_Reader *det)
{
//   cout << "Constructing FTPV1P0_ZS_SR" << endl;
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
    // noop
  }
  else {
    cout<<"No FTPADCD bank found for sector "<<sector<<endl;
    return FALSE;
  }
  adcx_p = detector->getBankFTPADCX(sector);
  if ((void *)adcx_p != NULL) {
    // noop
  }
  else {
    cout<<"No FTPADCX bank found for sector "<<sector<<endl;
    return FALSE;
  }
  seqd_p = detector->getBankFTPSEQD(sector);
  if ((void *)seqd_p != NULL) {
    // search through the  SEQD banks to build our tables of what's where

    int padrow=-1, pad=-1, lastbin=-2, oldstart=0;
    int len = seqd_p->header.BankLength - (sizeof(Bank_Header)/4);
    int numseq = (4*len)/sizeof(short); // find # sequences this bank
    // go through the SEQD twice. First time around just count the sequences
    // so we can malloc the Sequence arrays as needed
    for (i=0; i<numseq; i++) {
      if (seqd_p->sequence[i]<0) { //padrow, pad
        padrow = (seqd_p->sequence[i]>>8)& 0x7f;
        padrow = ((int)(padrow-1)/6)%2+1;
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
    int pad_seq=0; 
    oldstart = 0;
    len = seqd_p->header.BankLength - (sizeof(Bank_Header)/4);
    numseq = (4*len)/sizeof(short); // find # sequences this bank
    u_char *adc_locn = (u_char *)adcd_p->ADC;
    for (i=0; i<numseq; i++) {
      if (seqd_p->sequence[i]<0) { //padrow, pad
        padrow = (seqd_p->sequence[i]>>8)& 0x7f;
        padrow = ((int)(padrow-1)/6)%2+1;
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
	  return FALSE;
        }
      }
    }
    return TRUE;
  }
  else {
    //FTPSEQD bank doesn't exist
    // set up raw data pointers
    FTPV1P0_PADK_SR *padkr = detector->getPADKReader(sector);
    if (!padkr) return FALSE;
    classname(Bank_FTPADCR) *adcr =
      detector->getBankFTPADCR(sector) ; //pointer to ADC raw bank
    if (!adcr) return FALSE;
    classname(Bank_FTPCPPR) *cppr =
      detector->getBankFTPCPPR(sector) ; //pointer to CPP raw bank
    if (!cppr) return FALSE;

    // go through the CPPR/ADCR banks
    if (detector->ercpy->verbose)
      printf("reconstructing from RAW banks: SEC %d\n",sector);
    FTPPADK_entry ent;

    for (int row=1; row<=FTP_PADROWS; row++) {
      for (int pad=1; pad<=FTP_MAXPADS; pad++) {
        padkr->get(row, pad, &ent);
//         if((ent.mz != mz+1) || (ent.rb != rcb+1)) continue;
        struct ASIC_Cluster *clusters =
          (ASIC_Cluster *)(cppr->entry + 32*ent.offset);

        int lastbin = -2, len = 0;
        int i, start, stop=-1, newseq;

        for (i=0; i<FTP_MXSEQUENCE; i++) { //loop over ASIC sequences
          start = clusters[i].start_time_bin;
          if ((start < 0)||(start==255)||(start<=stop)) break;
          //protect against hardware quirks in VRAM
          newseq = (start>lastbin+1);//sequence not broken in pieces by MZ
          stop = clusters[i].stop_time_bin;
          //MJL: not used:   len = stop - start + 1;
          // catch bad values of start, stop
          if ( stop<start || start<0 || start>255 || stop<0 || stop>255) {
            struct EventInfo ei;
            ei = detector->ercpy->getEventInfo();
            printf("%s:%d: bad FTP cluster data detected\n",
                   __FILE__,__LINE__);
            printf("evt# %d , sector %d, row %d, pad %d\n",
                     ei.EventSeqNo,sector,row,pad);
//                     ei.EventSeqNo,sector+1,row,pad);  //JCS

            printf("cluster %d: start=0x%x   stop=0x%x\n",
                   i,clusters[i].start_time_bin,clusters[i].stop_time_bin);
            break; //stop processing this pad
          }
          lastbin = stop;
          if (newseq) Pad_array[row-1][pad-1].nseq++;
          //update the cluster counter for this pad
          // ...nseq now has CORRECT count of distinct ASIC sequences
        } // loop over ASIC sequences
        if (i==FTP_MXSEQUENCE) {
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
          if (i<FTP_MXSEQUENCE) {
            start = clusters[i].start_time_bin;

            if ((start < 0)||(start==255)) break;
            newseq = (start>lastbin+1);
            //sequence not broken in pieces by MZ
            stop = clusters[i].stop_time_bin;
            len = stop - start + 1;
            // catch bad values of start, stop
            if ( stop<start || start<0 || start>255 || stop<0 || stop>255) {
              struct EventInfo ei;
              ei = detector->ercpy->getEventInfo();
              printf("%s:%d: bad FTP cluster data detected\n",
                     __FILE__,__LINE__);
              printf("evt# %d , sector %d, row %d, pad %d\n",
                     ei.EventSeqNo,sector,row,pad);
//                     ei.EventSeqNo,sector+1,row,pad);    //JCS
              printf("cluster %d: start=0x%x   stop=0x%x\n",
                     i,clusters[i].start_time_bin,clusters[i].stop_time_bin);
              break; //stop processing this pad

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

          }
        } // loop on seq
      }   // loop on pad
    }     // loop on row
    return TRUE;
  }
}

FTPV1P0_ZS_SR::~FTPV1P0_ZS_SR()
{
//   cout << "Deleting FTPV1P0_ZS_SR" << endl;
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

  if (&nSeq) {
    return 1;           // If there are sequences, return 1.
  } else {              //This matches better with the ADCRawReader
    return 0;           //which returns 1 if raw ADC data exists.
  }
}

int FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq, 
				   Sequence **SeqData)
{
//#ifdef REAL_FEE_PIN_FOR_FTPC
  unsigned short PadRow = row_vs_fee_FTPC[Fee][Pin];
  unsigned short Pad = pad_vs_fee_FTPC[Fee][Pin];
  *nSeq = Pad_array[PadRow-1][Pad-1].nseq;   // number of sequences this pad
  *SeqData = Pad_array[PadRow-1][Pad-1].seq;  // pass back pointer to Sequence array 
  return 0;
//#else
//  printf("There is no table of FEE connections --- I quit\n");
//  return -1;
//#endif
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

