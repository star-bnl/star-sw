/***************************************************************************
 *      
 * $Id: SVTV1P0_ZS_SR.cxx,v 1.5 2003/09/02 17:55:33 perev Exp $
 *      
 * Author: J. Schambach
 *      
 ***************************************************************************
 *      
 * Description: SVT Zero Suppressed Reader
 *      
 ***************************************************************************
 *      
 * $Log: SVTV1P0_ZS_SR.cxx,v $
 * Revision 1.5  2003/09/02 17:55:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2003/02/20 21:39:45  ward
 * Remove voluminous warning message.
 *
 * Revision 1.3  2001/05/14 16:07:36  jschamba
 * corrected mezzanine and transition board evaluation in constructors
 *
 * Revision 1.2  2001/04/18 19:47:25  ward
 * StDaqLib/SVT stuff from Jo Schambach.
 *
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 *
 *      
 **************************************************************************/
#include <Stiostream.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "SVTV1P0.hh"
// 

SVTV1P0_ZS_SR::SVTV1P0_ZS_SR(int w, SVTV1P0_Reader *det)
{
  int w1;
  int numberOfLadders[3] = {8,12,16};
  int numberOfWafers[3] = {4,6,7};
  /*
   * The relationship between waferIndex (w [0..215]) and the triplet
   * (barrel[1..3], ladder[1..8,12,16], wafer[1..4,6,7]) is as follows:
   * w = 0 is wafer = 1, ladder = 1, barrel = 1
   * w = 1 is wafer = 2, ladder = 1, barrel = 1
   * ...
   * w = 4 is wafer = 1, ladder = 2, barrel = 1
   * ...
   * w = 32 is wafer = 1, ladder = 1, barrel = 2
   * ...
   * w = 215 is wafer = 7, ladder = 16, barrel = 3
   */

  if (w < (numberOfWafers[0] * numberOfLadders[0])) {
    barrel = 1;
    ladder = w/numberOfWafers[0] + 1;
    wafer = w%numberOfWafers[0] + 1;
  }
  else if( (w1 = w - numberOfWafers[0]*numberOfLadders[0]) <
	   (numberOfWafers[1] * numberOfLadders[1])) {
    barrel = 2;
    ladder = w1/numberOfWafers[1] + 1;
    wafer = w1%numberOfWafers[1] + 1;
  }
  else {
    w1 = w - numberOfWafers[0]*numberOfLadders[0]
      - numberOfWafers[1]*numberOfLadders[1];
    barrel = 3;
    ladder = w1/numberOfWafers[2] + 1;
    wafer = w1%numberOfWafers[2] + 1;
  }

  detector = det;

  // Now determine the hypersector, receiver board and mezzanine for this wafer
  switch (barrel) {
  case 1: 
    {
      const int mezzIndex[] = {1,2,2,1};
      if (wafer > 2) hyperSector = 1;
      else hyperSector = 3;
      rcb = (ladder-1)/2 + ladder;
      mezz = mezzIndex[wafer-1];
      transitionBoard = 2;
      break;
    }
  case 2:
    {
      const int mezzIndex[3][6] = { {1,1,3,3,1,1},
				    {1,2,3,3,2,1},
				    {2,2,3,3,2,2} };
      if (wafer > 3) hyperSector = 1;
      else hyperSector = 3;
      rcb = (ladder+2)/3;
      rcb *= 3;
      mezz = mezzIndex[(ladder-1)%3][wafer-1];
      transitionBoard = (ladder-1)%3 + 1;
      break;
    }
  case 3: 
    {
      const int mezzIndex[] = {2,2,3,3,3,1,1};
      if (wafer > (4-(ladder%2))) hyperSector = 1;
      else hyperSector = 3;
      rcb = (ladder-1)/4 + (ladder+1)/2;
      if (ladder%2) { //odd ladders 
	mezz = mezzIndex[wafer-1];
	if (hyperSector == 1) transitionBoard = 1;
	else transitionBoard = 3;
      }
      else { //even ladders
	mezz = mezzIndex[7-wafer];
	if (hyperSector == 1) transitionBoard = 3;
	else transitionBoard = 1;
      }
      break;
    }
  }

  if (rcb > 6) {
    hyperSector++;
    rcb -= 6;
  }
    
  /*
    printf("barrel %d, ladder %d, wafer %d hs %d rcb %d, mezz %d tb %d\n", 
    barrel, ladder, wafer, hyperSector, rcb, mezz, transitionBoard);
  */

  // NULLS in banks array
  adcd_p = (classname(Bank_SVTADCD) *)NULL;
  adcx_p = (classname(Bank_SVTADCX) *)NULL;
  seqd_p = (classname(Bank_SVTSEQD) *)NULL;

  for (int ii=0; ii<SVT_HYBRIDS; ii++) HybridSpacePts[ii] = 0;
}

SVTV1P0_ZS_SR::SVTV1P0_ZS_SR(int b, int l, int w, SVTV1P0_Reader *det)
{
  barrel = b;
  ladder = l;
  wafer = w;
  detector = det;

  // Now determine the hypersector, receiver board and mezzanine for this wafer
  switch (barrel) {
  case 1: 
    {
      const int mezzIndex[] = {1,2,2,1};
      if (wafer > 2) hyperSector = 1;
      else hyperSector = 3;
      rcb = (ladder-1)/2 + ladder;
      mezz = mezzIndex[wafer-1];
      transitionBoard = 2;
      break;
    }
  case 2:
    {
      const int mezzIndex[3][6] = { {1,1,3,3,1,1},
				    {1,2,3,3,2,1},
				    {2,2,3,3,2,2} };
      if (wafer > 3) hyperSector = 1;
      else hyperSector = 3;
      rcb = (ladder+2)/3;
      rcb *= 3;
      mezz = mezzIndex[(ladder-1)%3][wafer-1];
      transitionBoard = (ladder-1)%3 + 1;
      break;
    }
  case 3: 
    {
      const int mezzIndex[] = {2,2,3,3,3,1,1};
      if (wafer > (4-(ladder%2))) hyperSector = 1;
      else hyperSector = 3;
      rcb = (ladder-1)/4 + (ladder+1)/2;
      if (ladder%2) { //odd ladders 
	mezz = mezzIndex[wafer-1];
	if (hyperSector == 1) transitionBoard = 1;
	else transitionBoard = 3;
      }
      else { //even ladders
	mezz = mezzIndex[7-wafer];
	if (hyperSector == 1) transitionBoard = 3;
	else transitionBoard = 1;
      }
      break;
    }
  }

  if (rcb > 6) {
    hyperSector++;
    rcb -= 6;
  }
    
  // NULLS in banks array
  adcd_p = (classname(Bank_SVTADCD) *)NULL;
  adcx_p = (classname(Bank_SVTADCX) *)NULL;
  seqd_p = (classname(Bank_SVTSEQD) *)NULL;

  for (int ii=0; ii<SVT_HYBRIDS; ii++) HybridSpacePts[ii] = 0;
}

int SVTV1P0_ZS_SR::initialize()
{
  for (int hyb=0; hyb<SVT_HYBRIDS; hyb++) {
    for (int anode=0; anode<SVT_ANODES; anode++) {
      Anode_array[hyb][anode].nseq = 0;
      Anode_array[hyb][anode].seq = (Sequence *)0;
    }
  }

  // printf("Looking for ADCD, ADCX, and SEQD banks...\n");
  adcd_p = detector->getBankSVTADCD(hyperSector, rcb, mezz);
  if ((void *)adcd_p != NULL) {
    if (detector->ercpy->verbose) printf("found ADCD RB%d MZ%d\n",rcb,mezz);
    fflush(stdout);
  }
  adcx_p = detector->getBankSVTADCX(hyperSector, rcb, mezz);
  if ((void *)adcx_p != NULL) {
    if (detector->ercpy->verbose) printf("found ADCX RB%d MZ%d\n",rcb,mezz);
    fflush(stdout);
  }
  seqd_p = detector->getBankSVTSEQD(hyperSector, rcb, mezz);
  if ((void *)seqd_p != NULL) {
    if (detector->ercpy->verbose) printf("found SEQD RB%d MZ%d\n",rcb,mezz);
    fflush(stdout);
  }

  /***************************************************/
  // now go through the ( possibly found) SVTADCX bank
  // and see if our hybridIDs are in it
  /***************************************************/
  if (adcx_p == (classname(Bank_SVTADCX) *)NULL) {
    if (detector->ercpy->verbose)
      printf("SVTADCX for HS %d, Receiver %d, Mezz %d does not exist\n",
	     hyperSector, rcb, mezz);
    fflush(stdout);
    // SVTADCX bank doesn't exist
    // set up data pointers to raw data
    /* JS: this still needs to be done */

  }
  else { // SVTADCX bank exists
    // number of entries in SVTADCX bank
    int banklen = adcx_p->header.BankLength - (sizeof(Bank_Header)/4);
    int numXEntries = banklen/3;
    // number of entries in SVTSEQD bank
    banklen = seqd_p->header.BankLength - (sizeof(Bank_Header)/4);
    int numseq = (4*banklen)/sizeof(short);

    // now loop through the hybrids of this wafer
    for (int hybrid=1; hybrid<=SVT_HYBRIDS; hybrid++) {
      INT32 hybridID = 
	hybrid | 
	(wafer << 2) |
	(transitionBoard << 5);
      int i;
      for (i=0; i<numXEntries; i++)
	if (adcx_p->entry[i].hybrid == hybridID) break;
      if (i == numXEntries) {
	// Commented Feb 20 2003 by Herb Ward on request of
        // Jerome Lauret printf("HybridID 0x%x not found in ADCX bank\n", hybridID);
	continue;
      }

      // Found entry for this hybridID, process offsets
      int adcd_offset = adcx_p->entry[i].SVTADCD_offset;
      int seqd_offset = adcx_p->entry[i].SVTSEQD_offset;

      // seqd_offset is in bytes, sequence entries are shorts, correct:
      seqd_offset = seqd_offset / sizeof(short);

      /********************************************/
      // go through the SEQD twice:
      // First:  malloc the Sequence arrays needed
      // Second: fill in the Sequence structs
      /********************************************/
      i = seqd_offset;
      int anode = -1, oldstart=0;
      while (i < numseq) { 
	if (seqd_p->sequence[i] < 0) { // s_an_number word found
	  anode = (seqd_p->sequence[i])& 0xff;
	  INT32 seq_hybridID = (seqd_p->sequence[i]>>8)& 0x7f;
	  if (seq_hybridID != hybridID) break; //start of the next hybrid
	  anode = (seqd_p->sequence[i]) & 0xff;
	  oldstart = 0;
	  i++;
	}
	else { // normal sequence word found
	  unsigned short work = seqd_p->sequence[i];
	  int start = work>>6;
	  int len = work & 0x1f; if(len){/*nothing*/}
	  if (start >= oldstart) { // still on same pad
	    Anode_array[hybrid-1][anode-1].nseq++;
	    oldstart = start;
	    if (work & 0x20 ) { // last sequence ?
	      int nseq = Anode_array[hybrid-1][anode-1].nseq;
	      if (nseq) { // only if there are sequences on this anode
		// allocate memory for Sequence arrays
		if (Anode_array[hybrid-1][anode-1].seq) {
		  printf("ERROR DETECTED: Anode_array[%d][%d] already malloced\n",
			 hybrid-1, anode-1);
		  return FALSE;
		}
		Anode_array[hybrid-1][anode-1].seq = 
		  (Sequence *)malloc(nseq * sizeof(Sequence));
		if (Anode_array[hybrid-1][anode-1].seq == NULL) {
		  cout << "failed to malloc() Sequence structures " << endl;
		  return FALSE;
		}
	      }
	      anode++; // default to next anode in this hybrid
	      oldstart=0;
	    }
	    i++;
	    continue;
	  } // still on same pad
 	  else {     // starting new pad without bit 5 set!
	    if (detector->ercpy->verbose) printf("new pad detected with bit 5 clear!\n");
	    fflush(stdout);
	    
	    while ((seqd_p->sequence[i]>0) && (i<numseq)) i++; // skip until "switch=1"
	    
	    int nseq = Anode_array[hybrid-1][anode-1].nseq;
	    if (nseq) { // only if there are sequences on this pad
	      // allocate memory for Sequence arrays
	      // make sure we don't do it more than once
	      if (Anode_array[hybrid-1][anode-1].seq == NULL) {
		Anode_array[hybrid-1][anode-1].seq = 
		  (Sequence *)malloc(nseq*sizeof(Sequence));
		if (Anode_array[hybrid-1][anode-1].seq == NULL) {
		  cout << "failed to malloc() Sequence structures " << endl;
		  return FALSE;
		}
	      }
	    }  // if (nseq)
	  }    //  else { ...starting new pad without bit 5 set
	} // ...else { normal sequence word found
      }   // ...while (i < numseq

      /************************************************************/
      /* Now process SVTSEQD again to fill in the sequence arrays */
      /************************************************************/

      u_char *adc_locn = (u_char *)&(adcd_p->ADC[adcd_offset]);
      int anode_seq;
      i = seqd_offset;
      while (i < numseq) {
	if (seqd_p->sequence[i] < 0) { // anode specifier word
	  anode = (seqd_p->sequence[i])& 0xff;
	  if (anode == 255) break; // anode 255 is the last word as a filler
	  INT32 seq_hybridID = (seqd_p->sequence[i]>>8)& 0x7f;
	  if (seq_hybridID != hybridID) break; //start of the next hybrid
	  anode_seq = 0;
	  oldstart = 0;
	  i++;
	} // ...if (seqd_p->sequence[i] < 0
	else { // actual sequence word
	  unsigned short work = seqd_p->sequence[i];
	  int start = work>>6;
	  int len = work & 0x1f;
	  if (start >= oldstart) { // still on same pad
	    if (anode_seq >= Anode_array[hybrid-1][anode-1].nseq)
	      printf("sequence overrun %s %d hybrid %d anode %d seq %d\n",
		     __FILE__,__LINE__,hybrid,anode,anode_seq);
	    Anode_array[hybrid-1][anode-1].seq[anode_seq].startTimeBin = start;
	    Anode_array[hybrid-1][anode-1].seq[anode_seq].Length = len;
	    Anode_array[hybrid-1][anode-1].seq[anode_seq].FirstAdc = adc_locn;
	    adc_locn +=len;
	    anode_seq++;
	    oldstart = start;
	    if (work & 0x20) { //last sequence ?
	      anode++;    // default to next pad in this padrow
	      anode_seq = 0;
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
	    while (seqd_p->sequence[i]>0 && i<numseq) i++; 
	  }  // ...new anode starting with bit 5 set
	}    // ...else {  actual sequence word
      }      // ...while (i < numseq
    }        // ...for (int hybrid=1...
  }          // ...else {  SVTADCX bank exists

  return TRUE;
}

SVTV1P0_ZS_SR::~SVTV1P0_ZS_SR()
{
  // free memory allocated for Sequence arrays
  for (int hyb=0; hyb<SVT_HYBRIDS; hyb++) {
    for (int anode=0; anode<SVT_ANODES; anode++) {
      void *memaddr = Anode_array[hyb][anode].seq;
      if (memaddr) free (memaddr);
    }
    if (HybridSpacePts[hyb]) 
      free(HybridSpacePts[hyb]);
  }
}

int SVTV1P0_ZS_SR::getPadList(int Hybrid, u_char **anodeList)
{
  if ((Hybrid < 1) || (Hybrid > 2)) return -1;

  // fill in hybrid anode list
  int anode, nanode=0;
  for (anode=1; anode<241; anode++) {
    if (Anode_array[Hybrid-1][anode-1].nseq) 
      anodelist[Hybrid-1][nanode++] = anode;
  }

  *anodeList = &anodelist[Hybrid-1][0];

  return nanode;
}

int SVTV1P0_ZS_SR::getSequences(int Hybrid, int Anode, int *nSeq, 
				Sequence **SeqData)
{
  *nSeq = Anode_array[Hybrid-1][Anode-1].nseq; // number of sequences this anode
  *SeqData = Anode_array[Hybrid-1][Anode-1].seq; // pass back pointer to Sequence array

  return 0;
}

int SVTV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq, 
				Sequence **SeqData)
{
  return 0;
}


// Read the clusters (space points) found in the mezzanine cluster-finder
int SVTV1P0_ZS_SR::getSpacePts(int Hybrid, int *nSpacePts, SpacePt **SpacePts)
{
  classname(Bank_SVTMZCLD) *cld;
//  int numSpPts = 0; 
  INT32 hybridID =
    Hybrid | 
    (wafer << 2) |
    (transitionBoard << 5);

  *nSpacePts = 0;
  *SpacePts = HybridSpacePts[Hybrid-1];

  // make sure the array is empty to begin with
  if (HybridSpacePts[Hybrid-1]) 
    free(HybridSpacePts[Hybrid-1]);
  
  cld = detector->getBankSVTMZCLD(hyperSector, rcb, mezz);

  if (cld == (classname(Bank_SVTMZCLD) *)NULL) {
    // SVTMZCLD bank doesn't exist
    if (detector->ercpy->verbose)
      printf("SVTMZCLD for HS %d, Receiver %d, Mezz %d does not exist\n",
	     hyperSector, rcb, mezz);
    fflush(stdout);
  }
  else { // SVTMZCLD bank exists
    int numHybrids = cld->NumHybrids;
    if (numHybrids > 0) { // are there any entries?
      int *ptr = (int *)&cld->stuff;
      int i = 0;
      while (i < numHybrids) {
	INT32 cld_hybridID = *ptr++;
	int nsp = *ptr++; // number of space points this hybrid
	if (cld_hybridID == hybridID) { // found the right hybrid
	  if (detector->ercpy->verbose) 
	    cout << "Barrel " << barrel <<"ladder " << ladder <<"wafer "
		 << wafer << " hybrid " << Hybrid << ": found " 
		 << nsp << " space pts" << endl;
  
	  HybridSpacePts[Hybrid-1] = (SpacePt *)malloc(nsp *sizeof(struct SpacePt));
	  if (HybridSpacePts[Hybrid] == NULL) {
	    cout << "failed to malloc() Space Point structures " << endl;
	    return FALSE;
	  }
	  // now fill in the spacepoints
	  for (int isp=0; isp<nsp; isp++, ptr+=2) {
	    HybridSpacePts[Hybrid-1][isp] = *(SpacePt *)ptr;
	  }
	  *nSpacePts = nsp;
	  break;
	}
	ptr += 2*nsp; // move pointer over spacepoints for this hybrid
	i++;
      } // ...while (int i < numHybrids
    }   // ...if (cld->NumHybrids > 0
  }     // ...else { SVTMZCLD bank exists
  
  return TRUE;
}


int SVTV1P0_ZS_SR::MemUsed()
{
  return 0;
}

