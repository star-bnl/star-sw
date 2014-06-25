/***************************************************************************
 *      
 * $Id: SVTV1P0_ZS_SR.cxx,v 1.9 2014/06/25 15:33:16 jeromel Exp $
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
 * Revision 1.9  2014/06/25 15:33:16  jeromel
 * Code not used but erradicated use of flush
 *
 * Revision 1.8  2007/12/24 06:04:28  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.7  2007/08/07 19:44:10  perev
 * Gene scalers added
 *
 * Revision 1.6  2007/01/04 21:27:51  jml
 * zero suppressed reader no longer uses adcx, only seqd.  Fixes bug from early 2005
 *
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
using namespace OLDEVP;
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
//   printf("zs init\n");

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
  }

  seqd_p = detector->getBankSVTSEQD(hyperSector, rcb, mezz);

  if ((void *)seqd_p != NULL) {
    if (detector->ercpy->verbose) printf("found SEQD RB%d MZ%d\n",rcb,mezz);
  }

  if((adcd_p == NULL) || (seqd_p == NULL)) {  // perfectly valid, just no data...
    return TRUE;
  }

//   printf("got banks  (adcd_p = 0x%x seqd = 0x%x)\n",adcd_p,seqd_p);

  // number of entries in SVTSEQD bank
  int banklen = seqd_p->header.BankLength - (sizeof(Bank_Header)/4);
  int numseq = (4*banklen)/sizeof(short);

  // now loop through the hybrids of this wafer
  for (int hybrid=1; hybrid<=SVT_HYBRIDS; hybrid++) {
    INT32 hybridID = 
      hybrid | 
      (wafer << 2) |
      (transitionBoard << 5);
    int i;

    int adcd_offset = 0;
    int seqd_offset = 0;
    int found = 0;
    for(i=0;i<numseq;i++) {
      unsigned short x = seqd_p->sequence[i];

      if(x & 0x8000) {    // This is an index entry...
//  	printf("[%d] index %d/%d\n",
//  	       i,(x & 0x7fff)/256,(x&0x7fff)%256);

	if((x & 0x7fff) / 256 == hybridID) {
	  found = 1;
	  seqd_offset = i;
	  break;
	}
      }
      else {              // This is a sequence entry...
//  	printf("[%d] seq %d %d %d\n",
//  	       i,
//  	       (x & 0x7fc0)>>6, 
//  	       x & 0x20, 
//  	       x & 0x1f);

	adcd_offset += x & 0x1f;   // in adc counts...
      }
    }



 //    printf("hybridID = %d  found = %d  adcd_off = %d,  seqd_off = %d\n",
//  	   hybridID, found, adcd_offset, seqd_offset);
   

    if(!found) continue;


    /********************************************/
    // go through the SEQD twice:
    // First:  malloc the Sequence arrays needed
    // Second: fill in the Sequence structs
    /********************************************/
    i = seqd_offset;
    int anode = -1, oldtb=0;
    while (i < numseq) { 
      if (seqd_p->sequence[i] & 0x8000) { // index descriptor
	anode = (seqd_p->sequence[i])& 0xff;
	INT32 seq_hybridID = (seqd_p->sequence[i]>>8)& 0x7f;

	if (seq_hybridID != hybridID) break; //start of the next hybrid
	if (anode == 255) break;
	oldtb=0;
      }
      else { // sequence descriptor
	unsigned short work = seqd_p->sequence[i];
	int tb = (work>>6) & 0xff;
//	int len = work & 0x1f;
	int last = work & 0x20;

	if(oldtb > tb) {
	  printf("Time bins are messed up old=%d, new=%d\n",oldtb,tb);
	}

	oldtb = tb;

	Anode_array[hybrid-1][anode-1].nseq++;
	if(last) {
	  char *t = (char *)malloc(Anode_array[hybrid-1][anode-1].nseq *
			   sizeof(Sequence));
      
	  Anode_array[hybrid-1][anode-1].seq = (Sequence *)t;

	  if(!t) {
	    printf("Failed to malloc() sequence structures\n");
	    return FALSE;
	  }

	  anode++;
	  oldtb = 0;
	}
      }
      i++;
    }

//     printf("pass1 done\n");

    /************************************************************/
    /* Now process SVTSEQD again to fill in the sequence arrays */
    /************************************************************/
    
    u_char *adc_locn = (u_char *)&(adcd_p->ADC[adcd_offset]);
    i = seqd_offset;
    int anode_seq = 0;
    while (i < numseq) {
      if (seqd_p->sequence[i] & 0x8000) { // index element
	anode = (seqd_p->sequence[i])& 0xff;
	INT32 seq_hybridID = (seqd_p->sequence[i]>>8)& 0x7f;

	if (anode == 255) break; // anode 255 is the last word as a filler
	if (seq_hybridID != hybridID) break; //start of the next hybrid

	anode_seq = 0;
      } // ...if (seqd_p->sequence[i] < 0
      else { // sequence descriptor element
	unsigned short work = seqd_p->sequence[i];
	int tb = (work>>6) & 0xff;
	int len = work & 0x1f;
	int last = work & 0x20;

	Anode_array[hybrid-1][anode-1].seq[anode_seq].startTimeBin = tb;
	Anode_array[hybrid-1][anode-1].seq[anode_seq].Length = len;
	Anode_array[hybrid-1][anode-1].seq[anode_seq].FirstAdc = adc_locn;
	adc_locn +=len;
	anode_seq++;

      
	if(last) { //last sequence ?
	  anode++;    // default to next pad in this padrow
	  anode_seq = 0;
	}
      }   
      i++;
    }

//     printf("pass 2 done\n");
  }

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
    if (detector->ercpy->verbose) {
      printf("SVTMZCLD for HS %d, Receiver %d, Mezz %d does not exist\n",
	     hyperSector, rcb, mezz);
    }
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

