/***************************************************************************
 *
 * $Id: L3.Banks.cxx,v 1.4 2000/07/26 02:37:41 struck Exp $
 *
 * Author: Christof Struck, struck@star.physics.yale.edu
 ***************************************************************************
 *
 * Description: L3 specific swap functions
 *
 *
 *
 * change log:
 *   06 Jun 00 CS initial version
 *   24 Jul 00 CS added i960 cluster banks
 *
 ***************************************************************************
 *
 * $Log: L3.Banks.cxx,v $
 * Revision 1.4  2000/07/26 02:37:41  struck
 * minor changes
 *
 * Revision 1.3  2000/07/26 02:12:27  struck
 * added i960 cluster reader
 *
 * Revision 1.2  2000/07/06 18:16:00  ward
 * Install L3 code from Christof Struck.
 *
 *
 **************************************************************************/
#include "L3.Banks.hh"
#include <assert.h>


int Bank_L3_GTD::swap()
{
  // get bank length 
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  // returns zero means none needed
  if(!iret) return(0);  
  // returns negative means error
  if(iret < 0) return iret;

  // swap body first
  iret = swap_raw(header.ByteOrder, (INT32 *)&nHits, 5);
  assert(iret > 0);

  // now swap globalTrack structure
  for ( unsigned int i=0; i<nTracks; i++) {
        iret = swap_raw(header.ByteOrder, (int *)&track[i].id, 1);
	iret = l3Swap_short((short *)&track[i].flag, (short) 1);
	iret = swap_raw(header.ByteOrder, (int *)&track[i].chi2, 13);
  }

  // swap the header
  iret = header.swap();

  return iret;
}


int Bank_L3_LTD::swap()
{
  // get bank length 
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  // returns zero means none needed
  if(!iret) return(0);  
  // returns negative means error
  if(iret < 0) return iret;

  int nTracks = (int) (words * 4 - sizeof(Bank_Header)) / sizeof(localTrack);

  // now swap localTrack structure
  for (int i=0; i<nTracks; i++) {
        iret = l3Swap_short((short *)&track[i].id, 1);
	iret = l3Swap_short((short *)&track[i].innerMostRow, (short) 4);
	iret = swap_raw(header.ByteOrder, (INT32 *)&track[i].dedx, 8);
	iret = l3Swap_short((short *)&track[i].dpt, 4);
  }

  // swap the header
  iret = header.swap();

  return iret;
}


int Bank_L3_SECCD::swap()
{
  // get bank length 
  int words = header.BankLength;
  int iret = swap_raw(header.ByteOrder,&words,1);

  // returns zero means none needed
  if(!iret) return(0);  
  // returns negative means error
  if(iret < 0) return iret;

  // swap body first
  iret = swap_raw(header.ByteOrder, (INT32 *)&nrClusters_in_sector, 1);
  assert(iret > 0);

  // now swap l3_cluster structure
  for (unsigned int i=0; i<nrClusters_in_sector; i++) {
	iret = l3Swap_short((short *)&cluster[i].pad, (short) 5);
  }

  // swap the header
  iret = header.swap();

  return iret;
}


// taken from TPCV2PO.Banks.cxx
int Bank_TPCMZCLD::swap()
{
  int iret = swap_raw(header.ByteOrder, &numberOfRows, 1);
  if(iret <= 0) return iret;

  int nsp = 0;
  int *word = &numberOfRows;
  word++;                //bump to first instance of PadRow
  for (int i=0; i<numberOfRows; i++) {
        swap_raw(header.ByteOrder, word, 2);
	nsp = word[1];
	word += 2;       //point to first cluster
	swap_short(header.ByteOrder, word, 2*nsp);
	word += 2 * nsp; //bump past clusters
  }

  return header.swap();
}


// our own swap, which swaps shorts (and not words of shorts)
int l3Swap_short (short* data, short size)
{
  char *curr = (char *)data;
  for (int i=0; i<size; i++) {
        char temp = curr[0];
	curr[0] = curr[1];
	curr[1] = temp;
	curr += 2;
  }
  return 1;
};
