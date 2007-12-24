/***************************************************************************
 * $Id: FTPV1P0_CPP_SR.cxx,v 1.5 2007/12/24 06:04:13 fine Exp $
 * Author: Jeff Landgraf, M.J. LeVine, J.Klay, H.Huemmler
 ***************************************************************************
 * Description: 
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0_CPP_SR.cxx,v $
 * Revision 1.5  2007/12/24 06:04:13  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.4  2003/09/02 17:55:31  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2001/06/27 22:06:54  jcs
 * Remove unused variables mz, rb
 *
 * Revision 1.2  2001/06/25 22:58:54  jcs
 * cleanup code
 *
 * Revision 1.1  2000/01/18 18:01:19  levine
 * Hummler's implementaiton of FTPC reader. Note that method
 *
 * FTPV1P0_ZS_SR::getFeeSequences(int Fee, int Pin, int *nSeq,
 * 				   Sequence **SeqData)
 *
 * causes exit() since the required #include file has not yet been
 * (correctly) implemented.
 *
 *
 **************************************************************************/

#include <Stiostream.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "FTPV1P0.hh"
// FTP V1.0 Raw Reader
using namespace OLDEVP;

FTPV1P0_CPP_SR::FTPV1P0_CPP_SR(int s, FTPV1P0_Reader *det)
{
  //  cout << "Constructing FTPV1P0_CPP_SR" << endl;
  sector = s;
  detector = det;

  // NULLS in banks array
  bank = 0;
//  memset((char *)bank, 0, sizeof(bank));
}

int FTPV1P0_CPP_SR::initialize()
{
  // get a sector reader for PADK
  //  printf("FTPV1P0_CPP_SR::initialize() sector %d\n",sector);
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;
  //  printf("FTPV1P0_CPP_SR::initialize() SUCCESS sector %d\n",sector);

  // store pointers to the CPP banks
  bank = detector->getBankFTPCPPR(sector);
  classname(Bank_FTPCPPR) *cpp = bank;
  if (cpp && detector->ercpy->verbose)
  {    printf("ASIC params sec %d: TH_LO %d, TH_HI %d, NSEQ_LO %d, NSEQ_HI %d\n",sector, 
	     cpp->asic_params.thresh_lo,
	     cpp->asic_params.thresh_hi,
	     cpp->asic_params.n_seq_lo,
	     cpp->asic_params.n_seq_hi);
  }
  return TRUE;
}

FTPV1P0_CPP_SR::~FTPV1P0_CPP_SR()
{
  //  cout << "Deleting FTPV1P0_CPP_SR" << endl;
}

int FTPV1P0_CPP_SR::getClusters(int PadRow, int Pad, 
		  int *nClusters, struct ASIC_Cluster **clusters)
{
  // Return a pointer to the cluster array for this (padrow,pad)
  int i;
  FTPPADK_entry ent;

  // check pad, padrow for validity
  if (PadRow==0 || PadRow>FTP_PADROWS) {
    printf(" %s %d padrow not a legal value: %d\n", __FILE__,__LINE__,PadRow);
    return -1;
  }
  padkr->get(PadRow, Pad, &ent);
  if (!(void *)bank) {
    printf(" %s %d no CPP bank for sector %d\n",__FILE__,__LINE__,sector);
    printf("      PadRow %d,  Pad %d\n", PadRow, Pad);
    return -1; 
           //test for CPPR bank for this sector
  }

  clusters[Pad-1] = (ASIC_Cluster *)(bank->entry + 32*ent.offset);
  for (i=0; i<31; i++) { //loop over ASIC sequences
    if (clusters[Pad-1][i].start_time_bin < 0) break;
  }

  *nClusters = i;  //set number of valid clusters in array
  return 0;
}


int FTPV1P0_CPP_SR::MemUsed()
{
  return 0;
}



