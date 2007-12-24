/***************************************************************************
 * $Id: TPCV2P0_CPP_SR.cxx,v 1.8 2007/12/24 06:04:32 fine Exp $
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: TPC reader for raw cluster pointer banks (TPCCPPR)
 *      
 *
 *   change log
 * 06-May-99 MJL code cloned from Jeff's ADCR_SR
 * 22-Jun-99 MJL added TPCV2P0_CPP_SR::getAsicParams(struct ASIC_params *);
 * 29-Aug-99 MJL #include <Stiostream.h> for HP platform
 * 03-Feb-00 MJL print ASCI params so PT Barnum's clientele can grock them
 *
 ***************************************************************************
 * $Log: TPCV2P0_CPP_SR.cxx,v $
 * Revision 1.8  2007/12/24 06:04:32  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.7  2003/09/02 17:55:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.6  2000/02/03 21:18:36  levine
 * change printout of ASIC parameters to idiot-proof mode
 *
 * Revision 1.5  2000/01/04 20:55:04  levine
 * Implemented memory-mapped file access in EventReader.cxx. Old method
 * (via seeks) is still possible by setting mmapp=0 in
 *
 * 	getEventReader(fd,offset,(const char *)logfile,mmapp);
 *
 *
 * but memory-mapped access is much more effective.
 *
 * Revision 1.4  1999/09/02 21:47:12  fisyak
 * HP corrections
 *
 * Revision 1.3  1999/07/02 04:43:24  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/

#include <Stiostream.h>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV2P0.hh"
// TPC V1.0 Raw Reader
// change log
// 03-Jun-99 MJL added return TRUE to TPCV2P0_CPP_SR::initialize()
// 21-Jun-99 MJL test for existence of CPP bank before printing ASIC params (line 38)
// 23-Jun-99 MJL most output now supressed with EventReader.verbose

using namespace OLDEVP;

TPCV2P0_CPP_SR::TPCV2P0_CPP_SR(int s, TPCV2P0_Reader *det)
{
  //  cout << "Constructing TPCV2P0_CPP_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
}

int TPCV2P0_CPP_SR::initialize()
{
  // get a sector reader for PADK
  //  printf("TPCV2P0_CPP_SR::initialize() sector %d\n",sector);
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;
  //  printf("TPCV2P0_CPP_SR::initialize() SUCCESS sector %d\n",sector);

  // store pointers to the CPP banks
  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      banks[rcb][mz] = detector->getBankTPCCPPR(sector,rcb,mz);
      classname(Bank_TPCCPPR) *cpp = banks[rcb][mz];
      if (cpp)
      if (detector->ercpy->verbose) 
	printf("ASIC params sec%d RB%d MZ %d:\
\nLow threshold: >%d ADC counts in  >%d consecutive bins\
\nHigh threshold: >%d ADC counts in >%d of these bins\n",
	       sector+1, rcb+1, mz+1, 
	     cpp->asic_params.thresh_lo,
	     cpp->asic_params.n_seq_lo,
	     cpp->asic_params.thresh_hi,
	     cpp->asic_params.n_seq_hi);
    }
  }
  return TRUE;
}

int TPCV2P0_CPP_SR::getAsicParams(ASIC_params *params)
{
  for(int rcb = 0; rcb < 6; rcb++) {
    for(int mz = 0; mz < 3; mz++)   {
      classname(Bank_TPCCPPR) *cpp = banks[rcb][mz];
      if (cpp) {
	*params = cpp->asic_params;
	return TRUE;
      }
    }
  }
  return FALSE;
}

TPCV2P0_CPP_SR::~TPCV2P0_CPP_SR()
{
  //  cout << "Deleting TPCV2P0_CPP_SR" << endl;
}

int TPCV2P0_CPP_SR::getClusters(int PadRow, int Pad, 
		  int *nClusters, struct ASIC_Cluster **clusters)
{
  // Return a pointer to the cluster array for this (padrow,pad)
  int i, mz, rb;
  PADK_entry ent;

  // check pad, padrow for validity
  if (PadRow==0 || PadRow>TPC_PADROWS) {
    if (detector->ercpy->verbose) 
      printf(" %s %d padrow not a legal value: %d\n", __FILE__,__LINE__,PadRow);
    return -1;
  }
  padkr->get(PadRow, Pad, &ent);
  if(((mz=ent.mz) == 0) || ((rb=ent.rb) == 0)) {
    if (detector->ercpy->verbose) 
      printf(" %s %d mz: %d  rb: %d\n", __FILE__,__LINE__,rb,mz);
    if (detector->ercpy->verbose) 
      printf("      PadRow %d,  Pad %d\n", PadRow, Pad);
    return -1;
  }
  if (!(void *)banks[rb-1][mz-1]) {
    if (detector->ercpy->verbose) 
      printf(" %s %d no CPP bank for rb: %d  mz: %d\n", __FILE__,__LINE__,rb,mz);
    if (detector->ercpy->verbose) printf("      PadRow %d,  Pad %d\n", PadRow, Pad);
    return -1; 
           //test for CPPR bank for this RB, Mezz
  }

  clusters[Pad-1] = (ASIC_Cluster *)(banks[rb-1][mz-1]->entry + 32*ent.offset);
  for (i=0; i<31; i++) { //loop over ASIC sequences
    if (clusters[Pad-1][i].start_time_bin < 0) break;
  }

  *nClusters = i;  //set number of valid clusters in array
  return 0;
}


int TPCV2P0_CPP_SR::MemUsed()
{
  return 0;
}



