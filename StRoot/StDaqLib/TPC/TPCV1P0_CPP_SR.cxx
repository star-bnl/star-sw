/***************************************************************************
 * $Id: TPCV1P0_CPP_SR.cxx,v 1.6 2007/12/24 06:04:31 fine Exp $
 * Author: Jeff Landgraf and M.J. LeVine
 ***************************************************************************
 * Description: 
 *      
 *
 *   change log
 * 06-May-99  MJL code cloned from Jeff's ADCR_SR
 * 03-Jun-99 MJL added return TRUE to TPCV1P0_CPP_SR::initialize()
 * 29-Aug-99 MJL #include <Stiostream.h> for HP platform
 *
 ***************************************************************************
 * $Log: TPCV1P0_CPP_SR.cxx,v $
 * Revision 1.6  2007/12/24 06:04:31  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.5  2003/09/02 17:55:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2000/01/04 20:55:04  levine
 * Implemented memory-mapped file access in EventReader.cxx. Old method
 * (via seeks) is still possible by setting mmapp=0 in
 *
 * 	getEventReader(fd,offset,(const char *)logfile,mmapp);
 *
 *
 * but memory-mapped access is much more effective.
 *
 * Revision 1.3  1999/09/02 21:47:10  fisyak
 * HP corrections
 *
 * Revision 1.2  1999/07/02 04:43:23  levine
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
#include "TPCV1P0.hh"
// TPC V1.0 Raw Reader

using namespace OLDEVP;

TPCV1P0_CPP_SR::TPCV1P0_CPP_SR(int s, TPCV1P0_Reader *det)
{
  //  cout << "Constructing TPCV1P0_CPP_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
}

int TPCV1P0_CPP_SR::initialize()
{
  // get a sector reader for PADK
  //  printf("TPCV1P0_CPP_SR::initialize() sector %d\n",sector);
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;
  //  printf("TPCV1P0_CPP_SR::initialize() SUCCESS sector %d\n",sector);

  // store pointers to the CPP banks
  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      banks[rcb][mz] = detector->getBankTPCCPPR(sector,rcb,mz);
    }
  }
  return TRUE;
}

TPCV1P0_CPP_SR::~TPCV1P0_CPP_SR()
{
  //  cout << "Deleting TPCV1P0_CPP_SR" << endl;
}

int TPCV1P0_CPP_SR::getClusters(int PadRow, int Pad, 
		  int *nClusters, struct ASIC_Cluster **clusters)
{
  // Return a pointer to the cluster array for this (padrow,pad)
  int i, mz, rb;
  PADK_entry ent;

  // check pad, padrow for validity
  if (PadRow==0 || PadRow>TPC_PADROWS) {
    printf(" %s %d padrow not a legal value: %d\n", __FILE__,__LINE__,PadRow);
    return -1;
  }
  padkr->get(PadRow, Pad, &ent);
  if(((mz=ent.mz) == 0) || ((rb=ent.rb) == 0)) {
    printf(" %s %d mz: %d  rb: %d\n", __FILE__,__LINE__,rb,mz);
    printf("      PadRow %d,  Pad %d\n", PadRow, Pad);
    return -1;
  }
  if (!(void *)banks[rb-1][mz-1]) {
    printf(" %s %d no CPP bank for rb: %d  mz: %d\n", __FILE__,__LINE__,rb,mz);
    printf("      PadRow %d,  Pad %d\n", PadRow, Pad);
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


int TPCV1P0_CPP_SR::MemUsed()
{
  return 0;
}



