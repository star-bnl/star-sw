//  MJL 5/6/99  code cloned from Jeff's ADCR_SR
//

#include <iostream>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPCV1P0.hh"
// TPC V1.0 Raw Reader
// change log
// 03-Jun-99 MJL added return TRUE to TPCV1P0_CPP_SR::initialize()

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



