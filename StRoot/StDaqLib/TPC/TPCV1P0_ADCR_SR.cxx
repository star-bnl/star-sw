/***************************************************************************
 * $Id: TPCV1P0_ADCR_SR.cxx,v 1.6 2007/12/24 06:04:31 fine Exp $
 * Author: Jeff Landgraf
 ***************************************************************************
 * Description: TPC (v1.0) raw ADC reader 
 *      
 *
 *   change log
 * 03-Jun-99 MJL added return TRUE to TPCV1P0_ADCR_SR::initialize()
 * 03-Jun-99 MJL added return TRUE to TPCV1P0_PRMS_SR::initialize()
 * 03-Jun-99 MJL added return TRUE to TPCV1P0_PEDR_SR::initialize()
 * 29-Aug-99 MJL #include <Stiostream.h> for HP platform
 *
 ***************************************************************************
 * $Log: TPCV1P0_ADCR_SR.cxx,v $
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

//==================== ADC Raw  Reader =============================

using namespace OLDEVP;

TPCV1P0_ADCR_SR::TPCV1P0_ADCR_SR(int s, TPCV1P0_Reader *det)
{
  //  cout << "Constructing TPCV1P0_ADCR_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
}

int TPCV1P0_ADCR_SR::initialize()
{
  // get a sector reader for PADK
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;

  // store pointers to the ADCR banks
  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      banks[rcb][mz] = detector->getBankTPCADCR(sector,rcb,mz);
    }
  }
  return TRUE;
}

TPCV1P0_ADCR_SR::~TPCV1P0_ADCR_SR()
{
  //  cout << "Deleting TPCV1P0_ADCR_SR" << endl;
}

int TPCV1P0_ADCR_SR::getPadList(int PadRow, u_char **padList)
{
  // Construct the padlist array for this PadRow
  int i;
  PADK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=TPC_MAXPADS; i++)
  {
    padkr->get(PadRow, i, &ent);
    if((ent.mz == 0) || (ent.rb == 0)) continue;
    padlist[PadRow-1][j++] = i;
  }
          // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  return j;
}

int TPCV1P0_ADCR_SR::getSequences(int PadRow, int Pad, int *nArray, 
				  u_char **Array)
{
  PADK_entry ent;
  padkr->get(PadRow, Pad, &ent);
  if((ent.mz == 0) || (ent.rb == 0)) 
  {
    *nArray = 0;
    *Array = NULL;
    spERROR(ERR_BANK);
    return -1;
  }
  
  int offset = ent.offset * padkr->getADCBytes();
  *nArray = padkr->getADCBytes();

  //  printf("Offset = %d\n",offset);
  // printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
  if (banks[ent.rb-1][ent.mz-1] != NULL) 
    {
      *Array = (((u_char *)banks[ent.rb-1][ent.mz-1]->ADC) + offset);
      return 1;
    }
  return 0;
}

int TPCV1P0_ADCR_SR::MemUsed()
{
  return 0;
}

//==================== Pedestal Reader ===========================

TPCV1P0_PEDR_SR::TPCV1P0_PEDR_SR(int s, TPCV1P0_Reader *det)
{
  //  cout << "Constructing TPCV1P0_PEDR_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
  numEvents = 0;
}

int TPCV1P0_PEDR_SR::initialize()
{
  // get a sector reader for PADK
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;

  // store pointers to the PEDR banks
  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      banks[rcb][mz] = detector->getBankTPCPEDR(sector,rcb,mz);
      if (banks[rcb][mz] !=NULL) 
	numEvents = banks[rcb][mz]->NumEvents;
    }
  }
  return TRUE;
}

TPCV1P0_PEDR_SR::~TPCV1P0_PEDR_SR()
{
  //  cout << "Deleting TPCV1P0_PEDR_SR" << endl;
}

int TPCV1P0_PEDR_SR::getPadList(int PadRow, u_char **padList)
{
  // Construct the padlist array for this PadRow
  int i;
  PADK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=TPC_MAXPADS; i++)
  {
    padkr->get(PadRow, i, &ent);
    if((ent.mz == 0) || (ent.rb == 0)) continue;
    padlist[PadRow-1][j++] = i;
  }
          // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  return j;
}

int TPCV1P0_PEDR_SR::getSequences(int PadRow, int Pad, int *nArray, 
				  u_char **Array)
{
  PADK_entry ent;
  padkr->get(PadRow, Pad, &ent);
  if((ent.mz == 0) || (ent.rb == 0)) 
  {
    *nArray = 0;
    *Array = NULL;
    spERROR(ERR_BANK);
    return -1;
  }
  
  int offset = ent.offset * padkr->getPEDBytes();
  *nArray = padkr->getPEDBytes();

  if (banks[ent.rb-1][ent.mz-1] != NULL)
    {
      //      printf("Offset = %d\n",offset);
      //      printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)banks[ent.rb-1][ent.mz-1]->pedestal) + offset);
      return 1;
    }
  return 0;
}

int TPCV1P0_PEDR_SR::getNumberOfEvents()
{
  return numEvents;
}

int TPCV1P0_PEDR_SR::MemUsed()
{
  return 0;
}


//==================== RMS  Reader =============================

TPCV1P0_PRMS_SR::TPCV1P0_PRMS_SR(int s, TPCV1P0_Reader *det)
{
  //  cout << "Constructing TPCV1P0_PRMS_SR" << endl;
  sector = s-1; // convert the sector into internal representation
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
  numEvents = 0;

}

int TPCV1P0_PRMS_SR::initialize()
{
  // get a sector reader for PADK
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;

  // store pointers to the PRMS banks
  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      banks[rcb][mz] = detector->getBankTPCRMSR(sector,rcb,mz);
      if (banks[rcb][mz] !=NULL) 
	numEvents = banks[rcb][mz]->NumEvents;
    }
  }
  return TRUE;
}

TPCV1P0_PRMS_SR::~TPCV1P0_PRMS_SR()
{
  //  cout << "Deleting TPCV1P0_PRMS_SR" << endl;
}

int TPCV1P0_PRMS_SR::getPadList(int PadRow, u_char **padList)
{
  // Construct the padlist array for this PadRow
  int i;
  PADK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=TPC_MAXPADS; i++)
  {
    padkr->get(PadRow, i, &ent);
    if((ent.mz == 0) || (ent.rb == 0)) continue;
    padlist[PadRow-1][j++] = i;
  }
          // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  return j;
}

int TPCV1P0_PRMS_SR::getSequences(int PadRow, int Pad, int *nArray, 
				  u_char **Array)
{
  PADK_entry ent;
  padkr->get(PadRow, Pad, &ent);
  if((ent.mz == 0) || (ent.rb == 0)) 
  {
    *nArray = 0;
    *Array = NULL;
    spERROR(ERR_BANK);
    return -1;
  }
  
  int offset = ent.offset * padkr->getRMSBytes();
  *nArray = padkr->getRMSBytes();

  if (banks[ent.rb-1][ent.mz-1] != NULL)
    {
//       printf("Offset = %d\n",offset);
//       printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)banks[ent.rb-1][ent.mz-1]->pedRMSt16) + offset);
      return 1;
    }
  return 0;
}

int TPCV1P0_PRMS_SR::getNumberOfEvents()
{
  return numEvents;
}

int TPCV1P0_PRMS_SR::MemUsed()
{
  return 0;
}


