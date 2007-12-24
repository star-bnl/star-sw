/***************************************************************************
 * $Id: FTPV1P0_ADCR_SR.cxx,v 1.4 2007/12/24 06:04:13 fine Exp $
 * Author: Jeff Landgraf, J.Klay, H.Huemmler
 ***************************************************************************
 * Description: FTP (v1.0) raw ADC reader 
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0_ADCR_SR.cxx,v $
 * Revision 1.4  2007/12/24 06:04:13  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.3  2003/09/02 17:55:31  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2001/06/19 20:51:21  jeromel
 * Commited for Janet S.
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

// FTP V1.0 ADC Raw Reader
using namespace OLDEVP;

//==================== ADC Raw  Reader =============================

FTPV1P0_ADCR_SR::FTPV1P0_ADCR_SR(int s, FTPV1P0_Reader *det)
{
  //  cout << "Constructing FTPV1P0_ADCR_SR for sector "<<s << endl;
  sector = s;
  detector = det;

  // NULLS in banks array
  bank = 0;
//  memset((char *)bank, 0, sizeof(bank));
}

int FTPV1P0_ADCR_SR::initialize()
{
  // get a sector reader for PADK
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;

  // store pointer to the ADCR bank
  bank = detector->getBankFTPADCR(sector);
  if(!bank) { return FALSE; 
  } else return TRUE;
}

FTPV1P0_ADCR_SR::~FTPV1P0_ADCR_SR()
{
  //  cout << "Deleting FTPV1P0_ADCR_SR" << endl;
}

int FTPV1P0_ADCR_SR::getPadList(int PadRow, u_char **padList)
{
  // Construct the padlist array for this PadRow
  int i;
  FTPPADK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=FTP_MAXPADS; i++)
  {
    padkr->get(PadRow, i, &ent);
    padlist[PadRow-1][j++] = i;
  }
          // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  return j;
}

int FTPV1P0_ADCR_SR::getSequences(int PadRow, int Pad, int *nArray, 
				  u_char **Array)
{
  FTPPADK_entry ent;
  padkr->get(PadRow, Pad, &ent);
  
  int offset = ent.offset * padkr->getADCBytes();
  *nArray = padkr->getADCBytes();

  //  printf("Offset = %d\n",offset);
  if (bank != NULL) 
    {
      *Array = (((u_char *)bank->ADC) + offset);
      return 1;
    }
  return 0;
}

int FTPV1P0_ADCR_SR::MemUsed()
{
  return 0;
}

//==================== Pedestal Reader ===========================

FTPV1P0_PEDR_SR::FTPV1P0_PEDR_SR(int s, FTPV1P0_Reader *det)
{
  //  cout << "Constructing FTPV1P0_PEDR_SR" << endl;
  sector = s;
  detector = det;

  // NULLS in banks array
//  memset((char *)bank, 0, sizeof(bank));
  bank = 0;
  numEvents = 0;
}

int FTPV1P0_PEDR_SR::initialize()
{
  // get a sector reader for PADK
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;

  bank = detector->getBankFTPPEDR(sector);
  if (bank !=NULL) 
  {
    numEvents = bank->NumEvents;
  }
  return TRUE;
}

FTPV1P0_PEDR_SR::~FTPV1P0_PEDR_SR()
{
  //  cout << "Deleting FTPV1P0_PEDR_SR" << endl;
}

int FTPV1P0_PEDR_SR::getPadList(int PadRow, u_char **padList)
{
  // Construct the padlist array for this PadRow
  int i;
  FTPPADK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=FTP_MAXPADS; i++)
  {
    padkr->get(PadRow, i, &ent);
    padlist[PadRow-1][j++] = i;
  }
          // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  return j;
}

int FTPV1P0_PEDR_SR::getSequences(int PadRow, int Pad, int *nArray, 
				  u_char **Array)
{
  FTPPADK_entry ent;
  padkr->get(PadRow, Pad, &ent);
  int offset = ent.offset * padkr->getPEDBytes();
  *nArray = padkr->getPEDBytes();

  if (bank != NULL)
    {
      //      printf("Offset = %d\n",offset);
      //      printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)bank->pedestal) + offset);
      return 1;
    }
  return 0;
}

int FTPV1P0_PEDR_SR::getNumberOfEvents()
{
  return numEvents;
}

int FTPV1P0_PEDR_SR::MemUsed()
{
  return 0;
}


//==================== RMS  Reader =============================

FTPV1P0_PRMS_SR::FTPV1P0_PRMS_SR(int s, FTPV1P0_Reader *det)
{
  //  cout << "Constructing FTPV1P0_PRMS_SR" << endl;
  sector = s;
  detector = det;

  // NULLS in banks array
//  memset((char *)bank, 0, sizeof(bank));
  bank = 0;
  numEvents = 0;

}

int FTPV1P0_PRMS_SR::initialize()
{
  // get a sector reader for PADK
  padkr = detector->getPADKReader(sector);
  if (!padkr) return FALSE;

  // store pointers to the PRMS banks
  bank = detector->getBankFTPRMSR(sector);
  if (bank !=NULL) 
  {	
     numEvents = bank->NumEvents;
  }
  return TRUE;
}

FTPV1P0_PRMS_SR::~FTPV1P0_PRMS_SR()
{
  //  cout << "Deleting FTPV1P0_PRMS_SR" << endl;
}

int FTPV1P0_PRMS_SR::getPadList(int PadRow, u_char **padList)
{
  // Construct the padlist array for this PadRow
  int i;
  FTPPADK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=FTP_MAXPADS; i++)
  {
    padkr->get(PadRow, i, &ent);
    padlist[PadRow-1][j++] = i;
  }
          // confusing syntax but correct
  *padList = &padlist[PadRow-1][0];
  return j;
}

int FTPV1P0_PRMS_SR::getSequences(int PadRow, int Pad, int *nArray, 
				  u_char **Array)
{
  FTPPADK_entry ent;
  padkr->get(PadRow, Pad, &ent);
  int offset = ent.offset * padkr->getRMSBytes();
  *nArray = padkr->getRMSBytes();

  if (bank != NULL)
    {
//       printf("Offset = %d\n",offset);
//       printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)bank->pedRMSt16) + offset);
      return 1;
    }
  return 0;
}

int FTPV1P0_PRMS_SR::getNumberOfEvents()
{
  return numEvents;
}

int FTPV1P0_PRMS_SR::MemUsed()
{
  return 0;
}


