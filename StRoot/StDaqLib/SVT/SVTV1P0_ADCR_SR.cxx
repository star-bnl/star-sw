/***************************************************************************
 *      
 * $Id: SVTV1P0_ADCR_SR.cxx,v 1.1 2000/06/06 18:08:31 jml Exp $
 *      
 * Author: Jeff Landgraf, M.J. LeVine and Marcelo Munhoz(for the SVT group)
 *      
 ***************************************************************************
 *      
 * Description: // SVT ADC Raw Reader
 *      
 ***************************************************************************
 *
 * $Log: SVTV1P0_ADCR_SR.cxx,v $
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines)
 *
 *
 **************************************************************************/
#include <iostream.h>


#include "StDaqLib/GENERIC/EventReader.hh"
#include "SVTV1P0.hh"

//==================== ADC Raw  Reader =============================


SVTV1P0_ADCR_SR::SVTV1P0_ADCR_SR(int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_ADCR_SR" << endl;
  barrel = 0;
  ladder = 0;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
}

SVTV1P0_ADCR_SR::SVTV1P0_ADCR_SR(int b, int l, int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_ADCR_SR" << endl;
  barrel = b;
  ladder = l;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
}

int SVTV1P0_ADCR_SR::initialize()
{
  // get a sector reader for ANODK
  anodkr = detector->getANODKReader();
  if (!anodkr) return FALSE;

  int hypersector, rcb, mz;

  ANODK_entry ent;
  // store pointers to the ADCR banks
  for (int hybrid = 1; hybrid <= SVT_HYBRIDS; hybrid++)
    {
      if (barrel)
	anodkr->get(barrel, ladder, wafer, hybrid, &ent);
      else
	anodkr->get(wafer, hybrid, &ent);

      hypersector = ent.hypersector;
      rcb = ent.rb;
      mz = ent.mz;

      //printf("SVTV1P0_ADCR_SR::wafer = %d, hybrid = %d, hypersector = %d, rcb = %d, mz = %d\n",wafer,hybrid,hypersector,rcb,mz);

      if ((hypersector) && (rcb) && (mz))
	banks[hybrid-1] = detector->getBankSVTADCR(hypersector-1,rcb-1,mz-1);
      else
	return FALSE;      
    }

  return TRUE;
}

SVTV1P0_ADCR_SR::~SVTV1P0_ADCR_SR()
{
  //  cout << "Deleting SVTV1P0_ADCR_SR" << endl;
}

int SVTV1P0_ADCR_SR::getPadList(int hybrid, u_char **anodeList)
{
  // Construct the anodelist array for this PadRow
  int i;
  ANODK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=SVT_ANODES; i++)
  {
    if (barrel)
      anodkr->get(barrel, ladder, wafer, hybrid, &ent);
    else
      anodkr->get(wafer, hybrid, &ent);

    if((ent.mz == 0) || (ent.rb == 0) || (ent.hypersector == 0)) continue;
    anodelist[hybrid-1][j++] = i;
  }
          // confusing syntax but correct
  *anodeList = &anodelist[hybrid-1][0];
  return j;
}

int SVTV1P0_ADCR_SR::getSequences(int hybrid, int Anode, int *nArray, 
				  u_char **Array)
{
  ANODK_entry ent;
  if (barrel)
    anodkr->get(barrel, ladder, wafer, hybrid, &ent);
  else
    anodkr->get(wafer, hybrid, &ent);

  if((ent.mz == 0) || (ent.rb == 0) || (ent.hypersector == 0)) 
  {
    *nArray = 0;
    *Array = NULL;
    spERROR(ERR_BANK);
    return -1;
  }
  
  int offset = (ent.offset * anodkr->getADCBytes() * SVT_MZANODES) + (anodkr->getADCBytes() * (Anode-1));
  *nArray = anodkr->getADCBytes();

  //  printf("Offset = %d\n",offset);
  // printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);

  if (banks[hybrid-1] != NULL) 
    {
      *Array = (((u_char *)banks[hybrid-1]->ADC) + offset);
      //      cout << "SVTV1P0_ADCR_SR::Array = " << *Array << endl;
      return 1;
    }

  return 0;
}

int SVTV1P0_ADCR_SR::MemUsed()
{
  return 0;
}

//==================== Pedestal Reader ===========================

SVTV1P0_PEDR_SR::SVTV1P0_PEDR_SR(int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_PEDR_SR" << endl;
  barrel = 0;
  ladder = 0;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
  numEvents = 0;
}

SVTV1P0_PEDR_SR::SVTV1P0_PEDR_SR(int b, int l, int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_PEDR_SR" << endl;
  barrel = b;
  ladder = l;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
  numEvents = 0;
}

int SVTV1P0_PEDR_SR::initialize()
{
  // get a sector reader for ANODK
  anodkr = detector->getANODKReader();
  if (!anodkr) return FALSE;

  int hypersector, rcb, mz;

  ANODK_entry ent;
  // store pointers to the ADCR banks
  for (int hybrid = 1; hybrid <= SVT_HYBRIDS; hybrid++)
    {
      if (barrel)
	anodkr->get(barrel, ladder, wafer, hybrid, &ent);
      else
	anodkr->get(wafer, hybrid, &ent);

      hypersector = ent.hypersector;
      rcb = ent.rb;
      mz = ent.mz;

      banks[hybrid-1] = detector->getBankSVTPEDR(hypersector-1,rcb-1,mz-1);    

      if (banks[hybrid-1] !=NULL) 
	numEvents = banks[hybrid-1]->NumEvents;
    }

  return TRUE;
}

SVTV1P0_PEDR_SR::~SVTV1P0_PEDR_SR()
{
  //  cout << "Deleting SVTV1P0_PEDR_SR" << endl;
}

int SVTV1P0_PEDR_SR::getPadList(int hybrid, u_char **anodeList)
{
  // Construct the anodelist array for this PadRow
  int i;
  ANODK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=SVT_ANODES; i++)
  {
    if (barrel)
      anodkr->get(barrel, ladder, wafer, hybrid, &ent);
    else
      anodkr->get(wafer, hybrid, &ent);

    if((ent.mz == 0) || (ent.rb == 0) || (ent.hypersector == 0)) continue;
    anodelist[hybrid-1][j++] = i;
  }
          // confusing syntax but correct
  *anodeList = &anodelist[hybrid-1][0];
  return j;
}

int SVTV1P0_PEDR_SR::getSequences(int hybrid, int Anode, int *nArray, 
				  u_char **Array)
{
  ANODK_entry ent;
  if (barrel)
    anodkr->get(barrel, ladder, wafer, hybrid, &ent);
  else
    anodkr->get(wafer, hybrid, &ent);

  if((ent.mz == 0) || (ent.rb == 0) || (ent.hypersector == 0)) 
  {
    *nArray = 0;
    *Array = NULL;
    spERROR(ERR_BANK);
    return -1;
  }
  
  int offset = (ent.offset * anodkr->getPEDBytes() * SVT_MZANODES) + (anodkr->getPEDBytes() * (Anode-1));
  *nArray = anodkr->getPEDBytes();

  if (banks[hybrid-1] != NULL)
    {
      //      printf("Offset = %d\n",offset);
      //      printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)banks[hybrid-1]->pedestal) + offset);
      return 1;
    }
  return 0;
}

int SVTV1P0_PEDR_SR::getNumberOfEvents()
{
  return numEvents;
}

int SVTV1P0_PEDR_SR::MemUsed()
{
  return 0;
}


//==================== RMS  Reader =============================

SVTV1P0_PRMS_SR::SVTV1P0_PRMS_SR(int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_PRMS_SR" << endl;
  barrel = 0;
  ladder = 0;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
  numEvents = 0;

}

SVTV1P0_PRMS_SR::SVTV1P0_PRMS_SR(int b, int l, int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_PRMS_SR" << endl;
  barrel = b;
  ladder = l;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  memset((char *)banks, 0, sizeof(banks));
  numEvents = 0;
}

int SVTV1P0_PRMS_SR::initialize()
{
  // get a sector reader for ANODK
  anodkr = detector->getANODKReader();
  if (!anodkr) return FALSE;

  int hypersector, rcb, mz;

  ANODK_entry ent;
  // store pointers to the ADCR banks
  for (int hybrid = 1; hybrid <= SVT_HYBRIDS; hybrid++)
    {
      if (barrel)
	anodkr->get(barrel, ladder, wafer, hybrid, &ent);
      else
	anodkr->get(wafer, hybrid, &ent);

      hypersector = ent.hypersector;
      rcb = ent.rb;
      mz = ent.mz;

      banks[hybrid-1] = detector->getBankSVTRMSR(hypersector-1,rcb-1,mz-1);    

      if (banks[hybrid-1] !=NULL) 
	numEvents = banks[hybrid-1]->NumEvents;
    }

  return TRUE;
}

SVTV1P0_PRMS_SR::~SVTV1P0_PRMS_SR()
{
  //  cout << "Deleting SVTV1P0_PRMS_SR" << endl;
}

int SVTV1P0_PRMS_SR::getPadList(int hybrid, u_char **anodeList)
{
  // Construct the anodelist array for this PadRow
  int i;
  ANODK_entry ent;

  // Fill in padrows
  int j=0;
  for(i=1; i<=SVT_ANODES; i++)
  {
    if (barrel)
      anodkr->get(barrel, ladder, wafer, hybrid, &ent);
    else
      anodkr->get(wafer, hybrid, &ent);

    if((ent.mz == 0) || (ent.rb == 0) || (ent.hypersector == 0)) continue;
    anodelist[hybrid-1][j++] = i;
  }
          // confusing syntax but correct
  *anodeList = &anodelist[hybrid-1][0];
  return j;
}

int SVTV1P0_PRMS_SR::getSequences(int hybrid, int Anode, int *nArray, 
				  u_char **Array)
{
  ANODK_entry ent;
  if (barrel)
    anodkr->get(barrel, ladder, wafer, hybrid, &ent);
  else
    anodkr->get(wafer, hybrid, &ent);

  if((ent.mz == 0) || (ent.rb == 0) || (ent.hypersector == 0)) 
  {
    *nArray = 0;
    *Array = NULL;
    spERROR(ERR_BANK);
    return -1;
  }
  
  int offset = (ent.offset * anodkr->getRMSBytes() * SVT_MZANODES) + (anodkr->getRMSBytes() * (Anode-1));
  *nArray = anodkr->getRMSBytes();

  if (banks[hybrid-1] != NULL)
    {
//       printf("Offset = %d\n",offset);
//       printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)banks[hybrid-1]->pedRMSt16) + offset);
      return 1;
    }
  return 0;
}

int SVTV1P0_PRMS_SR::getNumberOfEvents()
{
  return numEvents;
}

int SVTV1P0_PRMS_SR::MemUsed()
{
  return 0;
}


