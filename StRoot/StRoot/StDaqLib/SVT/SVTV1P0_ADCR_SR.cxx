/***************************************************************************
 *      
 * $Id: SVTV1P0_ADCR_SR.cxx,v 1.5 2007/12/24 06:04:27 fine Exp $
 *      
 * Author: Jeff Landgraf, M.J. LeVine, Marcelo Munhoz, J. Schambach
 *      
 ***************************************************************************
 *      
 * Description: // SVT ADC Raw Reader
 *      
 ***************************************************************************
 *
 * $Log: SVTV1P0_ADCR_SR.cxx,v $
 * Revision 1.5  2007/12/24 06:04:27  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.4  2003/09/02 17:55:33  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2001/05/10 15:19:08  jschamba
 * unscrambling of Format = 2 data was done each time initialize was called. Rewrite header
 * to indicate Format = 0 after unscrambling prevents this.
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

//==================== ADC Raw  Reader =============================

using namespace OLDEVP;

SVTV1P0_ADCR_SR::SVTV1P0_ADCR_SR(int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_ADCR_SR" << endl;
  barrel = 0;
  ladder = 0;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  banks = (classname(Bank_SVTADCR) *)NULL;
}

SVTV1P0_ADCR_SR::SVTV1P0_ADCR_SR(int b, int l, int w, SVTV1P0_Reader *det)
{
  //  cout << "Constructing SVTV1P0_ADCR_SR" << endl;
  barrel = b;
  ladder = l;
  wafer = w; 
  detector = det;

  // NULLS in banks array
  banks = (classname(Bank_SVTADCR) *)NULL;
}

int SVTV1P0_ADCR_SR::initialize()
{
  // get a sector reader for ANODK
  anodkr = detector->getANODKReader();
  if (!anodkr) return FALSE;

  int hypersector, rcb, mz;

  ANODK_entry ent;
  // store pointers to the ADCR banks
  int hybrid = 1; // both hybrids are in same bank
  if (barrel)
    anodkr->get(barrel, ladder, wafer, hybrid, &ent);
  else
    anodkr->get(wafer, hybrid, &ent);
  
  hypersector = ent.hypersector;
  rcb = ent.rb;
  mz = ent.mz;
  
  /*
    printf("SVTV1P0_ADCR_SR::wafer = %d, hybrid = %d, hypersector = %d, rcb = %d, mz = %d\n",
    wafer,hybrid,hypersector,rcb,mz);
  */

  if ((hypersector) && (rcb) && (mz)) {
    banks = detector->getBankSVTADCR(hypersector,rcb,mz);
    
    if (banks) {
      // check the format number for different raw data formats:
      if (banks->header.FormatNumber == 2) {
	// new format of raw data, need to convert it:
	u_char *adc_old = (u_char *)banks->ADC;
	u_char adc_new[6][256][128];
	for (hybrid=0; hybrid<6; hybrid++) {
	  u_char *ptr0   = adc_old + hybrid*128*256;
	  u_char *ptr64  = adc_old + hybrid*128*256 + 1;
	  u_char *ptr128 = adc_old + hybrid*128*256 + 2;
	  u_char *ptr192 = adc_old + hybrid*128*256 + 3;
	  for (int anode=0; anode<64; anode++)
	    for (int tb=0; tb<128; tb++) {
	      adc_new[hybrid][anode][tb]     = *ptr0;
	      adc_new[hybrid][anode+64][tb]  = *ptr64;
	      adc_new[hybrid][anode+128][tb] = *ptr128;
	      adc_new[hybrid][anode+192][tb] = *ptr192;
	      ptr0+=4; ptr64+=4; ptr128+=4; ptr192+=4;
	    }
	}
	memcpy((void *)adc_old, (const void *)adc_new, 6*256*128);
	// since this now looks like unscrambled (format 0) data
	// change the format number in the header to 0
	banks->header.FormatNumber = 0;
      } // ...if (banks->header.FormatNumber == 2
    }   // ...if (banks
  }
  else
    return FALSE;
  
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
  
  int offset = (ent.offset * anodkr->getADCBytes() * SVT_MZANODES) + 
    (anodkr->getADCBytes() * (Anode-1));
  *nArray = anodkr->getADCBytes();

  //  printf("Offset = %d\n",offset);
  // printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);

  if (banks != NULL) 
    {
      *Array = (((u_char *)banks->ADC) + offset);
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
  banks = (classname(Bank_SVTPEDR) *)NULL;
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
  banks = (classname(Bank_SVTPEDR) *)NULL;
  numEvents = 0;
}

int SVTV1P0_PEDR_SR::initialize()
{
  // get a sector reader for ANODK
  anodkr = detector->getANODKReader();
  if (!anodkr) return FALSE;

  int hypersector, rcb, mz;

  ANODK_entry ent;
  // store pointers to the PEDR banks
  int hybrid = 1; // both hybrids are in same bank
  if (barrel)
    anodkr->get(barrel, ladder, wafer, hybrid, &ent);
  else
    anodkr->get(wafer, hybrid, &ent);
  
  hypersector = ent.hypersector;
  rcb = ent.rb;
  mz = ent.mz;
  
  banks = detector->getBankSVTPEDR(hypersector,rcb,mz);    

  if (banks != NULL) { 
    numEvents = banks->NumEvents;
    // check the format number for different raw data formats:
    if (banks->header.FormatNumber == 2) {
      // new format of raw data, need to convert it:
      u_char *adc_old = (u_char *)banks->pedestal;
      u_char adc_new[6][256][128];
      for (hybrid=0; hybrid<6; hybrid++) {
	u_char *ptr0   = adc_old + hybrid*128*256;
	u_char *ptr64  = adc_old + hybrid*128*256 + 1;
	u_char *ptr128 = adc_old + hybrid*128*256 + 2;
	u_char *ptr192 = adc_old + hybrid*128*256 + 3;
	for (int anode=0; anode<64; anode++)
	  for (int tb=0; tb<128; tb++) {
	    adc_new[hybrid][anode][tb]     = *ptr0;
	    adc_new[hybrid][anode+64][tb]  = *ptr64;
	    adc_new[hybrid][anode+128][tb] = *ptr128;
	    adc_new[hybrid][anode+192][tb] = *ptr192;
	    ptr0+=4; ptr64+=4; ptr128+=4; ptr192+=4;
	  }
      }
      memcpy((void *)adc_old, (const void *)adc_new, 6*256*128);
      // since this now looks like unscrambled (format 0) data
      // change the format number in the header to 0
      banks->header.FormatNumber = 0;
    } // ...if (banks->header.FormatNumber == 2
  }   // ...if (banks != NULL

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
  
  int offset = (ent.offset * anodkr->getPEDBytes() * SVT_MZANODES) + 
    (anodkr->getPEDBytes() * (Anode-1));
  *nArray = anodkr->getPEDBytes();

  if (banks != NULL) {
    //      printf("Offset = %d\n",offset);
    //      printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
    *Array = (((u_char *)banks->pedestal) + offset);
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
  banks = (classname(Bank_SVTRMSR) *)NULL;
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
  banks = (classname(Bank_SVTRMSR) *)NULL;
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
  int hybrid = 1;
  if (barrel)
    anodkr->get(barrel, ladder, wafer, hybrid, &ent);
  else
    anodkr->get(wafer, hybrid, &ent);
  
  hypersector = ent.hypersector;
  rcb = ent.rb;
  mz = ent.mz;
  
  banks = detector->getBankSVTRMSR(hypersector,rcb,mz);    
  
  if (banks != NULL) { 
    numEvents = banks->NumEvents;

    // check the format number for different raw data formats:
    if (banks->header.FormatNumber == 2) {
      // new format of raw data, need to convert it:
      u_char *adc_old = (u_char *)banks->pedRMSt16;
      u_char adc_new[6][256][128];
      for (hybrid=0; hybrid<6; hybrid++) {
	u_char *ptr0   = adc_old + hybrid*128*256;
	u_char *ptr64  = adc_old + hybrid*128*256 + 1;
	u_char *ptr128 = adc_old + hybrid*128*256 + 2;
	u_char *ptr192 = adc_old + hybrid*128*256 + 3;
	for (int anode=0; anode<64; anode++)
	  for (int tb=0; tb<128; tb++) {
	    adc_new[hybrid][anode][tb]     = *ptr0;
	    adc_new[hybrid][anode+64][tb]  = *ptr64;
	    adc_new[hybrid][anode+128][tb] = *ptr128;
	    adc_new[hybrid][anode+192][tb] = *ptr192;
	    ptr0+=4; ptr64+=4; ptr128+=4; ptr192+=4;
	  }
      }
      memcpy((void *)adc_old, (const void *)adc_new, 6*256*128);
      // since this now looks like unscrambled (format 0) data
      // change the format number in the header to 0
      banks->header.FormatNumber = 0;
    } // ...if (banks->header.FormatNumber == 2
  }   // ...if (banks !=NULL

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
  
  int offset = (ent.offset * anodkr->getRMSBytes() * SVT_MZANODES) + 
    (anodkr->getRMSBytes() * (Anode-1));
  *nArray = anodkr->getRMSBytes();

  if (banks != NULL)
    {
      // printf("Offset = %d\n",offset);
      // printf("array coord:  rb=%d,  mz=%d\n",ent.rb,ent.mz);
      *Array = (((u_char *)banks->pedRMSt16) + offset);
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


