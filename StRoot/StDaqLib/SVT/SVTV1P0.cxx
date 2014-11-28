/***************************************************************************
 *      
 * $Id: SVTV1P0.cxx,v 1.5 2007/12/24 06:04:27 fine Exp $
 *      
 * Author: Jeff Landgraf, M.J. LeVine, Marcelo Munhoz, J. Schambach
 *      
 ***************************************************************************
 *      
 * Description: common SVT implementation stuff
 *      
 ***************************************************************************
 *      
 * $Log: SVTV1P0.cxx,v $
 * Revision 1.5  2007/12/24 06:04:27  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.4  2007/01/04 21:27:50  jml
 * zero suppressed reader no longer uses adcx, only seqd.  Fixes bug from early 2005
 *
 * Revision 1.3  2001/05/04 18:05:23  jschamba
 * Corrected several instances of wrong counting representation for hypersector
 * and mezzanine (internal counting from 0, physical from 1)
 *
 * Revision 1.2  2001/04/18 19:47:25  ward
 * StDaqLib/SVT stuff from Jo Schambach.
 *
 * Revision 1.1  2000/06/06 18:08:31  jml
 * Initial version of SVT Readers (author: marcello munholz, helen caines, js)
 *
 *      
 **************************************************************************/
// SVT implementation
#include "SVTV1P0.hh"

using namespace OLDEVP;

SVTV1P0_ANODK_SR::SVTV1P0_ANODK_SR(SVTV1P0_Reader *det)
{
  detector = det;
}

int SVTV1P0_ANODK_SR::initialize(int maxSector)
{
  //  cout << "Initializing ANODK wafer: " <<  wafer <<  endl;

  // zero out lookup array  
  memset((char *)packed_address, 0, sizeof(packed_address));

  //cout << "Sizeof() = " << sizeof(packed_address) << endl;

  ANODK_entry ent;

  for(int hypersector = 1; hypersector <= maxSector; hypersector++)
    {
      for(int rcb = 1; rcb <= 6; rcb++)
	{
	  for(int mz = 1; mz <= 3; mz++)
	    {
	    
	      classname(Bank_SVTANODK) *raw_bank = detector->getBankSVTANODK(hypersector, rcb, mz);
	      if(raw_bank)
		{
		  //printf("ANODK DATA for hypersector %d, rcb %d, mz %d\n",hypersector,rcb,mz);
		}
	      else
		{
		  // missing ANODK bank
		  //printf("No ANODK DATA, hypersector %d, rcb %d, mz %d\n",hypersector,rcb,mz);
		  //printf("ERR: %s\n",detector->errstr0);
		  continue;
		}
	      
	      for(int i=0; i < SVT_MZHYBRIDS; i++)
		{
		  int barrel = raw_bank->index[i].barrel;
		  int ladder = raw_bank->index[i].ladder;
		  int wafer = raw_bank->index[i].waferID & 0x0F;
		  int hybrid = (raw_bank->index[i].waferID & 0xF0) >> 4;
		  
		  //if((padrow == 0xFF) && (anode == 0xFF)) continue;
		  
		  ent.offset = i;
		  ent.mz = mz;
		  ent.rb = rcb;
		  ent.hypersector = hypersector;
		  
		  place(barrel, ladder, wafer, hybrid, &ent);

		  //printf("ANODK DATA for barrel %d, ladder %d, wafer %d, hybrid %d\n",barrel,ladder,wafer,hybrid);

		}
	    }
	}
    }

  return TRUE;
}

void SVTV1P0_ANODK_SR::place(short barrel, short ladder, short wafer, short hybrid, ANODK_entry *p)
{
  short waferIndex = getWaferIndex(barrel,ladder,wafer);
  packed_address[waferIndex-1][hybrid-1] = pack(p->hypersector, p->rb, p->mz, p->offset);
}

void SVTV1P0_ANODK_SR::get(short barrel, short ladder, short wafer, short hybrid, ANODK_entry *p)
{
  short waferIndex = getWaferIndex(barrel,ladder,wafer);
  unpack(p,packed_address[waferIndex-1][hybrid-1]);
}

void SVTV1P0_ANODK_SR::get(short waferIndex, short hybrid, ANODK_entry *p)
{
  unpack(p,packed_address[waferIndex-1][hybrid-1]);
}

short SVTV1P0_ANODK_SR::pack(short hypersector, short rcb, short mz, short offset)
{
  //printf("SVTV1P0_ANODK_SR::pack:hypersector = %d, rcb = %d, mz = %d\n",hypersector,rcb,mz);
  short p = hypersector;  // 5 bits 
  p = p << 5;  
  p += rcb;  // 5 bits 
  p = p << 2;  
  p += mz;      // 2 bits
  p = p << 4;
  p += offset;  // 4 bits
  return p;
} 

void SVTV1P0_ANODK_SR::unpack(ANODK_entry *entry, short anodedress)
{
  entry->offset = anodedress & 0x000F;
  entry->mz = (anodedress & 0x0030) >> 4;
  entry->rb = (anodedress & 0x07C0) >> 6;
  entry->hypersector = anodedress >> 11;
  //printf("SVTV1P0_ANODK_SR::unpack:hypersector = %d, rcb = %d, mz = %d\n",entry->hypersector,entry->rb,entry->mz);
}

short SVTV1P0_ANODK_SR::getWaferIndex(short barrel, short ladder, short wafer)
{
 short index;
 int numberOfLadders[3] = {8,12,16};
 int numberOfWafers[3] = {4,6,7};
 // int numberOfHybrids = 2;

  switch  (barrel) {
    
  case 1:
    index = (ladder-1)*numberOfWafers[barrel-1] + wafer;
    break;

  case 2:
    index = numberOfLadders[barrel-2]*numberOfWafers[barrel-2] +
      (ladder-1)*numberOfWafers[barrel-1] + wafer;
    break;

  case 3:
    index = numberOfLadders[barrel-3]*numberOfWafers[barrel-3] + 
      numberOfLadders[barrel-2]*numberOfWafers[barrel-2] +
      (ladder-1)*numberOfWafers[barrel-1] + wafer;
    break;

 default:
   cout << "ERROR: There is NO barrel number " << barrel << " !!!" << endl;
   index = -1;
   break;
  }

  return index;
}

ZeroSuppressedReader *SVTV1P0_Reader::getZeroSuppressedReader(int wafer)
{
  if (ercpy->verbose) cout << "getSVT_ZS_SR wafer = " <<  wafer << endl;
  
  SVTV1P0_ZS_SR *zsp = new SVTV1P0_ZS_SR(wafer, this);
  if(!zsp->initialize())
  {
  if (ercpy->verbose) 
    cout << "ERROR: getSVT_ZS_SR FAILED wafer = " << wafer << endl;
    delete zsp;
    zsp = NULL;
  }

  return (ZeroSuppressedReader *)zsp;
}

ZeroSuppressedReader *SVTV1P0_Reader::getZeroSuppressedReader(int barrel, int ladder, int wafer)
{
  if (ercpy->verbose) cout << "getSVT_ZS_SR wafer = " <<  wafer << endl;
  
  SVTV1P0_ZS_SR *zsp = new SVTV1P0_ZS_SR(barrel, ladder, wafer, this);
  if(!zsp->initialize())
  {
  if (ercpy->verbose) 
    cout << "ERROR: getSVT_ZS_SR FAILED wafer = " << wafer << endl;
    delete zsp;
    zsp = NULL;
  }

  return (ZeroSuppressedReader *)zsp;
}

ADCRawReader *SVTV1P0_Reader::getADCRawReader(int wafer)
{
  //  cout << "getSVT_ADCR_SR" << endl;
  SVTV1P0_ADCR_SR *adc = new SVTV1P0_ADCR_SR(wafer, this);
  if(!adc->initialize())
  {
    delete adc;
    adc = NULL;
  }

  return (ADCRawReader *)adc;
}

ADCRawReader *SVTV1P0_Reader::getADCRawReader(int barrel, int ladder, int wafer)
{
  //  cout << "getSVT_ADCR_SR" << endl;
  SVTV1P0_ADCR_SR *adc = new SVTV1P0_ADCR_SR(barrel, ladder, wafer, this);
  if(!adc->initialize())
  {
    delete adc;
    adc = NULL;
  }

  return (ADCRawReader *)adc;
}

PedestalReader *SVTV1P0_Reader::getPedestalReader(int wafer)
{
  //  cout << "getSVT_P_SR" << endl;
  SVTV1P0_PEDR_SR *ped = new SVTV1P0_PEDR_SR(wafer, this);
  if(!ped->initialize())
    {
      delete ped;
      ped = NULL;
    }

  return (PedestalReader *)ped;
}

PedestalReader *SVTV1P0_Reader::getPedestalReader(int barrel, int ladder, int wafer)
{
  //  cout << "getSVT_P_SR" << endl;
  SVTV1P0_PEDR_SR *ped = new SVTV1P0_PEDR_SR(barrel, ladder, wafer, this);
  if(!ped->initialize())
    {
      delete ped;
      ped = NULL;
    }

  return (PedestalReader *)ped;
}

PedestalRMSReader *SVTV1P0_Reader::getPedestalRMSReader(int wafer)
{
  // cout << "getSVT_PRMS_SR" << endl;
  SVTV1P0_PRMS_SR *rms = new SVTV1P0_PRMS_SR(wafer, this);
  if(!rms->initialize())
    {
      delete rms;
      rms = NULL;
    }
  
  return (PedestalRMSReader *)rms;
}

PedestalRMSReader *SVTV1P0_Reader::getPedestalRMSReader(int barrel, int ladder, int wafer)
{
  // cout << "getSVT_PRMS_SR" << endl;
  SVTV1P0_PRMS_SR *rms = new SVTV1P0_PRMS_SR(barrel, ladder, wafer, this);
  if(!rms->initialize())
    {
      delete rms;
      rms = NULL;
    }
  
  return (PedestalRMSReader *)rms;
}

GainReader *SVTV1P0_Reader::getGainReader(int wafer)
{
  if (ercpy->verbose) cout << "getSVT_G_SR" << endl;
  return NULL;
}

GainReader *SVTV1P0_Reader::getGainReader(int barrel, int ladder, int wafer)
{
  if (ercpy->verbose) cout << "getSVT_G_SR" << endl;
  return NULL;
}

CPPReader *SVTV1P0_Reader::getCPPReader(int wafer)
{
  //  cout << "getSVT_CPP_SR" << endl;
  SVTV1P0_CPP_SR *cpp = new SVTV1P0_CPP_SR(wafer, this);
  if(!cpp->initialize())
  {
    delete cpp;
    cpp = NULL;
  }

  return (CPPReader *)cpp;}

CPPReader *SVTV1P0_Reader::getCPPReader(int barrel, int ladder, int wafer)
{
  //  cout << "getSVT_CPP_SR" << endl;
  SVTV1P0_CPP_SR *cpp = new SVTV1P0_CPP_SR(barrel, ladder, wafer, this);
  if(!cpp->initialize())
  {
    delete cpp;
    cpp = NULL;
  }

  return (CPPReader *)cpp;}

BadChannelReader *SVTV1P0_Reader::getBadChannelReader(int wafer)
{
  if (ercpy->verbose) cout << "getSVT_BC_SR" << endl;
  return NULL;
}

BadChannelReader *SVTV1P0_Reader::getBadChannelReader(int barrel, int ladder, int wafer)
{
  if (ercpy->verbose) cout << "getSVT_BC_SR" << endl;
  return NULL;
}

ConfigReader *SVTV1P0_Reader::getConfigReader(int wafer)
{
  if (ercpy->verbose) cout << "getSVT_CR_SR" << endl;
  return NULL;
}

ConfigReader *SVTV1P0_Reader::getConfigReader(int barrel, int ladder, int wafer)
{
  if (ercpy->verbose) cout << "getSVT_CR_SR" << endl;
  return NULL;
}

SVTV1P0_ANODK_SR *SVTV1P0_Reader::getANODKReader()
{
  //  cout << "GetANODKReader" << endl;
  
  //JS: can't do that anymore. The SVTP bank length is now 58, just like TPC
  // so the length has no relation to the number of sectors anymore.
  //int maxSector = pBankSVTP->header.BankLength/sizeof(Pointer)*2;

  int maxSector = 4;
  SVTV1P0_ANODK_SR *p;
  p = anodk;
  if(p == NULL)
  {
    p = new SVTV1P0_ANODK_SR(this);
    if(!p->initialize(maxSector))
    {
      //      if (ercpy->verbose) cout << "Error Reading ANODK banks" 
      //	   << ": " << errstr() << endl;
      delete p;
      return NULL;
    }
  }
  anodk = p;
  return p;
}

SVTV1P0_Reader::SVTV1P0_Reader(EventReader *er, classname(Bank_SVTP) *psvt)
{
  pBankSVTP = psvt; // copy pointer into class variable
  ercpy = er; // squirrel away pointer eventreader for our friends

  if (!pBankSVTP->test_CRC()) ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,"SVTP");
  if (pBankSVTP->swap() < 0) ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,"SVTP");
  pBankSVTP->header.CRC = 0;
  anodk = NULL;
}

SVTV1P0_Reader::~SVTV1P0_Reader()
{
  //  cout << "SVT destructor" << endl;
  
  // Delete Sector Readers buffers (The actual readers are deleted by client)
  
  // Delete ANODK's
  if(anodk != NULL) delete anodk;
}

int SVTV1P0_Reader::MemUsed()
{
  return 0;
}

// -----------------------------------------------------
// Here are the bank retrieval functions
// ---- These NAVIGATE to the raw banks
// ----------------------------------------------------- 

classname(Bank_SVTSECP) *SVTV1P0_Reader::getBankSVTSECP(int hypersector)
{
  //printf("getbankSVTSECP %d\n",hypersector);

  if((hypersector < 1 ) || (hypersector > 4))
  {
    char str0[40];
    sprintf(str0,"getBankSVTSECP(hs %d)",hypersector);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }
  hypersector--; //convert to internal represenation

  if((!pBankSVTP->HyperSector[hypersector].offset) ||
     (!pBankSVTP->HyperSector[hypersector].length))
  {
    char str0[40];
    sprintf(str0,"getBankSVTSECP(hs %d)",hypersector+1);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    return NULL;
  }

  classname(Bank_SVTSECP) *ptr = (classname(Bank_SVTSECP) *)
                      (((INT32 *)pBankSVTP) + 
			pBankSVTP->HyperSector[hypersector].offset);

  if(strncmp(ptr->header.BankType,"SVTSECP",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTSECP(hs %d)",hypersector+1);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL; 
  }

  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankSVTSECP(hs %d)",hypersector+1);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTSECP(hs %d)",hypersector+1);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;
 
  return ptr;
}

classname(Bank_SVTRBP) *SVTV1P0_Reader::getBankSVTRBP(int interleaved_rb, 
					   classname(Bank_SVTSECP) *secp)
{
  int hypersector = secp->header.BankId;
  // interleaved_rb counts from 1 to 12 
  if ((interleaved_rb < 1) || (interleaved_rb > 12))
  {
    char str0[40];
    sprintf(str0,"getBankSVTRBP(sec %d rb %d )",hypersector,interleaved_rb);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }

  //convert to internal representation:
  interleaved_rb--;

  //printf("getBankSVTRBP RB: %d\n",interleaved_rb);
   //   secp->print();

  if ((!secp->RcvBoard[interleaved_rb].offset) ||
      (!secp->RcvBoard[interleaved_rb].length) )
  { 
    //    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_SVTRBP) *ptr = (classname(Bank_SVTRBP) *)
                     (((INT32 *)secp) + 
		      secp->RcvBoard[interleaved_rb].offset);

  if(strncmp(ptr->header.BankType,"SVTRBP",6)) {
    char str0[40];
    sprintf(str0,"getBankSVTRBP(sec %d rb %d )",hypersector,interleaved_rb);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankSVTRBP(sec %d rb %d )",hypersector,interleaved_rb);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankSVTRBP(sec %d rb %d )",hypersector,interleaved_rb);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  mSCAZero = (int)ptr->FiberHeader[16];
  mTimeZero = (int)ptr->FiberHeader[17];

  return ptr;
}

classname(Bank_SVTMZP) *SVTV1P0_Reader::getBankSVTMZP(int mz, classname(Bank_SVTRBP) *rbp)
{
  // mezzanine counts from 1 to 3
  int rb = rbp->header.BankId;
  if ((mz < 1) || (mz > 3))
  {
    char str0[40];
    sprintf(str0,"getBankSVTMZP(rb %d  mz %d )",rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }

  // convert to internal representation
  mz--;

  if ((!rbp->Mz[mz].offset) || (!rbp->Mz[mz].length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTMZP(rb %d  mz %d )",rb,mz+1);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTMZP) *ptr = (classname(Bank_SVTMZP) *)
                     (((INT32 *)rbp) +
		      rbp->Mz[mz].offset);

  if(strncmp(ptr->header.BankType,"SVTMZP",6)) {
    char str0[40];
    sprintf(str0,"getBankSVTMZP(rb %d  mz %d )",rb,mz+1);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankSVTMZP(rb %d  mz %d )",rb,mz+1);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTMZP(rb %d  mz %d )",rb,mz+1);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

//    printf("getBankSVTMZP Mezz: %d\n",mz);
//    ptr->print();

  return ptr;
}

classname(Bank_SVTMZP) *SVTV1P0_Reader::getBankSVTMZP(int hypersector, int rb, int mz)
{
  //printf("getBankSVTMZP for hypersector %d, rcb %d, mz %d\n",hypersector,rb,mz);

  if ((hypersector < 1) || (hypersector > SVT_HYPERSECTORS))
  {
    char str0[40];
    sprintf(str0,"getBankSVTMZP(sec %d, rb %d, mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }

  if ((rb < 1) || (rb > 6))
  {
    char str0[40];
    sprintf(str0,"getBankSVTMZP(sec %d, rb %d, mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }

  if ((mz < 1) || (mz > 3))
  {
    char str0[40];
    sprintf(str0,"getBankSVTMZP(sec %d, rb %d, mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }

  // convert to internal representation:
  hypersector--;

  // use odd slots 1 and 3
  classname(Bank_SVTSECP) *secp = getBankSVTSECP(2*(hypersector/2)+1);
  if(!secp) return NULL;

  classname(Bank_SVTRBP) *rbp;
  if (hypersector%2)             // internal hypersector odd -> hypersector even -> rb = 7 to 12 
    {
      rbp = getBankSVTRBP(rb + 6, secp);
    }
  else                           // internal hypersector even -> hypersector odd -> rb = 1 to 6 
    {
      rbp = getBankSVTRBP(rb, secp);
    }
  if(!rbp) return NULL;

  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(mz,rbp);

  return mzp;
}

classname(Bank_SVTADCD) *SVTV1P0_Reader::getBankSVTADCD(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

//   printf(" hypersector: %d  RB: %d  MZ: %d\n", hypersector, rb, mz);
//   mzp->print();

  if((!mzp->SVTADCD.offset) || (!mzp->SVTADCD.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTADCD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTADCD) *ptr = (classname(Bank_SVTADCD) *)
                      (((INT32 *)mzp) +
		       mzp->SVTADCD.offset);

  if(strncmp(ptr->header.BankType,"SVTADCD",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTADCD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankSVTADCD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTADCD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_SVTSEQD) *SVTV1P0_Reader::getBankSVTSEQD(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';


  //printf("get mzp\n");
  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

  //printf("mzp hypersector: %d  RB: %d  MZ: %d\n", hypersector, rb, mz);
  //mzp->print();
  

  if((!mzp->SVTSEQD.offset) || (!mzp->SVTSEQD.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTSEQD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTSEQD) *ptr = (classname(Bank_SVTSEQD) *)
                      (((INT32 *)mzp) +
		       mzp->SVTSEQD.offset);


  if(strncmp(ptr->header.BankType,"SVTSEQD",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTSEQD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankSVTSEQD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTSEQD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;
  return ptr;}

classname(Bank_SVTADCX) *SVTV1P0_Reader::getBankSVTADCX(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

//   printf(" hypersector: %d  RB: %d  MZ: %d\n", hypersector, rb, mz);
//   mzp->print();

  if((!mzp->SVTADCX.offset) || (!mzp->SVTADCX.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTADCX(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTADCX) *ptr = (classname(Bank_SVTADCX) *)
                      (((INT32 *)mzp) +
		       mzp->SVTADCX.offset);

  if(strncmp(ptr->header.BankType,"SVTADCX",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTADCX(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankSVTADCX(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankSVTADCX(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;}

classname(Bank_SVTANODK) *SVTV1P0_Reader::getBankSVTANODK(int hypersector, int rb, int mz)
{
  //  cout << "getBankSVTANODK" << endl;
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

//   printf(" hypersector: %d  RB: %d  MZ: %d\n", hypersector, rb, mz);
//   mzp->print();

  if((!mzp->SVTANODK.offset) || (!mzp->SVTANODK.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTANODK(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTANODK) *ptr = (classname(Bank_SVTANODK) *)
                      (((INT32 *)mzp) +
		       mzp->SVTANODK.offset);

  if(strncmp(ptr->header.BankType,"SVTANODK",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTANODK(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankSVTANODK(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankSVTANODK(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;
};

classname(Bank_SVTCPPR) *SVTV1P0_Reader::getBankSVTCPPR(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->SVTCPPR.offset) || (!mzp->SVTCPPR.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTCPPR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTCPPR) *ptr = (classname(Bank_SVTCPPR) *)
                      (((INT32 *)mzp) +
			mzp->SVTCPPR.offset);

  if(strncmp(ptr->header.BankType,"SVTCPPR",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTCPPR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankSVTCPPR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTCPPR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_SVTADCR) *SVTV1P0_Reader::getBankSVTADCR(int hypersector, int rb, int mz)
{
  //printf("getBankSVTADCR for hypersector %d, rcb %d, mz %d\n",hypersector,rb,mz);

  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->SVTADCR.offset) || (!mzp->SVTADCR.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTADCR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTADCR) *ptr = (classname(Bank_SVTADCR) *)
                      (((INT32 *)mzp) +
			mzp->SVTADCR.offset);

  if(strncmp(ptr->header.BankType,"SVTADCR",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTADCR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankSVTADCR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankSVTADCR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_SVTMZCLD) *SVTV1P0_Reader::getBankSVTMZCLD(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->SVTMZCLD.offset) || (!mzp->SVTMZCLD.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTMZCLD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTMZCLD) *ptr = (classname(Bank_SVTMZCLD) *)
                      (((INT32 *)mzp) +
			mzp->SVTMZCLD.offset);

  if(strncmp(ptr->header.BankType,"SVTMZCLD",8)) {
    char str0[40];
    sprintf(str0,"getBankSVTMZCLD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankSVTMZCLD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTMZCLD(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}


classname(Bank_SVTCFGR) *SVTV1P0_Reader::getBankSVTCFGR(int hypersector, int rb, int mz)
{
  return NULL;
}

classname(Bank_SVTPEDR) *SVTV1P0_Reader::getBankSVTPEDR(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->SVTPEDR.offset) || (!mzp->SVTPEDR.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTPEDR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTPEDR) *ptr = (classname(Bank_SVTPEDR) *)
                      (((INT32 *)mzp) +
			mzp->SVTPEDR.offset);

  if(strncmp(ptr->header.BankType,"SVTPEDR",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTPEDR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankSVTPEDR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTPEDR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}


classname(Bank_SVTRMSR) *SVTV1P0_Reader::getBankSVTRMSR(int hypersector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_SVTMZP) *mzp = getBankSVTMZP(hypersector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->SVTRMSR.offset) || (!mzp->SVTRMSR.length))
  { 
    char str0[40];
    sprintf(str0,"getBankSVTRMSR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_SVTRMSR) *ptr = (classname(Bank_SVTRMSR) *)
                      (((INT32 *)mzp) +
			mzp->SVTRMSR.offset);

  if(strncmp(ptr->header.BankType,"SVTRMSR",7)) {
    char str0[40];
    sprintf(str0,"getBankSVTRMSR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankSVTRMSR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankSVTRMSR(sec %d rb %d  mz %d )",hypersector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_SVTGAINR) *SVTV1P0_Reader::getBankSVTGAINR(int hypersector, int rb, int mz)
{
  return NULL;
}

classname(Bank_SVTBADR) *SVTV1P0_Reader::getBankSVTBADR(int hypersector, int rb, int mz)
{
  return NULL;
}

