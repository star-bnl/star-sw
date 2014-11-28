/***************************************************************************
 * $Id: TPCV1P0.cxx,v 1.6 2007/12/24 06:04:31 fine Exp $
 * Author: M.J. LeVine
 ***************************************************************************
 * Description:  TPCV1P0 implementation
 *      
 *
 *   change log
 * 02-Jun-99 MJL fixed test on hypersector arg of getBankTPCSECP
 *
 ***************************************************************************
 * $Log: TPCV1P0.cxx,v $
 * Revision 1.6  2007/12/24 06:04:31  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.5  2000/01/11 22:03:44  levine
 * convert string to char* via c_str() member
 * (from Brian Lasiuk)
 *
 * Revision 1.4  1999/07/10 21:31:25  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.3  1999/07/02 04:43:23  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/

#include "TPCV1P0.hh"

using namespace OLDEVP;


TPCV1P0_PADK_SR::TPCV1P0_PADK_SR(int s, TPCV1P0_Reader *det)
{
  sector = s;
  detector = det;
}

int TPCV1P0_PADK_SR::initialize()
{
  //  cout << "Initializing PADK sector: " << sector << endl;

  // zero out lookup array  
  memset((char *)packed_address, 0, sizeof(packed_address));

  //  cout << "Sizeof() = " << sizeof(packed_address) << endl;

  PADK_entry ent;

  for(int rcb = 0; rcb < 6; rcb++)
  {
    for(int mz = 0; mz < 3; mz++)
    {
      classname(Bank_TPCPADK) *raw_bank = detector->getBankTPCPADK(sector, rcb, mz);
      if(raw_bank)
      {
	//	printf("PADK DATA for sector %d, rcb %d, mz %d\n",sector,rcb,mz);
      }
      else
      {
	//		 printf("No PADK DATA, sector %d, rcb %d, mz %d\n",sector,rcb,mz);
	//		 printf("ERR: %s\n",detector->errstr0);
		continue;
      }

      for(int i=0; i < TPC_MZPADS; i++)
      {
		int padrow = raw_bank->index[i].pad_row;
		int pad = raw_bank->index[i].pad;
		if((padrow == 0xFF) && (pad == 0xFF)) continue;
		  
		ent.offset = i;
		ent.mz = mz+1;
		ent.rb = rcb+1;
		
		place(padrow, pad, &ent);
      }
    }
  }
       
  return TRUE;
}

void TPCV1P0_PADK_SR::place(short padrow, short pad, PADK_entry *p)
{
  padrow--; pad--;  // use c standard for array
  packed_address[padrow][pad] = pack(p->rb, p->mz, p->offset);
}

void TPCV1P0_PADK_SR::get(short padrow, short pad, PADK_entry *p)
{
  padrow--; pad--;  // use c standard for array
  unpack(p,packed_address[padrow][pad]);
}

short TPCV1P0_PADK_SR::pack(short rcb, short mz, short offset)
{
  short p = rcb;  // 4 bits 
  p = p << 2;  
  p += mz;      // 2 bits
  p = p << 10;
  p += offset;  // 10 bits
  return p;
} 

void TPCV1P0_PADK_SR::unpack(PADK_entry *entry, short paddress)
{
  entry->offset = paddress & 0x03FF;
  entry->mz = (paddress & 0x0C00) >> 10;
  entry->rb = paddress >> 12;
}

ZeroSuppressedReader *TPCV1P0_Reader::getZeroSuppressedReader(int sector)
{
  cout << "getTPCV1P0_ZS_SR sector(" << sector <<")" << endl;
  
  TPCV1P0_ZS_SR *zsp = new TPCV1P0_ZS_SR(sector, this);
  if(!zsp->initialize())
  {
  cout << "ERROR: getTPCV1P0_ZS_SR FAILED sector(" << sector <<")" << endl;
    delete zsp;
    zsp = NULL;
  }

  return (ZeroSuppressedReader *)zsp;
}

ADCRawReader *TPCV1P0_Reader::getADCRawReader(int sector)
{
  //  cout << "getTPCV1P0_ADCR_SR" << endl;
  TPCV1P0_ADCR_SR *adc = new TPCV1P0_ADCR_SR(sector, this);
  if(!adc->initialize())
  {
    delete adc;
    adc = NULL;
  }

  return (ADCRawReader *)adc;
}

PedestalReader *TPCV1P0_Reader::getPedestalReader(int sector)
{
  //  cout << "getTPCV1P0_P_SR" << endl;
  TPCV1P0_PEDR_SR *ped = new TPCV1P0_PEDR_SR(sector, this);
  if(!ped->initialize())
    {
      delete ped;
      ped = NULL;
    }

  return (PedestalReader *)ped;
}

PedestalRMSReader *TPCV1P0_Reader::getPedestalRMSReader(int sector)
{
  //  cout << "getTPCV1P0_PRMS_SR" << endl;
  TPCV1P0_PRMS_SR *rms = new TPCV1P0_PRMS_SR(sector, this);
  if(!rms->initialize())
    {
      delete rms;
      rms = NULL;
    }
  
  return (PedestalRMSReader *)rms;
}

GainReader *TPCV1P0_Reader::getGainReader(int sector)
{
  cout << "getTPCV1P0_G_SR" << endl;
  return NULL;
}

CPPReader *TPCV1P0_Reader::getCPPReader(int sector)
{
  //  cout << "getTPCV1P0_CPP_SR" << endl;
  TPCV1P0_CPP_SR *cpp = new TPCV1P0_CPP_SR(sector, this);
  if(!cpp->initialize())
  {
    delete cpp;
    cpp = NULL;
  }

  return (CPPReader *)cpp;}

BadChannelReader *TPCV1P0_Reader::getBadChannelReader(int sector)
{
  cout << "getTPCV1P0_BC_SR" << endl;
  return NULL;
}

ConfigReader *TPCV1P0_Reader::getConfigReader(int sector)
{
  cout << "getTPCV1P0_CR_SR" << endl;
  return NULL;
}

TPCV1P0_PADK_SR *TPCV1P0_Reader::getPADKReader(int sector)
{
  //  cout << "GetPADKReader" << endl;
  
  TPCV1P0_PADK_SR *p;
  p = padk[sector];
  if(p == NULL)
  {
    p = new TPCV1P0_PADK_SR(sector, this);
    if(!p->initialize())
    {
      cout << "Error Reading PADK banks, sector=" << sector 
	   << ": " << errstr().c_str() << endl;
      delete p;
      return NULL;
    }
  }
  padk[sector] = p;
  return p;
}

TPCV1P0_Reader::TPCV1P0_Reader(EventReader *er, classname(Bank_TPCP) *ptpc)
{
  pBankTPCP = ptpc; // copy arg into class variable

  if (!pBankTPCP->test_CRC()) ERROR(ERR_CRC);
  if (pBankTPCP->swap() < 0) ERROR(ERR_SWAP);
  pBankTPCP->header.CRC = 0;

  // We can have a padk for each of 24 sectors
  for(int i=0;i<TPC_SECTORS;i++)
  {
    padk[i] = NULL;
  }
}

TPCV1P0_Reader::~TPCV1P0_Reader()
{
  //  cout << "TPCV1P0 destructor" << endl;
  
  // Delete Sector Readers buffers (The actual readers are deleted by client)
  
  // Delete PADK's
  for(int i=0;i<TPC_SECTORS;i++)
  {
    if(padk[i] != NULL) delete padk[i];
  }   
}

int TPCV1P0_Reader::MemUsed()
{
  return 0;
}

// -----------------------------------------------------
// Here lie bank retrieval functions
// ---- These NAVAGATE to the raw banks
// ----------------------------------------------------- 

classname(Bank_TPCSECP) *TPCV1P0_Reader::getBankTPCSECP(int hypersector)
{
  if((hypersector <= 0) || (hypersector >= 24))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }
  hypersector--; //convert to internal represenation

  if((!pBankTPCP->HyperSector[hypersector].offset) ||
     (!pBankTPCP->HyperSector[hypersector].length))
  {
    pERROR(ERR_BANK);
    return NULL;
  }

  classname(Bank_TPCSECP) *ptr = (classname(Bank_TPCSECP) *)
                      (((INT32 *)pBankTPCP) + 
			pBankTPCP->HyperSector[hypersector].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;
 
  return ptr;
}

classname(Bank_TPCRBP) *TPCV1P0_Reader::getBankTPCRBP(int interleaved_rb, 
					   classname(Bank_TPCSECP) *secp)
{
  if ((interleaved_rb < 0) || (interleaved_rb >= 12))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//   printf("getBankTPCRBP RB: %d\n",interleaved_rb);
//   secp->print();

  if ((!secp->RcvBoard[interleaved_rb].offset) ||
      (!secp->RcvBoard[interleaved_rb].length) )
  { 
    //    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCRBP) *ptr = (classname(Bank_TPCRBP) *)
                     (((INT32 *)secp) + 
		      secp->RcvBoard[interleaved_rb].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_TPCMZP) *TPCV1P0_Reader::getBankTPCMZP(int mz, classname(Bank_TPCRBP) *rbp)
{
  if ((mz < 0) || (mz >= 3))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  if ((!rbp->Mz[mz].offset) || (!rbp->Mz[mz].length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCMZP) *ptr = (classname(Bank_TPCMZP) *)
                     (((INT32 *)rbp) +
		      rbp->Mz[mz].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

//    printf("getBankTPCMZP Mezz: %d\n",mz);
//    ptr->print();

  return ptr;
}

classname(Bank_TPCMZP) *TPCV1P0_Reader::getBankTPCMZP(int sector, int rb, int mz)
{
  if ((sector < 0) || (sector >= TPC_SECTORS))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }
  if ((rb < 0) || (rb >= 6))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }
  if ((mz < 0) || (mz >= 3))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  classname(Bank_TPCSECP) *secp = getBankTPCSECP(2*(sector/2)+1);
  // use odd slots 1,...,23
  if(!secp) return NULL;
  
  classname(Bank_TPCRBP) *rbp;
  if (sector%2)                // internal sector odd->sector even->7..12 
  {
    rbp = getBankTPCRBP(rb + 6, secp);
  }
  else                          // internal sector even->sector odd->1..6
  {
    rbp = getBankTPCRBP(rb, secp);
  }
  if(!rbp) return NULL;

  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(mz,rbp);
  return mzp;
}

classname(Bank_TPCADCD) *TPCV1P0_Reader::getBankTPCADCD(int sector, int rb, int mz)
{
    errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

//   printf(" sector: %d  RB: %d  MZ: %d\n", sector, rb, mz);
//   mzp->print();

  if((!mzp->TPCADCD.offset) || (!mzp->TPCADCD.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCADCD) *ptr = (classname(Bank_TPCADCD) *)
                      (((INT32 *)mzp) +
		       mzp->TPCADCD.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_TPCSEQD) *TPCV1P0_Reader::getBankTPCSEQD(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

//   printf(" sector: %d  RB: %d  MZ: %d\n", sector, rb, mz);
//   mzp->print();

  if((!mzp->TPCSEQD.offset) || (!mzp->TPCSEQD.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCSEQD) *ptr = (classname(Bank_TPCSEQD) *)
                      (((INT32 *)mzp) +
		       mzp->TPCSEQD.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;}

classname(Bank_TPCADCX) *TPCV1P0_Reader::getBankTPCADCX(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

//   printf(" sector: %d  RB: %d  MZ: %d\n", sector, rb, mz);
//   mzp->print();

  if((!mzp->TPCADCX.offset) || (!mzp->TPCADCX.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCADCX) *ptr = (classname(Bank_TPCADCX) *)
                      (((INT32 *)mzp) +
		       mzp->TPCADCX.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;}

classname(Bank_TPCPADK) *TPCV1P0_Reader::getBankTPCPADK(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

//   printf(" sector: %d  RB: %d  MZ: %d\n", sector, rb, mz);
//   mzp->print();

  if((!mzp->TPCPADK.offset) || (!mzp->TPCPADK.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCPADK) *ptr = (classname(Bank_TPCPADK) *)
                      (((INT32 *)mzp) +
		       mzp->TPCPADK.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
};

classname(Bank_TPCCPPR) *TPCV1P0_Reader::getBankTPCCPPR(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->TPCCPPR.offset) || (!mzp->TPCCPPR.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCCPPR) *ptr = (classname(Bank_TPCCPPR) *)
                      (((INT32 *)mzp) +
			mzp->TPCCPPR.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_TPCADCR) *TPCV1P0_Reader::getBankTPCADCR(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->TPCADCR.offset) || (!mzp->TPCADCR.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_TPCADCR) *ptr = (classname(Bank_TPCADCR) *)
                      (((INT32 *)mzp) +
			mzp->TPCADCR.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_TPCCFGR) *TPCV1P0_Reader::getBankTPCCFGR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCPEDR) *TPCV1P0_Reader::getBankTPCPEDR(int sector, int rb, int mz)
{
  return NULL;
}


classname(Bank_TPCRMSR) *TPCV1P0_Reader::getBankTPCRMSR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCGAINR) *TPCV1P0_Reader::getBankTPCGAINR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCBADR) *TPCV1P0_Reader::getBankTPCBADR(int sector, int rb, int mz)
{
  return NULL;
}

