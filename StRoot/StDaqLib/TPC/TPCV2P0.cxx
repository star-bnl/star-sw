// TPCV2P0 implementation
#include "TPCV2P0.hh"

// change log
// 02-Jun-99 MJL fixed test on hypersector arg of getBankTPCSECP
// 11-Jun-99 MJL merged PEDR PedRMS readers from RAW version

TPCV2P0_PADK_SR::TPCV2P0_PADK_SR(int s, TPCV2P0_Reader *det)
{
  sector = s;
  detector = det;
}

int TPCV2P0_PADK_SR::initialize()
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
	{// missing PADK bank
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

void TPCV2P0_PADK_SR::place(short padrow, short pad, PADK_entry *p)
{
  padrow--; pad--;  // use c standard for array
  packed_address[padrow][pad] = pack(p->rb, p->mz, p->offset);
}

void TPCV2P0_PADK_SR::get(short padrow, short pad, PADK_entry *p)
{
  padrow--; pad--;  // use c standard for array
  unpack(p,packed_address[padrow][pad]);
}

short TPCV2P0_PADK_SR::pack(short rcb, short mz, short offset)
{
  short p = rcb;  // 4 bits 
  p = p << 2;  
  p += mz;      // 2 bits
  p = p << 10;
  p += offset;  // 10 bits
  return p;
} 

void TPCV2P0_PADK_SR::unpack(PADK_entry *entry, short paddress)
{
  entry->offset = paddress & 0x03FF;
  entry->mz = (paddress & 0x0C00) >> 10;
  entry->rb = paddress >> 12;
}

ZeroSuppressedReader *TPCV2P0_Reader::getZeroSuppressedReader(int sector)
{
  cout << "getTPCV2P0_ZS_SR sector(" << sector <<")" << endl;
  
  TPCV2P0_ZS_SR *zsp = new TPCV2P0_ZS_SR(sector, this);
  if(!zsp->initialize())
  {
  cout << "ERROR: getTPCV2P0_ZS_SR FAILED sector(" << sector <<")" << endl;
    delete zsp;
    zsp = NULL;
  }

  return (ZeroSuppressedReader *)zsp;
}

ADCRawReader *TPCV2P0_Reader::getADCRawReader(int sector)
{
  //  cout << "getTPCV2P0_ADCR_SR" << endl;
  TPCV2P0_ADCR_SR *adc = new TPCV2P0_ADCR_SR(sector, this);
  if(!adc->initialize())
  {
    delete adc;
    adc = NULL;
  }

  return (ADCRawReader *)adc;
}

PedestalReader *TPCV2P0_Reader::getPedestalReader(int sector)
{
  //  cout << "getTPCV2P0_P_SR" << endl;
  TPCV2P0_PEDR_SR *ped = new TPCV2P0_PEDR_SR(sector, this);
  if(!ped->initialize())
    {
      delete ped;
      ped = NULL;
    }

  return (PedestalReader *)ped;
}

PedestalRMSReader *TPCV2P0_Reader::getPedestalRMSReader(int sector)
{
  // cout << "getTPCV2P0_PRMS_SR" << endl;
  TPCV2P0_PRMS_SR *rms = new TPCV2P0_PRMS_SR(sector, this);
  if(!rms->initialize())
    {
      delete rms;
      rms = NULL;
    }
  
  return (PedestalRMSReader *)rms;
}

GainReader *TPCV2P0_Reader::getGainReader(int sector)
{
  cout << "getTPCV2P0_G_SR" << endl;
  return NULL;
}

CPPReader *TPCV2P0_Reader::getCPPReader(int sector)
{
  //  cout << "getTPCV2P0_CPP_SR" << endl;
  TPCV2P0_CPP_SR *cpp = new TPCV2P0_CPP_SR(sector, this);
  if(!cpp->initialize())
  {
    delete cpp;
    cpp = NULL;
  }

  return (CPPReader *)cpp;}

BadChannelReader *TPCV2P0_Reader::getBadChannelReader(int sector)
{
  cout << "getTPCV2P0_BC_SR" << endl;
  return NULL;
}

ConfigReader *TPCV2P0_Reader::getConfigReader(int sector)
{
  cout << "getTPCV2P0_CR_SR" << endl;
  return NULL;
}

TPCV2P0_PADK_SR *TPCV2P0_Reader::getPADKReader(int sector)
{
  //  cout << "GetPADKReader" << endl;
  
  TPCV2P0_PADK_SR *p;
  p = padk[sector];
  if(p == NULL)
  {
    p = new TPCV2P0_PADK_SR(sector, this);
    if(!p->initialize())
    {
      cout << "Error Reading PADK banks, sector=" << sector 
	   << ": " << errstr() << endl;
      delete p;
      return NULL;
    }
  }
  padk[sector] = p;
  return p;
}

TPCV2P0_Reader::TPCV2P0_Reader(EventReader *er)
{
  //  cout << "TPCV2P0 constructor" << endl;

  // Fix up DATAP
  pBankDATAP = (Bank_DATAP *)er->getDATAP();
  //  printf("pBankDATAP.head: %s\n",pBankDATAP->header.BankType);

  if (!pBankDATAP->test_CRC()) ERROR(ERR_CRC);
  if (pBankDATAP->swap() < 0) ERROR(ERR_SWAP);
  pBankDATAP->header.CRC = 0;

  // Fix up TPCP
  pBankTPCP = (classname(Bank_TPCP) *)
              (((INT32 *)pBankDATAP) + pBankDATAP->TPC.offset);
  //  printf("Offset = %d\n",pBankDATAP->TPC.offset);
  //  printf("pBankTPCP.head: %s\n",pBankTPCP->header.BankType);
  if (!pBankTPCP->test_CRC()) ERROR(ERR_CRC);
  if (pBankTPCP->swap() < 0) ERROR(ERR_SWAP);
  pBankTPCP->header.CRC = 0;

  // We can have a padk for each of 24 sectors
  for(int i=0;i<TPC_SECTORS;i++)
  {
    padk[i] = NULL;
  }
}

TPCV2P0_Reader::~TPCV2P0_Reader()
{
  //  cout << "TPCV2P0 destructor" << endl;
  
  // Delete Sector Readers buffers (The actual readers are deleted by client)
  
  // Delete PADK's
  for(int i=0;i<TPC_SECTORS;i++)
  {
    if(padk[i] != NULL) delete padk[i];
  }   
}

int TPCV2P0_Reader::MemUsed()
{
  return 0;
}

// -----------------------------------------------------
// Here lie bank retrieval functions
// ---- These NAVAGATE to the raw banks
// ----------------------------------------------------- 

classname(Bank_TPCSECP) *TPCV2P0_Reader::getBankTPCSECP(int hypersector)
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

classname(Bank_TPCRBP) *TPCV2P0_Reader::getBankTPCRBP(int interleaved_rb, 
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

classname(Bank_TPCMZP) *TPCV2P0_Reader::getBankTPCMZP(int mz, classname(Bank_TPCRBP) *rbp)
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

classname(Bank_TPCMZP) *TPCV2P0_Reader::getBankTPCMZP(int sector, int rb, int mz)
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

classname(Bank_TPCADCD) *TPCV2P0_Reader::getBankTPCADCD(int sector, int rb, int mz)
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

classname(Bank_TPCSEQD) *TPCV2P0_Reader::getBankTPCSEQD(int sector, int rb, int mz)
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

classname(Bank_TPCADCX) *TPCV2P0_Reader::getBankTPCADCX(int sector, int rb, int mz)
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

classname(Bank_TPCPADK) *TPCV2P0_Reader::getBankTPCPADK(int sector, int rb, int mz)
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

classname(Bank_TPCCPPR) *TPCV2P0_Reader::getBankTPCCPPR(int sector, int rb, int mz)
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

classname(Bank_TPCADCR) *TPCV2P0_Reader::getBankTPCADCR(int sector, int rb, int mz)
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

classname(Bank_TPCCFGR) *TPCV2P0_Reader::getBankTPCCFGR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCPEDR) *TPCV2P0_Reader::getBankTPCPEDR(int sector, int rb, int mz)
{
  return NULL;
}


classname(Bank_TPCRMSR) *TPCV2P0_Reader::getBankTPCRMSR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCGAINR) *TPCV2P0_Reader::getBankTPCGAINR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCBADR) *TPCV2P0_Reader::getBankTPCBADR(int sector, int rb, int mz)
{
  return NULL;
}

