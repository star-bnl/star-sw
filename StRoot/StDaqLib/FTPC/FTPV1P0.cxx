/***************************************************************************
 * $Id: FTPV1P0.cxx,v 1.1 2000/01/18 18:01:19 levine Exp $
 * Author: M.J. LeVine, J.Klay, H.Huemmler
 ***************************************************************************
 * Description:  FTPV1P0 implementation
 *      
 *
 *   change log
 *
 ***************************************************************************
 * $Log: FTPV1P0.cxx,v $
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

#include "FTPV1P0.hh"

FTPV1P0_PADK_SR::FTPV1P0_PADK_SR(int s, FTPV1P0_Reader *det)
{
  sector = s;
  detector = det;
}

int FTPV1P0_PADK_SR::initialize()
{
  //cout << "Initializing PADK sector: " << sector << endl;

  // zero out lookup array  
  memset((char *)packed_address, 0, sizeof(packed_address));

  // cout << "Sizeof() = " << sizeof(packed_address) << endl;

  FTPPADK_entry ent;

  classname(Bank_FTPPADK) *raw_bank = detector->getBankFTPPADK(sector);
//  cout << "RAW_BANK " << raw_bank << endl;
  if(!raw_bank)
  {
//    printf("No PADK DATA, sector %d\n",sector);
//    printf("ERR: %s\n",detector->errstr0);
  }
  else
  {
//    printf("PADK DATA for sector %d\n",sector);
      for(int i=0; i < FTP_MZPADS; i++)
      {
         int padrow = raw_bank->index[i].pad_row;
         int pad = raw_bank->index[i].pad;
         if((padrow == 0xFF) && (pad == 0xFF)) continue;
		  
         ent.offset = i;
		
         place(padrow, pad, &ent);
      }
       
  }

  return TRUE;
}

void FTPV1P0_PADK_SR::place(short padrow, short pad, FTPPADK_entry *p)
{
  padrow--; pad--;  // use c standard for array
  packed_address[padrow][pad] = pack(p->offset);
}

void FTPV1P0_PADK_SR::get(short padrow, short pad, FTPPADK_entry *p)
{
  padrow--; pad--;  // use c standard for array
  unpack(p,packed_address[padrow][pad]);
}

short FTPV1P0_PADK_SR::pack(short offset)
{
  short p = offset;  // 10 bits
  return p;
} 

void FTPV1P0_PADK_SR::unpack(FTPPADK_entry *entry, short paddress)
{
  entry->offset = paddress & 0x03FF;
}

ZeroSuppressedReader *FTPV1P0_Reader::getZeroSuppressedReader(int sector)
{
  cout << "getFTPV1P0_ZS_SR" << endl;
  
  FTPV1P0_ZS_SR *zs = new FTPV1P0_ZS_SR(sector, this);
  if(!zs->initialize())
  {
    delete zs;
    zs = NULL;
  }
  return (ZeroSuppressedReader *)zs;
}

ADCRawReader *FTPV1P0_Reader::getADCRawReader(int sector)
{
//  cout << "getFTPV1P0_ADCR_SR" << endl;
  FTPV1P0_ADCR_SR *adc = new FTPV1P0_ADCR_SR(sector, this);
  if(!adc->initialize())
  {
    delete adc;
    adc = NULL;
  }
  return (ADCRawReader *)adc;
}

PedestalReader *FTPV1P0_Reader::getPedestalReader(int sector)
{
  //  cout << "getFTPV1P0_PEDR_SR" << endl;
  FTPV1P0_PEDR_SR *ped = new FTPV1P0_PEDR_SR(sector, this);
  if(!ped->initialize())
  {
    delete ped;
    ped = NULL;
  }

  return (PedestalReader *)ped;
}

PedestalRMSReader *FTPV1P0_Reader::getPedestalRMSReader(int sector)
{
  //  cout << "getFTPV1P0_PRMS_SR" << endl;
  FTPV1P0_PRMS_SR *rms = new FTPV1P0_PRMS_SR(sector, this);
  if(!rms->initialize())
  {
    delete rms;
    rms = NULL;
  }

  return (PedestalRMSReader *)rms;
}

GainReader *FTPV1P0_Reader::getGainReader(int sector)
{
  cout << "getFTPV1P0_G_SR" << endl;
  return NULL;
}

CPPReader *FTPV1P0_Reader::getCPPReader(int sector)
{
  //  cout << "getFTPV1P0_CPP_SR" << endl;
  FTPV1P0_CPP_SR *cpp = new FTPV1P0_CPP_SR(sector, this);
  if(!cpp->initialize())
  {
    delete cpp;
    cpp = NULL;
  }

  return (CPPReader *)cpp;
}

BadChannelReader *FTPV1P0_Reader::getBadChannelReader(int sector)
{
  cout << "getFTPV1P0_BC_SR" << endl;
  return NULL;
}

ConfigReader *FTPV1P0_Reader::getConfigReader(int sector)
{
  cout << "getFTPV1P0_CR_SR" << endl;
  return NULL;
}

FTPV1P0_PADK_SR *FTPV1P0_Reader::getPADKReader(int sector)
{
//  cout << "GetPADKReader" << endl;
  
  FTPV1P0_PADK_SR *p;
  p = padk[sector];
  if(p == NULL)
  {
    p = new FTPV1P0_PADK_SR(sector, this);
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

FTPV1P0_Reader::FTPV1P0_Reader(EventReader *er, classname(Bank_FTPP) *pftp)
{
  pBankFTPP = pftp;  // copy arg into class variable

  if (!pBankFTPP->test_CRC()) ERROR(ERR_CRC);
  if (pBankFTPP->swap() < 0) ERROR(ERR_SWAP);
  pBankFTPP->header.CRC = 0;

  // We can have a padk for each of 60 sectors
  for(int i=0;i<FTP_SECTORS;i++)
  {
    padk[i] = NULL;
  }
}

FTPV1P0_Reader::~FTPV1P0_Reader()
{
  //  cout << "FTPV1P0 destructor" << endl;
  
  // Delete Sector Readers buffers (The actual readers are deleted by client)
  
  // Delete PADK's
  for(int i=0;i<FTP_SECTORS;i++)
  {
    if(padk[i] != NULL) delete padk[i];
  }   
}

int FTPV1P0_Reader::MemUsed()
{
  return 0;
}

// -----------------------------------------------------
// Here lie bank retrieval functions
// ---- These NAVAGATE to the raw banks
// ----------------------------------------------------- 

classname(Bank_FTPCHAP) *FTPV1P0_Reader::getBankFTPCHAP(int sector)
{
  if((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  int chamber = (sector/31); //convert to internal...
  if((chamber < 0) || (chamber >= 2))  //check...
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  if((!pBankFTPP->Chamber[chamber].offset) ||
     (!pBankFTPP->Chamber[chamber].length))
  {
    pERROR(ERR_BANK);
    return NULL;
  }

  classname(Bank_FTPCHAP) *ptr = (classname(Bank_FTPCHAP) *)
                      (((INT32 *)pBankFTPP) + 
			pBankFTPP->Chamber[chamber].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;
 
  return ptr;
}

classname(Bank_FTPRBP) *FTPV1P0_Reader::getBankFTPRBP(int sector, 
					   classname(Bank_FTPCHAP) *chap)
{
  if ((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  int rcvb = ((sector-1)/3); if(rcvb>9) rcvb-=10;
  if ((rcvb < 0) || (rcvb >= 10))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//   printf("getBankFTPRBP RB: %d\n",rcvb);
//   chap->print();

  if ((!chap->RcvBoard[rcvb].offset) ||
      (!chap->RcvBoard[rcvb].length) )
  { 
    //    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPRBP) *ptr = (classname(Bank_FTPRBP) *)
                     (((INT32 *)chap) + 
		      chap->RcvBoard[rcvb].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_FTPAZIP) *FTPV1P0_Reader::getBankFTPAZIP(int sector, 
						classname(Bank_FTPRBP) *rbp)
{
  if((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }
  int rcvb = (sector-1)/3;
  int intsec = (sector - ((rcvb*3) + 1));

  if((intsec < 0) || (intsec >= 3))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  if((!rbp->Sector[intsec].offset) ||
     (!rbp->Sector[intsec].length))
  {
    pERROR(ERR_BANK);
    return NULL;
  }

  classname(Bank_FTPAZIP) *ptr = (classname(Bank_FTPAZIP) *)
                      (((INT32 *)rbp) + 
			rbp->Sector[intsec].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;
 
  return ptr;
}

classname(Bank_FTPMZP) *FTPV1P0_Reader::getBankFTPMZP(int sector, 
						      classname(Bank_FTPAZIP) *azip)
{

  if ((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  int rcvb = ((sector-1)/3);
  int mz = (sector - ((rcvb*3) + 1));
  if ((mz < 0) || (mz >= 3))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  if ((!azip->Mz[mz].offset) || (!azip->Mz[mz].length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPMZP) *ptr = (classname(Bank_FTPMZP) *)
                     (((INT32 *)azip) +
		      azip->Mz[mz].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

//    printf("getBankFTPMZP Mezz: %d\n",mz);
//    ptr->print();

  return ptr;
}

classname(Bank_FTPMZP) *FTPV1P0_Reader::getBankFTPMZP(int sector)
{
  if ((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  classname(Bank_FTPCHAP) *chap = getBankFTPCHAP(sector);
  if(!chap) return NULL;
  
  classname(Bank_FTPRBP) *rbp = getBankFTPRBP(sector, chap);
  if(!rbp) return NULL;

  classname(Bank_FTPAZIP) *azip = getBankFTPAZIP(sector, rbp);
  if(!azip) return NULL;

  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector,azip);
//  cout << "inside getBankMZP " << mzp << endl;
  if(!mzp) 
  {
    return NULL;
  } else return mzp;
}

classname(Bank_FTPADCD) *FTPV1P0_Reader::getBankFTPADCD(int sector)
{
  return NULL;
}

classname(Bank_FTPSEQD) *FTPV1P0_Reader::getBankFTPSEQD(int sector)
{
  return NULL;
}

classname(Bank_FTPADCX) *FTPV1P0_Reader::getBankFTPADCX(int sector)
{
  return NULL;
}

classname(Bank_FTPPADK) *FTPV1P0_Reader::getBankFTPPADK(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';

//  cout << "getBankFTPPADK\n";
  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
//  cout << "MZP " << mzp << endl;
  if(!mzp) return NULL;

//   printf(" sector: %d\n",sector);
//   mzp->print();

  if((!mzp->FTPPADK.offset) || (!mzp->FTPPADK.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPPADK) *ptr = (classname(Bank_FTPPADK) *)
                      (((INT32 *)mzp) +
		       mzp->FTPPADK.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
};

classname(Bank_FTPCPPR) *FTPV1P0_Reader::getBankFTPCPPR(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPCPPR.offset) || (!mzp->FTPCPPR.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPCPPR) *ptr = (classname(Bank_FTPCPPR) *)
                      (((INT32 *)mzp) +
			mzp->FTPCPPR.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_FTPADCR) *FTPV1P0_Reader::getBankFTPADCR(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';

//  cout << "getBankFTPADCR\n";
  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPADCR.offset) || (!mzp->FTPADCR.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPADCR) *ptr = (classname(Bank_FTPADCR) *)
                      (((INT32 *)mzp) +
			mzp->FTPADCR.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_FTPCFGR) *FTPV1P0_Reader::getBankFTPCFGR(int sector)
{
  return NULL;
}

classname(Bank_FTPPEDR) *FTPV1P0_Reader::getBankFTPPEDR(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPPEDR.offset) || (!mzp->FTPPEDR.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPPEDR) *ptr = (classname(Bank_FTPPEDR) *)
                      (((INT32 *)mzp) +
			mzp->FTPPEDR.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;      

}

classname(Bank_FTPRMSR) *FTPV1P0_Reader::getBankFTPRMSR(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPRMSR.offset) || (!mzp->FTPRMSR.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPRMSR) *ptr = (classname(Bank_FTPRMSR) *)
                      (((INT32 *)mzp) +
			mzp->FTPRMSR.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;       

}

classname(Bank_FTPGAINR) *FTPV1P0_Reader::getBankFTPGAINR(int sector)
{
  return NULL;
}

classname(Bank_FTPBADR) *FTPV1P0_Reader::getBankFTPBADR(int sector)
{
  return NULL;
}
