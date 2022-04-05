/***************************************************************************
 * $Id: FTPV1P0.cxx,v 1.6 2007/12/24 06:04:13 fine Exp $
 * Author: M.J. LeVine, J.Klay, H.Huemmler
 ***************************************************************************
 * Description:  FTPV1P0 implementation
 *      
 *
 *   change log
 *
 * JLK 11-Jul-2000 Added new geometry files to correctly navigate banks
 ***************************************************************************
 * $Log: FTPV1P0.cxx,v $
 * Revision 1.6  2007/12/24 06:04:13  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.5  2001/06/27 22:05:11  jcs
 * Comment out unsed variable rcvb
 *
 * Revision 1.4  2001/06/25 22:57:22  jcs
 * add correction for FTPC sector handling
 *
 * Revision 1.3  2001/06/19 20:51:21  jeromel
 * Commited for Janet S.
 *
 * Revision 1.2  2000/08/18 15:38:58  ward
 * New FTPC stuff from JKlay and Hummler.
 *
 * Revision 1.2  2000/07/11 16:07:58 jklay
 * Added new include file which properly translates the global sector 
 * numbering 0-59 to the physical mapping as per 
 * http://wwwstar.mppmu.mpg.de/map/mapping.html
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

#include "FTPV1P0.hh"
#include "azim_to_rcvb.h"

using namespace OLDEVP;

FTPV1P0_PADK_SR::FTPV1P0_PADK_SR(int s, FTPV1P0_Reader *det)
{
  sector = s;
  detector = det;
}

int FTPV1P0_PADK_SR::initialize()
{

  // zero out lookup array  
  memset((char *)packed_address, 0, sizeof(packed_address));


  FTPPADK_entry ent;

  classname(Bank_FTPPADK) *raw_bank = detector->getBankFTPPADK(sector);
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
		
         // convert to correct FTPC software padrow
         padrow = ((int)(padrow-1)/6)%2+1;
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
//   printf("get row %d pad %d address %d\n", padrow, pad, packed_address[padrow][pad]);
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
  return NULL;
}

CPPReader *FTPV1P0_Reader::getCPPReader(int sector)
{
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
  return NULL;
}

ConfigReader *FTPV1P0_Reader::getConfigReader(int sector)
{
  return NULL;
}

FTPV1P0_PADK_SR *FTPV1P0_Reader::getPADKReader(int sector)
{
  
  FTPV1P0_PADK_SR *p;

  if ((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//  p = padk[sector];
  p = padk[sector-1];     //JCS
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
//  padk[sector] = p;
  padk[sector-1] = p;
  return p;
}

FTPV1P0_Reader::FTPV1P0_Reader(EventReader *er, classname(Bank_FTPP) *pftp)
{
  pBankFTPP = pftp;  // copy arg into class variable
  ercpy = er; // squirrel away pointer eventreader for our friends

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
// ---- These NAVIGATE to the raw banks
// ----------------------------------------------------- 

classname(Bank_FTPCHAP) *FTPV1P0_Reader::getBankFTPCHAP(int sector)
{
  if((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//  //JLK This is new 11-July-2000 - we now have geometry files
  int chamber = sector_map[sector-1][0]; //convert to internal...
  if((chamber < 0) || (chamber >= 2))  //check...
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//  int chamber = sector;
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
 
//  ptr->print();
 
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

  //JLK This is new 11-July-2000 - we now have geometry files  
  int rcvb = sector_map[sector-1][1]; if(rcvb>9) rcvb-=10;
  if ((rcvb < 0) || (rcvb >= 10))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//   printf("getBankFTPRBP RB: %d\n",rcvb);
//   chap->print();

//  cout << "getBankFTPRBP RB Offset: " << chap->RcvBoard[rcvb].offset << endl;
//  cout << "getBankFTPRBP RB Length: " << chap->RcvBoard[rcvb].length << endl;
  if ((!chap->RcvBoard[rcvb].offset) ||
      (!chap->RcvBoard[rcvb].length) )
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPRBP) *ptr = (classname(Bank_FTPRBP) *)
                     (((INT32 *)chap) + 
		      chap->RcvBoard[rcvb].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

//  ptr->print();
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
  //JLK This is new 11-July-2000 - we now have geometry files
//  int rcvb = sector_map[sector-1][1];     //JCS - apparently unused
  int intsec = sector_map[sector-1][2];

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
 
//  ptr->print();
  return ptr;
}

classname(Bank_FTPMZP) *FTPV1P0_Reader::getBankFTPMZP(int sector, 
	classname(Bank_FTPRBP) *azip)
//							classname(Bank_FTPAZIP) *azip)
{

  if ((sector <= 0) || (sector > 60))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

  //JLK This is new 11-July-2000 - we now have geometry files
//  int rcvb = sector_map[sector-1][1];     //JCS - apparently unused
  int mz = sector_map[sector-1][2];  //SAME as rcvbsector
  if ((mz < 0) || (mz >= 3))
  {
    pERROR(ERR_BAD_ARG);
    return NULL;
  }

//  cout << "getBankFTPMZP Mezz Offset: " << azip->Mz[mz].offset << endl;
//  cout << "getBankFTPMZP Mezz Length: " << azip->Mz[mz].length << endl;
//  if ((!azip->Mz[mz].offset) || (!azip->Mz[mz].length))
//  cout << "getBankFTPMZP Mezz Offset: " << azip->Sector[mz].offset << endl;
//  cout << "getBankFTPMZP Mezz Length: " << azip->Sector[mz].length << endl;
  if ((!azip->Sector[mz].offset) || (!azip->Sector[mz].length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPMZP) *ptr = (classname(Bank_FTPMZP) *)
                     (((INT32 *)azip) +
  		      azip->Sector[mz].offset);
//      azip->Mz[mz].offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

//  printf("getBankFTPMZP Mezz: %d\n",mz);
//  ptr->print();

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

//classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector,azip);
  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector,rbp);
  if(!mzp) 
  {
    return NULL;
  } else return mzp;
}

classname(Bank_FTPADCD) *FTPV1P0_Reader::getBankFTPADCD(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPADCD.offset) || (!mzp->FTPADCD.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPADCD) *ptr = (classname(Bank_FTPADCD) *)
                      (((INT32 *)mzp) +
                       mzp->FTPADCD.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_FTPSEQD) *FTPV1P0_Reader::getBankFTPSEQD(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPSEQD.offset) || (!mzp->FTPSEQD.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPSEQD) *ptr = (classname(Bank_FTPSEQD) *)
                      (((INT32 *)mzp) +
                       mzp->FTPSEQD.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_FTPADCX) *FTPV1P0_Reader::getBankFTPADCX(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
  if(!mzp) return NULL;

  if((!mzp->FTPADCX.offset) || (!mzp->FTPADCX.length))
  { 
    pERROR(ERR_BANK); 
    return NULL; 
  }

  classname(Bank_FTPADCX) *ptr = (classname(Bank_FTPADCX) *)
                      (((INT32 *)mzp) +
                       mzp->FTPADCX.offset);

  if(!ptr->test_CRC()) { pERROR(ERR_CRC); return NULL; }
  if(ptr->swap() < 0) { pERROR(ERR_SWAP); return NULL; }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_FTPPADK) *FTPV1P0_Reader::getBankFTPPADK(int sector)
{
  errnum = 0;
  errstr0[0] = '\0';

  classname(Bank_FTPMZP) *mzp = getBankFTPMZP(sector);
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
  cout << "warning: FTPV1P0_Reader::getBankFTPGAINR() not implemented!"<< endl;
  return NULL;
}

classname(Bank_FTPBADR) *FTPV1P0_Reader::getBankFTPBADR(int sector)
{
  cout << "warning: FTPV1P0_Reader::getBankFTPBADR() not implemented!"<< endl;
  return NULL;
}
