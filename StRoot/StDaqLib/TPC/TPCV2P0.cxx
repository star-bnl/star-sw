/***************************************************************************
 * $Id: TPCV2P0.cxx,v 1.11 2007/12/24 06:04:32 fine Exp $
 * Author: Jeff Landgraf and M.J. LeVine
 ***************************************************************************
 * Description: common TPC (V2) implementation stuff
 *      
 *
 *   change log
 * 02-Jun-99 MJL fixed test on hypersector arg of getBankTPCSECP
 * 11-Jun-99 MJL merged PEDR PedRMS readers from RAW version
 * 23-Jun-99 MJL most output now supressed with EventReader.verbose
 * 23-Jun-99 MJL can now navigate DATAP even though offset/len to various
 *   lower banks are at arbitrary positions
 * 09-Jul-99 MJL removed navigation code from TPC_Reader. Introduced
 *               Bank_TPCP argument to TPCV2P0_Reader constructor
 * 20-Jul-99 MJL add error logging. Add bank type checking for every getBank...()
 * 27-Jul-99 MJL implement TPCV2P0_Reader::getBankTPCPEDR()
 * 27-Jul-99 MJL implement TPCV2P0_Reader::getBankTPCRMSR()
 *
 ***************************************************************************
 * $Log: TPCV2P0.cxx,v $
 * Revision 1.11  2007/12/24 06:04:32  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.10  2004/03/04 21:51:29  ward
 * Replaced MERGE_SEQUENCES with a StDAQMaker chain parameter, as suggested by Landgraf and Lauret.
 *
 * Revision 1.9  2002/10/13 20:43:37  ward
 * Support for decoding DAQ100 data and writing it into a table.
 *
 * Revision 1.8  2000/01/11 22:03:44  levine
 * convert string to char* via c_str() member
 * (from Brian Lasiuk)
 *
 * Revision 1.7  1999/07/27 23:19:32  levine
 * implemented methods TPCV2P0_Reader::getBankTPCPEDR()
 *                     TPCV2P0_Reader::getBankTPCRMSR()
 *
 * Revision 1.6  1999/07/21 21:32:39  levine
 *
 *
 * changes to include error logging to file.
 *
 * There are now 2 constructors for EventReader:
 *
 *  EventReader();
 *  EventReader(const char *logfilename);
 *
 * Constructed with no argument, there is no error logging. Supplying a file name
 * sends all diagnostic output to the named file (N.B. opens in append mode)
 *
 * See example in client.cxx for constructing a log file name based on the
 * datafile name.
 *
 * It is strongly advised to use the log file capability. You can grep it for
 * instances of "ERROR:" to trap anything noteworthy (i.e., corrupted data files).
 *
 * Revision 1.5  1999/07/10 21:31:25  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.4  1999/07/03 04:25:48  levine
 * changes to get past Linux cyyyYompiler
 *
 * Revision 1.3  1999/07/02 04:43:24  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch EventReader.verbose
 *  added TPCV2P0_CPP_SR::getAsicParams()
 *
 *
 **************************************************************************/
// TPCV2P0 implementation
#include "TPCV2P0.hh"

using namespace OLDEVP;

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
  char mergeSequences;
  if(sector>=100) { sector-=100; mergeSequences=1; } else mergeSequences=0;
  if (ercpy->verbose) cout << "getTPCV2P0_ZS_SR sector(" << sector <<")" << endl;
  
  TPCV2P0_ZS_SR *zsp = new TPCV2P0_ZS_SR(sector, this, mergeSequences);
  if(!zsp->initialize())
  {
  if (ercpy->verbose) 
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
  if (ercpy->verbose) cout << "getTPCV2P0_G_SR" << endl;
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
  if (ercpy->verbose) cout << "getTPCV2P0_BC_SR" << endl;
  return NULL;
}

ConfigReader *TPCV2P0_Reader::getConfigReader(int sector)
{
  if (ercpy->verbose) cout << "getTPCV2P0_CR_SR" << endl;
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
      if (ercpy->verbose) cout << "Error Reading PADK banks, sector=" << sector 
	   << ": " << errstr().c_str() << endl;
      delete p;
      return NULL;
    }
  }
  padk[sector] = p;
  return p;
}

TPCV2P0_Reader::TPCV2P0_Reader(EventReader *er, classname(Bank_TPCP) *ptpc)
{
  pBankTPCP = ptpc; // copy pointer into class variable
  motherPointerBank=pBankTPCP; // Herb Oct 2002 for DAQ100.
  ercpy = er; // squirrel away pointer eventreader for our friends

  if (!pBankTPCP->test_CRC()) ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,"TPCP");
  if (pBankTPCP->swap() < 0) ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,"TPCP");
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
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,"TPCSECP");
    return NULL;
  }
  hypersector--; //convert to internal represenation

  if((!pBankTPCP->HyperSector[hypersector].offset) ||
     (!pBankTPCP->HyperSector[hypersector].length))
  {
    char str0[40];
    sprintf(str0,"getBankTPCSECP(hs %d)",hypersector);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0);
    return NULL;
  }

  classname(Bank_TPCSECP) *ptr = (classname(Bank_TPCSECP) *)
                      (((INT32 *)pBankTPCP) + 
			pBankTPCP->HyperSector[hypersector].offset);

  if(strncmp(ptr->header.BankType,"TPCSECP",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCSECP(hs %d)",hypersector);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__, str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankTPCSECP(hs %d)",hypersector);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCSECP(hs %d)",hypersector);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;
 
  return ptr;
}

classname(Bank_TPCRBP) *TPCV2P0_Reader::getBankTPCRBP(int interleaved_rb, 
					   classname(Bank_TPCSECP) *secp)
{
  int sector = secp->header.BankId;
  if ((interleaved_rb < 0) || (interleaved_rb >= 12))
  {
    char str0[40];
    sprintf(str0,"getBankTPCRBP(sec %d rb %d )",sector,interleaved_rb);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
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

  if(strncmp(ptr->header.BankType,"TPCRBP",6)) {
    char str0[40];
    sprintf(str0,"getBankTPCRBP(sec %d rb %d )",sector,interleaved_rb);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankTPCRBP(sec %d rb %d )",sector,interleaved_rb);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankTPCRBP(sec %d rb %d )",sector,interleaved_rb);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;
}

classname(Bank_TPCMZP) *TPCV2P0_Reader::getBankTPCMZP(int mz, classname(Bank_TPCRBP) *rbp)
{
  int rb = rbp->header.BankId;
  if ((mz < 0) || (mz >= 3))
  {
    char str0[40];
    sprintf(str0,"getBankTPCMZP(rb %d  mz %d )",rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }

  if ((!rbp->Mz[mz].offset) || (!rbp->Mz[mz].length))
  { 
    char str0[40];
    sprintf(str0,"getBankTPCMZP(rb %d  mz %d )",rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCMZP) *ptr = (classname(Bank_TPCMZP) *)
                     (((INT32 *)rbp) +
		      rbp->Mz[mz].offset);

  if(strncmp(ptr->header.BankType,"TPCMZP",6)) {
    char str0[40];
    sprintf(str0,"getBankTPCMZP(rb %d  mz %d )",rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankTPCMZP(rb %d  mz %d )",rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCMZP(rb %d  mz %d )",rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

//    printf("getBankTPCMZP Mezz: %d\n",mz);
//    ptr->print();

  return ptr;
}

classname(Bank_TPCMZP) *TPCV2P0_Reader::getBankTPCMZP(int sector, int rb, int mz)
{
  if ((sector < 0) || (sector >= TPC_SECTORS))
  {
    char str0[40];
    sprintf(str0,"getBankTPCMZP(sec %d, rb %d, mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }
  if ((rb < 0) || (rb >= 6))
  {
    char str0[40];
    sprintf(str0,"getBankTPCMZP(sec %d, rb %d, mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
    return NULL;
  }
  if ((mz < 0) || (mz >= 3))
  {
    char str0[40];
    sprintf(str0,"getBankTPCMZP(sec %d, rb %d, mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_ARG,__FILE__,__LINE__,str0);
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
    char str0[40];
    sprintf(str0,"getBankTPCADCD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCADCD) *ptr = (classname(Bank_TPCADCD) *)
                      (((INT32 *)mzp) +
		       mzp->TPCADCD.offset);

  if(strncmp(ptr->header.BankType,"TPCADCD",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCADCD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankTPCADCD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCADCD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
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
    char str0[40];
    sprintf(str0,"getBankTPCSEQD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCSEQD) *ptr = (classname(Bank_TPCSEQD) *)
                      (((INT32 *)mzp) +
		       mzp->TPCSEQD.offset);

  if(strncmp(ptr->header.BankType,"TPCSEQD",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCSEQD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankTPCSEQD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCSEQD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
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
    char str0[40];
    sprintf(str0,"getBankTPCADCX(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCADCX) *ptr = (classname(Bank_TPCADCX) *)
                      (((INT32 *)mzp) +
		       mzp->TPCADCX.offset);

  if(strncmp(ptr->header.BankType,"TPCADCX",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCADCX(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankTPCADCX(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankTPCADCX(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
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
    char str0[40];
    sprintf(str0,"getBankTPCPADK(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCPADK) *ptr = (classname(Bank_TPCPADK) *)
                      (((INT32 *)mzp) +
		       mzp->TPCPADK.offset);

  if(strncmp(ptr->header.BankType,"TPCPADK",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCPADK(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankTPCPADK(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankTPCPADK(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
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
    char str0[40];
    sprintf(str0,"getBankTPCCPPR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCCPPR) *ptr = (classname(Bank_TPCCPPR) *)
                      (((INT32 *)mzp) +
			mzp->TPCCPPR.offset);

  if(strncmp(ptr->header.BankType,"TPCCPPR",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCCPPR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankTPCCPPR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCCPPR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
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
    char str0[40];
    sprintf(str0,"getBankTPCADCR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCADCR) *ptr = (classname(Bank_TPCADCR) *)
                      (((INT32 *)mzp) +
			mzp->TPCADCR.offset);

  if(strncmp(ptr->header.BankType,"TPCADCR",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCADCR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) {
    char str0[40];
    sprintf(str0,"getBankTPCADCR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) {
    char str0[40];
    sprintf(str0,"getBankTPCADCR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_TPCMZCLD) *TPCV2P0_Reader::getBankTPCMZCLD(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->TPCMZCLD.offset) || (!mzp->TPCMZCLD.length))
  { 
    char str0[40];
    sprintf(str0,"getBankTPCMZCLD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCMZCLD) *ptr = (classname(Bank_TPCMZCLD) *)
                      (((INT32 *)mzp) +
			mzp->TPCMZCLD.offset);

  if(strncmp(ptr->header.BankType,"TPCMZCLD",8)) {
    char str0[40];
    sprintf(str0,"getBankTPCMZCLD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankTPCMZCLD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCMZCLD(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}


classname(Bank_TPCCFGR) *TPCV2P0_Reader::getBankTPCCFGR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCPEDR) *TPCV2P0_Reader::getBankTPCPEDR(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->TPCPEDR.offset) || (!mzp->TPCPEDR.length))
  { 
    char str0[40];
    sprintf(str0,"getBankTPCPEDR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCPEDR) *ptr = (classname(Bank_TPCPEDR) *)
                      (((INT32 *)mzp) +
			mzp->TPCPEDR.offset);

  if(strncmp(ptr->header.BankType,"TPCPEDR",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCPEDR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankTPCPEDR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCPEDR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}


classname(Bank_TPCRMSR) *TPCV2P0_Reader::getBankTPCRMSR(int sector, int rb, int mz)
{
  errnum = 0;
  errstr0[0] = '\0';
  
  classname(Bank_TPCMZP) *mzp = getBankTPCMZP(sector, rb, mz);
  if(!mzp) return NULL;

  if((!mzp->TPCRMSR.offset) || (!mzp->TPCRMSR.length))
  { 
    char str0[40];
    sprintf(str0,"getBankTPCRMSR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(INFO_MISSING_BANK,__FILE__,__LINE__,str0); 
    return NULL; 
  }

  classname(Bank_TPCRMSR) *ptr = (classname(Bank_TPCRMSR) *)
                      (((INT32 *)mzp) +
			mzp->TPCRMSR.offset);

  if(strncmp(ptr->header.BankType,"TPCRMSR",7)) {
    char str0[40];
    sprintf(str0,"getBankTPCRMSR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_BAD_HEADER,__FILE__,__LINE__,str0); return NULL; 
  }
  if(!ptr->test_CRC()) { 
    char str0[40];
    sprintf(str0,"getBankTPCRMSR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_CRC,__FILE__,__LINE__,str0); return NULL; 
  }
  if(ptr->swap() < 0) { 
    char str0[40];
    sprintf(str0,"getBankTPCRMSR(sec %d rb %d  mz %d )",sector,rb,mz);
    ercpy->fprintError(ERR_SWAP,__FILE__,__LINE__,str0); return NULL; 
  }
  ptr->header.CRC = 0;

  return ptr;       
}

classname(Bank_TPCGAINR) *TPCV2P0_Reader::getBankTPCGAINR(int sector, int rb, int mz)
{
  return NULL;
}

classname(Bank_TPCBADR) *TPCV2P0_Reader::getBankTPCBADR(int sector, int rb, int mz)
{
  return NULL;
}

