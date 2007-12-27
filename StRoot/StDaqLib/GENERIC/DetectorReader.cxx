/***************************************************************************
 * $Id: DetectorReader.cxx,v 1.24 2007/12/27 21:55:04 perev Exp $
 * Author: Jeff Landgraf
 ***************************************************************************
 * Description:  Detector Factory
 *      
 *
 *   change log
 * 02-Jul-99 MJL add #includes for other detectors
 * 08-Jul-99 MJL separate RICH_Reader
 * 14-Jan-00 HH   add FTPC 
 *
 ***************************************************************************
 * $Log: DetectorReader.cxx,v $
 * Revision 1.24  2007/12/27 21:55:04  perev
 * EEMCreader created even if no EEMC bank. TRG info assumed(Pibero)
 *
 * Revision 1.23  2007/12/24 06:04:16  fine
 * introduce OLDEVP namespace to allow ole and new EVP library concurrently
 *
 * Revision 1.22  2007/11/30 01:22:36  genevb
 * Update for 2008 BEMC from A. Kocoloski
 *
 * Revision 1.21  2007/08/07 19:44:09  perev
 * Gene scalers added
 *
 * Revision 1.20  2007/05/25 13:37:46  jeromel
 * /DEBUG//
 *
 * Revision 1.19  2007/05/24 20:56:38  jeromel
 * (Pointer to) method returns FALSE instead of NULL fixed (+ one debug statement to remove later)
 *
 * Revision 1.18  2004/11/11 20:06:58  jeromel
 * Message format change only (explicit)
 *
 * Revision 1.17  2004/08/07 02:43:32  perev
 *  more test for corruption added
 *
 * Revision 1.16  2004/02/18 20:31:14  ward
 * There was a big mess.  I am trying to fix it.
 *
 * Revision 1.14  2004/02/18 20:17:48  ward
 * Access SSD data in makers.
 *
 * Revision 1.13  2003/03/24 18:12:15  ward
 * Full support for EEMC from Herbert Ward.
 *
 * Revision 1.12  2002/12/09 18:54:23  ward
 * EMC stuff from Subhassis.
 *
 * Revision 1.11  2002/01/17 17:29:26  jeromel
 *
 * Files:  CVS: DetectorReader.cxx EventReader.cxx EventReader.hh CVS: RecHeaderFormats.hh CVS: ----------------------------------------------------------------------
 * Modifications for FPD support
 *
 * Revision 1.10  2001/07/10 18:12:47  jeromel
 * Changes commited for Frank Geurts (TOF) after approval from Herb Ward
 * on Tue, 10 Jul 2001 11:19:48 and review by Victor.
 * Changes implements TOF DAQ Reader.
 *
 * Revision 1.9  2001/06/19 21:07:54  jeromel
 * Activate getFTPCReader (Janet S.)
 *
 * Revision 1.8  2000/06/30 21:51:15  perev
 * L3 stuff added
 *
 * Revision 1.7  2000/06/13 17:29:30  jml
 * Adding L3 Detector reader and template L3 Reader
 *
 * Revision 1.6  2000/06/06 21:46:59  jml
 * Added code for SVTV1P0
 *
 * Revision 1.5  2000/01/18 17:56:08  levine
 * Added FTPC call to TPC-like detectors
 *
 * Revision 1.3  1999/07/26 17:00:02  levine
 * changes to RICH file organization
 *
 * Revision 1.2  1999/07/10 21:31:17  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.1  1999/07/02 22:26:55  levine
 * moved from TPC directory. Now is the factory for all detector readers.
 * Detector groups do not have to touch this file - only add functionality
 * to their own directory tree.
 *
 * Revision 1.2  1999/07/02 04:43:22  levine
 * Many changes -
 *  navigates to head of TPCP bank independent of position.
 *  move declarations out of loops where they were upsetting some compilers
 *  suppress output from class libraries with run-time switch 
 *  EventReader.verbose
 *
 *
 **************************************************************************/
#include "StDaqLib/GENERIC/EventReader.hh"
#include "TPC/TPCV1P0_Reader.hh"
#include "TPC/TPCV2P0_Reader.hh"
#include "TRG/TRG_Reader.hh"
#include "SVT/SVTV1P0_Reader.hh"
#include "SSD/SSD_Reader.hh"
#include "EMC/EMC_Reader.hh"
#include "EEMC/EEMC_Reader.hh"
#include "PMD/PMD_Reader.hh"
#include "RICH/RICH_Reader.hh"
#include "FTPC/FTPV1P0_Reader.hh"
#include "L3/L3_Reader.hh"
#include "TOF/TOF_Reader.hh"
#include "FPD/FPD_Reader.hh"
#include "SC/SC_Reader.hh"

using namespace OLDEVP;

DetectorReader *getDetectorReader(EventReader *er, string det)
{
  DetectorReader *dr;
  // this switch handles all "TPC-like" detectors
  if(det == "TPC")
    {
      Bank_TPCPV1P0  *pTPCP;
      pTPCP = (Bank_TPCPV1P0 *)er->findBank("TPCP");
      if (!pTPCP) dr = NULL;
      else dr =  new TPCV1P0_Reader(er,pTPCP);
      
    }
  else if (det == "TPCV1P0")
    {
      Bank_TPCPV1P0  *pTPCP;
      pTPCP = (Bank_TPCPV1P0 *)er->findBank("TPCP");
      if (!pTPCP) dr = NULL;
      else dr =  new TPCV1P0_Reader(er,pTPCP);
    }
   else if (det == "TPCV2P0")
     {
      Bank_TPCPV2P0  *pTPCP;
      pTPCP = (Bank_TPCPV2P0 *)er->findBank("TPCP");
      if (!pTPCP) dr = NULL;
      else dr =  new TPCV2P0_Reader(er,pTPCP);
     }
  else if  (det == "SVT")
     {
      Bank_SVTPV1P0 *pSVTP;
      pSVTP = (Bank_SVTPV1P0 *)er->findBank("SVTP");
      if (!pSVTP) dr = NULL;
      else dr =  new SVTV1P0_Reader(er,pSVTP);
     }
  else if  (det == "FTPC")
     {
      Bank_FTPPV1P0 *pFTPP;
      pFTPP = (Bank_FTPPV1P0 *)er->findBank("FTPP");
      if (!pFTPP) dr = NULL;
      else dr =  new FTPV1P0_Reader(er,pFTPP);
     }
  else
  {
    dr = NULL;
  }

  return dr;
}

DetectorReader *getFTPCReader(EventReader *er)
{
  string det="FTPC";
  DetectorReader *detReader=getDetectorReader(er,det);
  return detReader;
}

RICH_Reader *getRICHReader(EventReader *er)
{
  Bank_RICP *pRICP;
  pRICP = (Bank_RICP *)er->findBank("RICP");
  if (pRICP)  {
    if (!pRICP->test_CRC())  printf("DetectorReader - getRICHReader: CRC error in RICP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pRICP->swap() < 0)   printf("DetectorReader - getRICHReader: swap error in RICP: %s %d\n",
					__FILE__,__LINE__) ;
    pRICP->header.CRC = 0;
    return new RICH_Reader(er,pRICP);
  }
  return NULL;
}

EEMC_Reader *getEEMCReader(EventReader *er)
{
  Bank_EEMCP *pEEMCP;
  pEEMCP = (Bank_EEMCP *)er->findBank("EECP");
  if (pEEMCP)  {
    if (!pEEMCP->test_CRC())  printf("DetectorReader - getEMCReader: CRC error in EEMCP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pEEMCP->swap() < 0)   printf("DetectorReader - getEMCReader: swap error in EEMCP: %s %d\n",
					__FILE__,__LINE__) ;
    pEEMCP->header.CRC = 0;
  }
  return new EEMC_Reader(er,pEEMCP);
}
SSD_Reader *getSSDReader(EventReader *er)
{
  printf("DetectorReader - getSSDReader: before instantiation of SSD_Reader.\n"); 
  return new SSD_Reader(er);
  return NULL;
}
EMC_Reader *getEMCReader(EventReader *er)
{
  // SMD data is here, also towers for year < 2008
  Bank_EMCP *pEMCP;
  pEMCP = (Bank_EMCP *)er->findBank("EMCP");
  
  // towers are now in trg block
  Bank_TRGP *pTRGP;
  pTRGP = (Bank_TRGP *)er->findBank("TRGP");
    
  if (pEMCP)  {
    if (!pEMCP->test_CRC())  printf("DetectorReader - getEMCReader: CRC error in EMCP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pEMCP->swap() < 0)   printf("DetectorReader - getEMCReader: swap error in EMCP: %s %d\n",
					__FILE__,__LINE__) ;
    pEMCP->header.CRC = 0;
  }
  
  return new EMC_Reader(er,pEMCP,pTRGP);
}
PMD_Reader *getPMDReader(EventReader *er)
{
  Bank_PMDP *pPMDP;
  pPMDP = (Bank_PMDP *)er->findBank("PMDP");
  if (pPMDP)  {
    if (!pPMDP->test_CRC())  printf("DetectorReader - getPMDReader: CRC error in PMDP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pPMDP->swap() < 0)   printf("DetectorReader - getPMDReader: swap error in PMDP: %s %d\n",
					__FILE__,__LINE__) ;
    pPMDP->header.CRC = 0;
    return new PMD_Reader(er,pPMDP);
  }
  return NULL;
}
TRG_Reader *getTRGReader(EventReader *er)
{
  Bank_TRGP *pTRGP;
  pTRGP = (Bank_TRGP *)er->findBank("TRGP");
  //printf("DEBUG In getTRGReader - pointer to TRGP is %p = %s\n",
  //	 (void *) pTRGP,
  //	 pTRGP?"TRUE":"FALSE");
  if (pTRGP)  {
    if (!pTRGP->test_CRC())  {printf("DetectorReader - getTRGReader: CRC error in TRGP: %s %d\n",
					__FILE__,__LINE__) ; return NULL;}
    if (pTRGP->swap() < 0)   {printf("DetectorReader - getTRGReader: swap error in TRGP: %s %d\n",
					__FILE__,__LINE__) ; return NULL;}
    pTRGP->header.CRC = 0;
    TRG_Reader *r = new TRG_Reader(er,pTRGP);
    if (!r->GetErr()) return r;
    (void) printf("DetectorReader - getTRGReader: Error => deleting implementation (check will cause event to be skipped)\n");
    delete r;
    return NULL;
  }
  return NULL;
}

L3_Reader *getL3Reader(EventReader *er)
{
  Bank_L3_P *pL3P;
  pL3P = (Bank_L3_P *)er->findBank("L3_P");
  if(pL3P)
  {
    if(!pL3P->test_CRC()) printf("DetectorReader - getL3Reader:  CRC error in L3P: %s %d\n",
				 __FILE__,__LINE__);
    if(pL3P->swap() < 0)  printf("DetectorReader - getL3Reader:  swap error in L3P: %s %d\n",
				 __FILE__,__LINE__);
    pL3P->header.CRC = 0;
    return new L3_Reader(er,pL3P);
  }
  return NULL;
}

TOF_Reader *getTOFReader(EventReader *er)
{
  Bank_TOFP *pTOFP;
  pTOFP = (Bank_TOFP *)er->findBank("TOFP");
  if (pTOFP)  {
    if (!pTOFP->test_CRC())  printf("DetectorReader - getTOFReader: CRC error in TOFP: %s %d\n",
                                      __FILE__,__LINE__) ;
    if (pTOFP->swap() < 0)   printf("DetectorReader - getTOFReader: swap error in TOFP: %s %d\n",
                                      __FILE__,__LINE__) ;
    pTOFP->header.CRC = 0;
    return new TOF_Reader(er,pTOFP);
  }
  return NULL;
}

FPD_Reader *getFPDReader(EventReader *er)
{
  Bank_FPDP *pFPDP;
  pFPDP = (Bank_FPDP *)er->findBank("FPDP");
  if (pFPDP)  {
    if (!pFPDP->test_CRC())  printf("DetectorReader - getFPDReader: CRC error in FPDP: %s %d\n",
                                      __FILE__,__LINE__) ;
    if (pFPDP->swap() < 0)   printf("DetectorReader - getFPDReader: swap error in FPDP: %s %d\n",
                                      __FILE__,__LINE__) ;
    pFPDP->header.CRC = 0;
    return new FPD_Reader(er,pFPDP);
  }
  return NULL;
}

SC_Reader *getSCReader(EventReader *er)
{
  return new SC_Reader(er);
  return FALSE;
}

