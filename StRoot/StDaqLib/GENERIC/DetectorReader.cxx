/***************************************************************************
 * $Id: DetectorReader.cxx,v 1.6 2000/06/06 21:46:59 jml Exp $
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
#include "RICH/RICH_Reader.hh"
#include "FTPC/FTPV1P0_Reader.hh"


DetectorReader *getDetectorReader(EventReader *er, string det)
{
  DetectorReader *dr;
  // this switch handles all "TPC-like" detectors
  if(det == "TPC")
    {
      Bank_TPCPV1P0  *pTPCP;
      pTPCP = (Bank_TPCPV1P0 *)er->findBank("TPCP");
      if (!pTPCP) dr = FALSE;
      else dr =  new TPCV1P0_Reader(er,pTPCP);
      
    }
  else if (det == "TPCV1P0")
    {
      Bank_TPCPV1P0  *pTPCP;
      pTPCP = (Bank_TPCPV1P0 *)er->findBank("TPCP");
      if (!pTPCP) dr = FALSE;
      else dr =  new TPCV1P0_Reader(er,pTPCP);
    }
   else if (det == "TPCV2P0")
     {
      Bank_TPCPV2P0  *pTPCP;
      pTPCP = (Bank_TPCPV2P0 *)er->findBank("TPCP");
      if (!pTPCP) dr = FALSE;
      else dr =  new TPCV2P0_Reader(er,pTPCP);
     }
  else if  (det == "SSD")
     {
      Bank_SSDP *pSSDP;
      pSSDP = (Bank_SSDP *)er->findBank("SSDP");
      if (!pSSDP) dr = FALSE;
      else dr =  new SSD_Reader(er,pSSDP);
     }
  else if  (det == "SVT")
     {
      Bank_SVTPV1P0 *pSVTP;
      pSVTP = (Bank_SVTPV1P0 *)er->findBank("SVTP");
      if (!pSVTP) dr = FALSE;
      else dr =  new SVTV1P0_Reader(er,pSVTP);
     }
  else if  (det == "FTPC")
     {
      Bank_FTPPV1P0 *pFTPP;
      pFTPP = (Bank_FTPPV1P0 *)er->findBank("FTPP");
      if (!pFTPP) dr = FALSE;
      else dr =  new FTPV1P0_Reader(er,pFTPP);
     }
  else
  {
    dr = NULL;
  }

  return dr;
}

// DetectorReader *getFTPCReader(EventReader *er)
// {
//   string det="FTPC";
//   DetectorReader *detReader=getDetectorReader(er,det);
//   return detReader;
// }

RICH_Reader *getRICHReader(EventReader *er)
{
  Bank_RICP *pRICP;
  pRICP = (Bank_RICP *)er->findBank("RICP");
  if (pRICP)  {
    if (!pRICP->test_CRC())  printf("CRC error in RICP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pRICP->swap() < 0)   printf("swap error in RICP: %s %d\n",
					__FILE__,__LINE__) ;
    pRICP->header.CRC = 0;
    return new RICH_Reader(er,pRICP);
  }
  return NULL;
}

EMC_Reader *getEMCReader(EventReader *er)
{
  Bank_EMCP *pEMCP;
  pEMCP = (Bank_EMCP *)er->findBank("EMCP");
  if (pEMCP)  {
    if (!pEMCP->test_CRC())  printf("CRC error in EMCP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pEMCP->swap() < 0)   printf("swap error in EMCP: %s %d\n",
					__FILE__,__LINE__) ;
    pEMCP->header.CRC = 0;
    return new EMC_Reader(er,pEMCP);
  }
  return FALSE;
}


TRG_Reader *getTRGReader(EventReader *er)
{
  Bank_TRGP *pTRGP;
  pTRGP = (Bank_TRGP *)er->findBank("TRGP");
  if (pTRGP)  {
    if (!pTRGP->test_CRC())  printf("CRC error in TRGP: %s %d\n",
					__FILE__,__LINE__) ;
    if (pTRGP->swap() < 0)   printf("swap error in TRGP: %s %d\n",
					__FILE__,__LINE__) ;
    pTRGP->header.CRC = 0;
    return new TRG_Reader(er,pTRGP);
  }
  return FALSE;
}
