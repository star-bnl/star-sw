/***************************************************************************
 *
 * $Id: StTPCReader.cxx,v 1.8 2004/03/10 05:59:04 jeromel Exp $
 *
 * Author: Victor Perev
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ reader classes
 *
 ***************************************************************************
 *
 * $Log: StTPCReader.cxx,v $
 * Revision 1.8  2004/03/10 05:59:04  jeromel
 * Implement SetSequenceMerging(). See comment in St_tpcdaq_Maker
 *
 * Revision 1.7  2004/03/04 21:51:27  ward
 * Replaced MERGE_SEQUENCES with a StDAQMaker chain parameter, as suggested by Landgraf and Lauret.
 *
 * Revision 1.6  2003/04/29 16:22:44  perev
 * non TPCoriented cleanup
 *
 * Revision 1.5  2003/04/22 20:12:43  ward
 * So the chain can run when there is no TPC data.
 *
 * Revision 1.4  2003/03/24 18:12:10  ward
 * Full support for EEMC from Herbert Ward.
 *
 * Revision 1.3  2002/10/13 20:43:36  ward
 * Support for decoding DAQ100 data and writing it into a table.
 *
 * Revision 1.2  2000/07/13 22:29:52  perev
 * Return kStErr when TPC data is not in event.
 *
 * Revision 1.1  2000/06/12 15:12:27  perev
 * SVT + cleanup
 *
 *
 **************************************************************************/
#include "Stypes.h"

//	non standard open,close,read
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "Stypes.h"

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQReader.h"
#include "StTPCReader.h"

//_____________________________________________________________________________
StTPCReader::StTPCReader(StDAQReader *daqr,char mergeSequences)
{
  mMergeSequences=mergeSequences; // won't do much since the entry point is different
  ptrTPCP=NULL;                   // Herb Oct 2002 for DAQ100.
  fDAQReader = daqr;
  fSector = -1999;
  fTPCImpReader 	= 0;
  fZeroSuppressedReader = 0;
  fADCRawReader 	= 0;
  fPedestalReader 	= 0;
  fPedestalRMSReader 	= 0;
  fGainReader 		= 0;
  fCPPReader 		= 0;
  fBadChannelReader 	= 0;
  Update();
}
//_____________________________________________________________________________
int StTPCReader::Update()
{
  return setSector(-1);
}
//_____________________________________________________________________________
int StTPCReader::empty()
{
  if(!fTPCImpReader) Update();
  return (!fTPCImpReader);
}
//_____________________________________________________________________________
StTPCReader::~StTPCReader()
{ close();}

//_____________________________________________________________________________
int StTPCReader::close()
{
  delete fZeroSuppressedReader;	fZeroSuppressedReader 	= 0;
  delete fADCRawReader ;	fADCRawReader 		= 0;
  delete fPedestalReader ;	fPedestalReader 	= 0;
  delete fPedestalRMSReader;	fPedestalRMSReader 	= 0;
  delete fGainReader;		fGainReader 		= 0;
  delete fCPPReader;		fCPPReader 		= 0;
  delete fBadChannelReader;	fBadChannelReader 	= 0;
  delete fTPCImpReader;  	fTPCImpReader  		= 0;
  fSector=-1999;
  ptrTPCP=NULL; // Herb Oct 2002 for DAQ100.
  return 0;
 }
//_____________________________________________________________________________
int StTPCReader::setSector(int sector)
{
  if (fTPCImpReader && sector == fSector) return 0;

  delete fZeroSuppressedReader;	fZeroSuppressedReader 	= 0;
  delete fADCRawReader ;	fADCRawReader 		= 0;
  delete fPedestalReader ;	fPedestalReader 	= 0;
  delete fPedestalRMSReader;	fPedestalRMSReader 	= 0;
  delete fGainReader;		fGainReader 		= 0;
  delete fCPPReader;		fCPPReader 		= 0;
  delete fBadChannelReader;	fBadChannelReader 	= 0;

  if (!fTPCImpReader || sector == -1) {
   delete fTPCImpReader;
   ptrTPCP=NULL; // Herb Oct 2002 for DAQ100.
   fTPCImpReader = ::getDetectorReader(fDAQReader->getEventReader(),fDAQReader->getTPCVersion());
   // assert(fTPCImpReader); We will depend on the "return 1" below.  Herb Ward, Apr 22 2003.
   fSector = -1999;
   if(!fTPCImpReader) return 1;
   ptrTPCP=fTPCImpReader->motherPointerBank; // Herb Oct 2002 for DAQ100.
   if (sector == -1) return 0;
  }

  fSector = sector;

  assert(fTPCImpReader); // Better than a crash.
  assert( fSector<90 && (mMergeSequences==1||mMergeSequences==0) ); // See below getZeroSuppressedReader.
  fZeroSuppressedReader = fTPCImpReader->getZeroSuppressedReader(fSector+100*mMergeSequences);
  fADCRawReader 	= fTPCImpReader->getADCRawReader(fSector);
  fPedestalReader 	= fTPCImpReader->getPedestalReader(fSector);
  fPedestalRMSReader 	= fTPCImpReader->getPedestalRMSReader(fSector);
  fGainReader 		= fTPCImpReader->getGainReader(fSector);
  fCPPReader 		= fTPCImpReader->getCPPReader(fSector);
  fBadChannelReader 	= fTPCImpReader->getBadChannelReader(fSector);


  return 0;
}
//_____________________________________________________________________________

int StTPCReader::getMaxPad(int padrow) const
{
  const unsigned char PADS[45] = {
   88, 96,104,112,118,126,134,142,150,
  158,166,174,182, 98,100,102,104,106,
  106,108,110,112,112,114,116,118,120,
  122,122,124,126,128,128,130,132,134,
  136,138,138,140,142,144,144,144,144};
  
  assert(padrow>0 && padrow <=45);   
  return PADS[padrow-1];
}
  
//_____________________________________________________________________________
int StTPCReader::getPadList(int Sector, int PadRow, unsigned char *&padList)
{
  if (setSector(Sector)) 	return -1;  
  if (!fZeroSuppressedReader) 	return -1;
  return fZeroSuppressedReader->getPadList(PadRow, &padList);  
} 
//_____________________________________________________________________________
  int StTPCReader::getSequences(int Sector, int PadRow, int Pad, int &nSeq,
			   TPCSequence *&SeqData) 
{
  if (setSector(Sector)) 	return -1;
  nSeq = 0; SeqData = 0;
  if (!fZeroSuppressedReader) 	return -1;
  Sequence *seq;
  int iret = fZeroSuppressedReader->getSequences(PadRow,Pad,&nSeq,&seq);
  assert (sizeof(TPCSequence)==sizeof(Sequence));
  SeqData = (TPCSequence*)seq;
  return iret;
}

//_____________________________________________________________________________
int StTPCReader::getRawADC(int Sector,int PadRow, int Pad, int &nArray,
                        unsigned char *&Array)
{
  if (setSector(Sector)) 	return -1;
  nArray = 0; Array=0;
  if (!fADCRawReader) return -1;
  return fADCRawReader->getSequences(PadRow,Pad,&nArray,&Array);
}  
//_____________________________________________________________________________
int StTPCReader::getPedestals(int Sector,int PadRow, int Pad, int &nArray,
                           unsigned char *&Array)
{  
  if (setSector(Sector)) 	return -1;
  nArray = 0; Array=0;
  if (!fPedestalReader) 	return -1;
  return fPedestalReader->getSequences(PadRow,Pad,&nArray,&Array);
}  
//_____________________________________________________________________________
int StTPCReader::getRMSPedestals(int Sector,int PadRow, int Pad, int &nArray,
                           unsigned char *&Array)
{  
  if (setSector(Sector)) 	return -1;
  nArray = 0; Array=0;
  if (!fPedestalRMSReader) 	return -1;
  return fPedestalRMSReader->getSequences(PadRow,Pad,&nArray,&Array);
}  
//_____________________________________________________________________________
int StTPCReader::getGain(int Sector, int PadRow, int Pad, TPCGain *&gain)
{  
  if (setSector(Sector)) 	return -1;
  gain = 0;
  if (!fGainReader) 		return -1; 
  struct Gain *gainqq;
  int iret = fGainReader->getGain(PadRow,Pad,&gainqq);
  assert(sizeof(TPCGain)==sizeof(struct Gain));
  gain = (TPCGain*)gainqq;
  return iret;
}
//_____________________________________________________________________________
int StTPCReader::getClusters(int Sector, int PadRow, int Pad, int &nClusters, 
			     TPCCluster *&clusters)
{  
  if (setSector(Sector)) 	return -1;
  nClusters=0; clusters=0;
  if (!fCPPReader) 		return -1;
  struct ASIC_Cluster *clustersqq;
  int iret = fCPPReader->getClusters(PadRow,Pad,&nClusters,&clustersqq);
  assert(sizeof(TPCCluster)==sizeof(struct ASIC_Cluster));
  clusters = (TPCCluster *)clustersqq;
  return iret;
}  
//_____________________________________________________________________________
int StTPCReader::IsBad(int Sector, int PadRow, int Pad)
{
  if (setSector(Sector)) 	return -1;
  if (!fBadChannelReader) 	return -1;
  return fBadChannelReader->IsBad(PadRow,Pad);
}

void StTPCReader::SetSequenceMerging (char mergeSequences)
{
  mMergeSequences=mergeSequences;
}
