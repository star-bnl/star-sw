/***************************************************************************
 *
 * $Id: 
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ SVT reader classes
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/
//	non standard open,close,read
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include "Stypes.h"
//
#include "StSVTReader.h"
#include "StDaqLib/SVT/SVTV1P0_Reader.hh"



typedef EventInfo DAQEventInfo;
  
//_____________________________________________________________________________
StSVTReader::StSVTReader(StDAQReader *daqr)
{
  fDAQReader = daqr;
  fWafer = -1999;
  fSVTImpReader 	= 0;
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
void StSVTReader::Update()
{
  setWafer(-1,-1,-1);
}
//_____________________________________________________________________________
StSVTReader::~StSVTReader()
{
  close();
}
//_____________________________________________________________________________
int StSVTReader::close()
{
  fWafer = -1999;

  delete fSVTImpReader;		fSVTImpReader		=0;
  delete fZeroSuppressedReader;	fZeroSuppressedReader	=0;
  delete fADCRawReader;        	fADCRawReader		=0;
  delete fPedestalReader;   	fPedestalReader		=0;  
  delete fPedestalRMSReader;	fPedestalRMSReader 	=0; 
  delete fGainReader ;      	fGainReader    		=0;
  delete fCPPReader ;		fCPPReader    		=0;       
  delete fBadChannelReader;	fBadChannelReader 	=0;   
  return 0;
 }
//_____________________________________________________________________________
int StSVTReader::setWafer(int barrel, int ladder, int wafer)
{
  int waferIndex = getWaferIndex(barrel, ladder, wafer);

  if (waferIndex == fWafer) return waferIndex;

  delete fZeroSuppressedReader;	fZeroSuppressedReader	=0;
  delete fADCRawReader;        	fADCRawReader		=0;
  delete fPedestalReader;   	fPedestalReader		=0;  
  delete fPedestalRMSReader;	fPedestalRMSReader 	=0; 
  delete fGainReader ;      	fGainReader    		=0;
  delete fCPPReader ;		fCPPReader    		=0;       
  delete fBadChannelReader;	fBadChannelReader 	=0;   

  if ((barrel == -1) && (ladder == -1) && (wafer == -1)) {
   delete fSVTImpReader;
   fSVTImpReader = ::getDetectorReader(fDAQReader->getEventReader(), "SVT");
   fWafer = -1999;
  }

  if ((barrel == -1) && (ladder == -1) && (wafer == -1)) return -1;

  fWafer = waferIndex;

  fZeroSuppressedReader = ((SVTV1P0_Reader*)fSVTImpReader)->getZeroSuppressedReader(barrel, ladder, wafer);
  fADCRawReader 	= ((SVTV1P0_Reader*)fSVTImpReader)->getADCRawReader(barrel, ladder, wafer);
  fPedestalReader 	= ((SVTV1P0_Reader*)fSVTImpReader)->getPedestalReader(barrel, ladder, wafer);
  fPedestalRMSReader 	= ((SVTV1P0_Reader*)fSVTImpReader)->getPedestalRMSReader(barrel, ladder, wafer);
  //  fGainReader 		= ((SVTV1P0_Reader*)fSVTImpReader)->getGainReader(barrel, ladder, wafer);
  fCPPReader 		= ((SVTV1P0_Reader*)fSVTImpReader)->getCPPReader(barrel, ladder, wafer);
  //  fBadChannelReader 	= ((SVTV1P0_Reader*)fSVTImpReader)->getBadChannelReader(barrel, ladder, wafer);

  mSCAZero = ((SVTV1P0_Reader*)fSVTImpReader)->getSCAZero();
  mTimeZero = ((SVTV1P0_Reader*)fSVTImpReader)->getTimeZero();

  return waferIndex;
}
//_____________________________________________________________________________

int StSVTReader::getWaferIndex(int barrel, int ladder, int wafer) 
{
 short index;
 int numberOfLadders[3] = {8,12,16};
 int numberOfWafers[3] = {4,6,7};
 int numberOfHybrids = 2; if(numberOfHybrids) {/*touch*/}

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
   //   cout << "ERROR: There is NO barrel number " << barrel << " !!!" << endl;
   index = -1;
   break;
  }

  if ((index < 1) || (index > 216)) index = -1;
  return index;
}
  
//_____________________________________________________________________________
int StSVTReader::getAnodeList(int barrel, int ladder, int wafer, int hybrid, unsigned char *&anodeList)
{
  int waferIndex = setWafer(barrel, ladder, wafer); 
  if (waferIndex == -1) return -1;
  if (!fZeroSuppressedReader) return -1;
  return fZeroSuppressedReader->getPadList(hybrid, &anodeList);  
} 
//_____________________________________________________________________________
int StSVTReader::getSequences(int barrel, int ladder, int wafer, int hybrid, int Anode, int &nSeq,
			   TPCSequence *&SeqData) 
{
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  nSeq = 0; SeqData = 0;
  if (!fZeroSuppressedReader) return -1;
  Sequence *seq;
  int iret = fZeroSuppressedReader->getSequences(hybrid,Anode,&nSeq,&seq);
  assert (sizeof(TPCSequence)==sizeof(Sequence));
  SeqData = (TPCSequence*)seq;
  return iret;
}

//_____________________________________________________________________________
int StSVTReader::getRawADC(int barrel, int ladder, int wafer, int hybrid, int Anode, int &nArray,
                        unsigned char *&Array)
{
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  nArray = 0; Array=0;
  if (!fADCRawReader) return -1;
  int status = fADCRawReader->getSequences(hybrid,Anode,&nArray,&Array);
  //  if (nArray) cout << "StSVTReader::nArray = " << nArray << ", Array = " << Array << endl;
  //  cout << "StSVTReader::status = " << status << endl;
  return status;
}  
//_____________________________________________________________________________
int StSVTReader::getPedestals(int barrel, int ladder, int wafer, int hybrid, int Anode, int &nArray,
                           unsigned char *&Array)
{  
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  nArray = 0; Array=0;
  if (!fPedestalReader) return -1;
  return fPedestalReader->getSequences(hybrid,Anode,&nArray,&Array);
}  
//_____________________________________________________________________________
int StSVTReader::getRMSPedestals(int barrel, int ladder, int wafer, int hybrid, int Anode, int &nArray,
                           unsigned char *&Array)
{  
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  nArray = 0; Array=0;
  if (!fPedestalRMSReader) return -1;
  return fPedestalRMSReader->getSequences(hybrid,Anode,&nArray,&Array);
}  
//_____________________________________________________________________________
int StSVTReader::getGain(int barrel, int ladder, int wafer, int hybrid, int Anode, TPCGain *&gain)
{  
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  gain = 0;
  if (!fGainReader) return -1; 
  struct Gain *gainqq;
  int iret = fGainReader->getGain(hybrid,Anode,&gainqq);
  assert(sizeof(TPCGain)==sizeof(struct Gain));
  gain = (TPCGain*)gainqq;
  return iret;
}
//_____________________________________________________________________________
int StSVTReader::getClusters(int barrel, int ladder, int wafer, int hybrid, int Anode, int &nClusters, 
			     TPCCluster *&clusters)
{  
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  nClusters=0; clusters=0;
  if (!fCPPReader) return -1;
  struct ASIC_Cluster *clustersqq;
  int iret = fCPPReader->getClusters(hybrid,Anode,&nClusters,&clustersqq);
  assert(sizeof(TPCCluster)==sizeof(struct ASIC_Cluster));
  clusters = (TPCCluster *)clustersqq;
  return iret;
}  
//_____________________________________________________________________________
int StSVTReader::IsBad(int barrel, int ladder, int wafer, int hybrid, int Anode)
{
  int waferIndex = setWafer(barrel, ladder, wafer);
  if (waferIndex == -1) return -1;
  if (!fBadChannelReader) return 1;
  return fBadChannelReader->IsBad(hybrid,Anode);
}
  
  
