/***************************************************************************
 *
 * $Id: StSvtHybridSimData.cc,v 1.1 2000/11/30 20:47:49 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridSimData.cc,v $
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This is the class to access the data from each hybrid.                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtHybridSimData.hh"
#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "iostream.h"

ClassImp(StSvtHybridSimData)

StSvtHybridSimData::StSvtHybridSimData(int barrel, int ladder, int wafer, int hybrid,StSvtHybridPixels* mSimDataPixels):StSvtHybridData(barrel, ladder, wafer,hybrid)
{
  if (mSimDataPixels)
    setSimHybridData(mSimDataPixels);
}

int StSvtHybridSimData::setSimHybridData(StSvtHybridPixels* mSimDataPixels)
{


  nAnodes = 240;
  if (!anodeList)
    anodeList = new int[nAnodes];
  if (!nSeq)
    nSeq = new int[nAnodes];
  if (!seq)
    seq = new StSequence*[nAnodes];


  for (int ianode=0;ianode<nAnodes;ianode++) {
    anodeList[ianode] = ianode + 1;
    nSeq[ianode]= 1;

    if (!seq[ianode])
      seq[ianode] = new StSequence[nSeq[ianode]];
        
    for (int iseq=0;iseq<nSeq[ianode];iseq++) {

      if (!seq[ianode][iseq].firstAdc)
	seq[ianode][iseq].firstAdc = new unsigned char[128];

      for (int i=0;i<128;i++)
	{
	  float adc = mSimDataPixels->getPixelContent(ianode+1,i) + 100.0;
	  if( adc >=0 && adc < 255){
	    seq[ianode][iseq].firstAdc[i] =  (unsigned char) adc;
	  }
	  else{
	    seq[ianode][iseq].firstAdc[i] =  (unsigned char)255;
	  }
	}

      seq[ianode][iseq].startTimeBin = 0;
      seq[ianode][iseq].length = 128;
    }
  }

  return 0;
}
