/***************************************************************************
 *
 * $Id: StSvtHybridSimData.cc,v 1.3 2001/11/06 20:12:06 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridSimData.cc,v $
 * Revision 1.3  2001/11/06 20:12:06  caines
 * Add include for new compiler
 *
 * Revision 1.2  2001/05/10 04:29:52  caines
 * Change pedestal offset to match real raw data
 *
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
  mOffSet=10;
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
	  float adc = (float)mSimDataPixels->getPixelContent(ianode+1,i)+mOffSet;
	  if( adc >=0. && adc < 255.){
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
