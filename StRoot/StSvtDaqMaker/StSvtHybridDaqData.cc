/***************************************************************************
 *
 * $Id: StSvtHybridDaqData.cc,v 1.3 2001/10/24 16:49:43 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDaqData.cc,v $
 * Revision 1.3  2001/10/24 16:49:43  munhoz
 * adding capability to retrieve t0 and first SCA
 *
 * Revision 1.2  2001/07/11 23:29:48  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 * Revision 1.1  2000/06/13 20:42:06  caines
 * StRoot/StSvtDaqMaker
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This is the class to access the data from each hybrid.                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtHybridDaqData.hh"
#include "StSequence.hh"
#include "StDAQMaker/StSVTReader.h"

ClassImp(StSvtHybridDaqData)

StSvtHybridDaqData::StSvtHybridDaqData(int barrel, int ladder, int wafer, int hybrid, StSVTReader* reader, char* option) : 
  StSvtHybridData(barrel, ladder, wafer, hybrid)
{
  if (reader)
    setHybridData(reader,option);
}

int StSvtHybridDaqData::setHybridData(StSVTReader* reader, char* option)
{
  // fills the data members of this object, 
  // so one can access the data through the two methods mentioned below. 
  // Therefore, it must be invoked before the access methods. 
  // This method can be overloaded to allow other types of input data (simulators, ASCII files, etc.). 
  // If everything goes OK, it returns kTRUE.

  int anode, n, status=0, ianode;

  if ( !strncmp(option, "RAW", strlen("RAW")) ) {
    unsigned char* array;

    nAnodes = 240;
    if (!anodeList)
      anodeList = new int[nAnodes];
    if (!nSeq)
      nSeq = new int[nAnodes];
    if (!seq)
      seq = new StSequence*[nAnodes];
    for (ianode=0;ianode<nAnodes;ianode++)
      seq[ianode] = 0;
    
    for (ianode=0;ianode<nAnodes;ianode++) {

      anode = ianode + 1;
      anodeList[ianode] = anode;
      status = reader->getRawADC(mBarrel, mLadder, mWafer, mHybrid, anode, n, array);

      if (status != 1) return status;
      //    nSeq[ianode] = n;
      nSeq[ianode] = 1;
      if (!seq[ianode])
	seq[ianode] = new StSequence[nSeq[ianode]];

      for (int iseq=0;iseq<nSeq[ianode];iseq++) {
	seq[ianode][iseq].startTimeBin = iseq;
	//      seq[ianode][iseq].length = 1;
	seq[ianode][iseq].length = 128;
	seq[ianode][iseq].firstAdc = array;
      }
    }
  }

  else if ( !strncmp(option, "ZS", strlen("ZS")) ) {
    
    int anode, n, status=0, ianode;
    unsigned char* array;
  
    // Initialize anodeList with maximum number of anodes
    if (!anodeList)
      anodeList = new int[240];

    // fill anodeList
    nAnodes = reader->getAnodeList(mBarrel, mLadder, mWafer, mHybrid, array);
     
    //printf("SvtHybridDaqData:barrel %d, ladder %d, wafer %d, hybrid %d\n",mBarrel,mLadder,mWafer,mHybrid);

    // check if reader returns a positive number. Otherwise, there is some problem...
    if (nAnodes < 0) return nAnodes;

    // Initialization
    if (!nSeq)
      nSeq = new int[nAnodes];
    if (!seq)
      seq = new StSequence*[nAnodes];
    for (ianode=0;ianode<nAnodes;ianode++)
      seq[ianode] = 0;

    TPCSequence *tempseq;

    // loop through all relevant anodes and fill sequences    
    for (ianode=0;ianode<nAnodes;ianode++) {

      anode = (int)array[ianode];
      anodeList[ianode] = anode;

      status = reader->getSequences(mBarrel, mLadder, mWafer, mHybrid, anode, n, tempseq);

      if (status < 0) return status;

      nSeq[ianode] = n;
      if (!seq[ianode])
	seq[ianode] = new StSequence[nSeq[ianode]];

      for (int iseq=0;iseq<nSeq[ianode];iseq++) {
	seq[ianode][iseq].startTimeBin = tempseq[iseq].startTimeBin;
	seq[ianode][iseq].length = tempseq[iseq].Length;
	seq[ianode][iseq].firstAdc = tempseq[iseq].FirstAdc;
      }
    }
  }

  mSCAZero = (unsigned char)reader->getSCAZero();
  mTimeZero = (unsigned char)reader->getTimeZero(); 
  
  return status;
}
