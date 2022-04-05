/***************************************************************************
 *
 * $Id: StSvtHybridDaqData.cc,v 1.5 2009/08/26 17:51:35 fine Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDaqData.cc,v $
 * Revision 1.5  2009/08/26 17:51:35  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.4  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
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
#include <cassert>

ClassImp(StSvtHybridDaqData)

StSvtHybridDaqData::StSvtHybridDaqData(int barrel, int ladder, int wafer, int hybrid, StSVTReader* reader, char* option) : 
  StSvtHybridData(barrel, ladder, wafer, hybrid)
{
  assert(sizeof(TPCSequence)==sizeof(StSequence));
  assert(&(((TPCSequence*)1)->startTimeBin)==&(((StSequence*)1)->startTimeBin));
  assert(&(((TPCSequence*)1)->Length      )==&(((StSequence*)1)->length      ));
  assert(&(((TPCSequence*)1)->FirstAdc    )==&(((StSequence*)1)->firstAdc    ));

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
  unsigned char* array;

  // Initialize anodeList with maximum number of anodes
  int nAnodes = 240;

  if ( !strncmp(option, "RAW", strlen("RAW")) ) {

    seq.resize(nAnodes); 
    
    for (ianode=0;ianode<nAnodes;ianode++) {

      anode = ianode + 1;
      seq[ianode].mAnode= anode;
      status = reader->getRawADC(mBarrel, mLadder, mWafer, mHybrid, anode, n, array);

      if (status != 1) return status;
      //    nSeq[ianode] = n;
      n = 1; // VP
      seq[ianode].resize(n);

      for (int iseq=0;iseq<n;iseq++) {
	seq[ianode][iseq].startTimeBin = iseq;
	seq[ianode][iseq].length = 128;
	seq[ianode][iseq].firstAdc = array;
      }
    }
  }

  else if ( !strncmp(option, "ZS", strlen("ZS")) ) {
      

    // fill anodeList
    nAnodes = reader->getAnodeList(mBarrel, mLadder, mWafer, mHybrid, array);
     
    //printf("SvtHybridDaqData:barrel %d, ladder %d, wafer %d, hybrid %d\n",mBarrel,mLadder,mWafer,mHybrid);

    // check if reader returns a positive number. Otherwise, there is some problem...
    if (nAnodes < 0) return nAnodes;

    // Initialization
    seq.resize(nAnodes);

    TPCSequence *tempseq=0;

    // loop through all relevant anodes and fill sequences    
    for (ianode=0;ianode<nAnodes;ianode++) {

      anode = (int)array[ianode];
      seq[ianode].mAnode= anode;
      status = reader->getSequences(mBarrel, mLadder, mWafer, mHybrid, anode, n, tempseq);

      if (status < 0) return status;

      seq[ianode].resize(n);
      memcpy(&(seq[ianode][0]),tempseq,n*sizeof(StSequence));
    }
  }
  setAnodeList();
  mSCAZero  = (unsigned char)reader->getSCAZero();
  mTimeZero = (unsigned char)reader->getTimeZero(); 
  
  return status;
}
