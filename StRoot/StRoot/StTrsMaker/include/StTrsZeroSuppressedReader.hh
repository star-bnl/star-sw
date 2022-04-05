
/***************************************************************************
 *
 * $Id: StTrsZeroSuppressedReader.hh,v 1.7 2008/06/20 15:01:04 fisyak Exp $
 *
 * Authors: bl, mcbs
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface
 *
 *
 * Int_t getPadList(Int_t PadRow, u_char **padList);
 *
 *        Fills (*padList[]) with the list of pad numbers containing hits
 *        returns number of pads in (*padList)[]
 *        or negative if call fails
 *
 * Int_t getSequences(Int_t PadRow, Int_t Pad, Int_t *nSeq, StSequence **SeqData)
 *
 *        Fills (*SeqData)[] along with the ADC
 *        buffers pointed to by (*SeqData)[]
 *        Set nSeq to the # of elements in the (*SeqData)[] array
 *        returns 0 if OK...or negative if call fails
 *
 * struct Sequence
 * {
 *  u_short startTimeBin;
 *  u_short Length;
 *  u_char *FirstAdc;
 * };
 ***************************************************************************
 *
 * $Log: StTrsZeroSuppressedReader.hh,v $
 * Revision 1.7  2008/06/20 15:01:04  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.6  2005/09/09 22:12:48  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.4  2005/07/19 22:23:04  perev
 * Bug fix
 *
 * Revision 1.3  2003/12/24 13:44:52  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.2  2000/03/15 18:08:43  calderon
 * ZSR is no longer a singleton.  Two will be needed for mixer chain.
 *
 * Revision 1.1  1999/11/05 22:17:05  calderon
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 *
 ***************************************************************************/
#ifndef ST_TRS_ZERO_SUPPRESSED_READER_HH
#define ST_TRS_ZERO_SUPPRESSED_READER_HH

#include "StSequence.hh"
#include "StDaqLib/GENERIC/EventReader.hh"
#include <vector>

#include "StTrsRawDataEvent.hh"
#include "StTrsDigitalSector.hh"

class StTrsZeroSuppressedReader {
  
public:
  StTrsZeroSuppressedReader(StTpcRawData* ev=0);
  ~StTrsZeroSuppressedReader() {}

  Int_t getPadList(Int_t padRow, unsigned char **padList) {return mTheSector->getPadList(padRow,padList);}
  Int_t getSequences(Int_t padRow, Int_t Pad, Int_t *nSeq, StSequence** SeqData, UShort_t ***Ids=0) {
    return mTheSector->getSequences(padRow,Pad,nSeq, SeqData, Ids);
  }
  Int_t getSequences(Int_t padRow, Int_t Pad, Int_t *nSeq, Sequence**   SeqData, UShort_t ***Ids=0) {
    return getSequences(padRow, Pad, nSeq, (StSequence**) SeqData, Ids);
  }
  Int_t setSector(Int_t);
  void clear() {if (mTheSector) mTheSector->clear();}

private:
    StTrsZeroSuppressedReader(Int_t);    

    Int_t checkTheData(UInt_t);

    Int_t                 mSector;
    StTrsDigitalSector* mTheSector;
    StTrsRawDataEvent*  mTrsEvent;

};
#endif
