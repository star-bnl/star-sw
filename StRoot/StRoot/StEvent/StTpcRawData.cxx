/***************************************************************************
 *
 * $Id: StTpcRawData.cxx,v 2.18 2018/04/10 11:32:08 smirnovd Exp $
 *
 * Author: Yuri Fisyak, Mar 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcRawData.cxx,v $
 * Revision 2.18  2018/04/10 11:32:08  smirnovd
 * Minor corrections across multiple files
 *
 * - Remove ClassImp macro
 * - Change white space
 * - Correct windows newlines to unix
 * - Remove unused debugging
 * - Correct StTpcRTSHitMaker header guard
 * - Remove unused preprocessor directives in StiCA
 * - Minor changes in status and debug print out
 * - Remove using std namespace from StiKalmanTrackFinder
 * - Remove includes for unused headers
 *
 * Revision 2.17  2018/04/07 03:32:06  smirnovd
 * Set default sector id to 20
 *
 * Is should not matter which sector to use for past data but 20 is the one with
 * iTPC in 2018
 *
 * Revision 2.16  2018/04/05 03:16:20  smirnovd
 * Make StTpcDigitalSector compatible with iTPC
 *
 * Revision 2.15  2018/02/18 23:04:49  perev
 * Put back iTPC update
 *
 * Revision 2.13  2012/10/23 20:15:57  fisyak
 * Don't add empty ADC
 *
 * Revision 2.12  2012/05/16 21:35:03  fisyak
 * replace StDigitalPair by its reference
 *
 * Revision 2.11  2012/05/07 14:41:59  fisyak
 * Remove hardcoded separation between Inner and Outer Sectors
 *
 * Revision 2.10  2011/03/31 19:27:47  fisyak
 * Add more safety for work with pixel data
 *
 * Revision 2.9  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.8  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.7  2009/10/12 23:52:32  fisyak
 * Fix relation npad from pad row
 *
 * Revision 2.6  2008/07/31 20:47:26  fisyak
 * Modify operator += and =
 *
 * Revision 2.5  2008/06/23 19:16:19  fisyak
 * fix memset size
 *
 * Revision 2.4  2008/06/20 14:56:34  fisyak
 * Add protection for pad no.
 *
 * Revision 2.3  2008/05/27 14:40:03  fisyak
 * keep pixel raw data as short istead of uchar
 *
 * Revision 2.2  2008/04/24 16:06:25  fisyak
 * Clean up before next move
 *
 * Revision 2.1  2008/03/13 16:42:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcRawData.h"
#include "Riostream.h"
#include <assert.h>
#include "TMath.h"
#include "StDaqLib/TPC/trans_table.hh"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
//________________________________________________________________________________
StTpcDigitalSector::StTpcDigitalSector(void *db) : mSector(20)
{
  StDigitalTimeBins  timeBins;
  mNoRows = St_tpcPadPlanesC::instance()->padRows();
  for(Int_t row=1; row <= mNoRows; row++) {
    StDigitalPadRow    padRow;
    for (Int_t pad = 0; pad < numberOfPadsAtRow(row); pad++) {
      padRow.push_back( timeBins);
    }
    mData.push_back(padRow);
  }
}


StTpcDigitalSector::StTpcDigitalSector(int sector) : mSector(sector)
{
  StDigitalTimeBins  timeBins;
  mNoRows = St_tpcPadConfigC::instance()->padRows(sector);
  for(Int_t row=1; row <= mNoRows; row++) {
    StDigitalPadRow    padRow;
    for (Int_t pad = 0; pad < numberOfPadsAtRow(row); pad++) {
      padRow.push_back( timeBins);
    }
    mData.push_back(padRow);
  }
}

//________________________________________________________________________________
void StTpcDigitalSector::clear() {// clears only the time bins
  for(UInt_t row=0; row<mData.size(); row++) {
    for(UInt_t ipad=0; ipad<mData[row].size(); ipad++) {
      mData[row][ipad].clear();
    }
  }
}
//________________________________________________________________________________
void StTpcDigitalSector::assignTimeBins(Int_t rowN, Int_t padN, StDigitalTimeBins* tbins) {
  assert( (rowN >= 1 && rowN <= mNoRows ) ||
	  (padN >= 1 && padN <= numberOfPadsAtRow(rowN)));
  StDigitalPadRow    &Row = mData[(rowN-1)];
  StDigitalTimeBins  &Pad = Row[(padN-1)];
  if (Pad.size() > 0)  Pad.clear();
  Pad.swap(*tbins);
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::cleanup() {
  UInt_t numberOfEmptyRows=0;
  for (UInt_t iRow=0; iRow<mData.size(); iRow++) {
    UInt_t numberOfEmptyPads=0;
    for (UInt_t iPad=0; iPad<mData[iRow].size(); iPad++) {
      if (mData[iRow][iPad].size()<7) {
	mData[iRow][iPad].clear();
	numberOfEmptyPads++;
      }
    } // Pads are now clean
    if (numberOfEmptyPads == mData[iRow].size()) {
      mData[iRow].clear();
      numberOfEmptyRows++;
    }
  } // Rows are now clean
    //cout << "This sector had " << numberOfEmptyRows << " empty rows." << endl;
  if (numberOfEmptyRows==mData.size()) return 1;
  else return 0;
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::getSequences(Int_t row, Int_t pad, Int_t *nSeq, StSequence** Seq, UShort_t ***Ids) {
  *Seq=0;
  if (Ids) *Ids=0;*nSeq=0;
  mSequence.clear();
  mIds.clear();
  StDigitalTimeBins* TrsPadData = timeBinsOfRowAndPad(row,pad);
  if (!TrsPadData) return 1;
  StDigitalTimeBins &trsPadData = *TrsPadData;
  Int_t nTimeBins = trsPadData.size();
  if (!nTimeBins) return 2;
  // Construct the sequences:
  StSequence aSequence;
  static UChar_t ADCs[__MaxNumberOfTimeBins__];
  static UShort_t IDTs[__MaxNumberOfTimeBins__];
  getTimeAdc(row,pad,ADCs, IDTs);
  
  for (Int_t ibin=0;ibin<nTimeBins;ibin++)  {
    aSequence.length       = trsPadData[ibin].size();
    if (aSequence.length > 31) aSequence.length = 31;
    aSequence.startTimeBin = trsPadData[ibin].time();
    aSequence.firstAdc     = &ADCs[aSequence.startTimeBin];
    mSequence.push_back(aSequence);
    mIds.push_back(&IDTs[aSequence.startTimeBin]);
  }
  *nSeq = mSequence.size();
  *Seq = &mSequence[0];
  if (Ids) *Ids = &mIds[0];
  return 0;
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::getPadList(Int_t row, UChar_t **padList) {
  mPadList.clear();
  assert( row>=1 && row <=mNoRows);
  // Loop over all the pads:
  for(Int_t ii = 1; ii <= numberOfPadsAtRow(row); ii++) {
    if (numberOfTimeBins(row,ii) > 0) {
      mPadList.push_back(ii);
    }
  }
  *padList = &mPadList[0];
  return mPadList.size();
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::putTimeAdc(Int_t row, Int_t pad, Short_t *ADCs, UShort_t *IDTs) {// 10 -> 8 conversion
  Int_t ntimebins = 0;
  StDigitalTimeBins  digPadData;
  Int_t tbC = -999;
  for (Int_t tb = 0; tb < __MaxNumberOfTimeBins__; tb++) {
    if (! ADCs[tb]) continue;
    if (tb != tbC+1) digPadData.push_back(StDigitalPair(tb));
    tbC = tb;
    if (IDTs) digPadData.back().add(ADCs[tb],IDTs[tb]);
    else      digPadData.back().add(ADCs[tb]);
    ntimebins++;
  }
  if (ntimebins) assignTimeBins(row,pad,&digPadData);
  return ntimebins;
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::putTimeAdc(Int_t row, Int_t pad, UChar_t *ADCs, UShort_t *IDTs) {// no conversion
  Int_t ntimebins = 0;
  StDigitalTimeBins  digPadData;
  Int_t tbC = -999;
  for (Int_t tb = 0; tb < __MaxNumberOfTimeBins__; tb++) {
    if (! ADCs[tb]) continue;
    if (tb != tbC+1) digPadData.push_back(StDigitalPair(tb));
    tbC = tb;
    Short_t adc = log8to10_table[ADCs[tb]];
    if (IDTs) digPadData.back().add(adc,IDTs[tb]);
    else      digPadData.back().add(adc);
    ntimebins++;
  }
  assignTimeBins(row,pad,&digPadData);
  return ntimebins;
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::getTimeAdc(Int_t row, Int_t pad, 
				     Short_t  ADCs[__MaxNumberOfTimeBins__],
				     UShort_t IDTs[__MaxNumberOfTimeBins__]) { 
  // no conversion
  UInt_t nTimeSeqs = 0;
  memset (ADCs, 0, __MaxNumberOfTimeBins__*sizeof(Short_t));
  memset (IDTs, 0, __MaxNumberOfTimeBins__*sizeof(UShort_t));
  StDigitalTimeBins* TrsPadData = timeBinsOfRowAndPad(row,pad);
  if (! TrsPadData) return nTimeSeqs;
  StDigitalTimeBins &trsPadData = *TrsPadData;
  nTimeSeqs = trsPadData.size();
  if (! nTimeSeqs) return nTimeSeqs;
  for (UInt_t i = 0; i < nTimeSeqs; i++) {
    StDigitalPair &digPair = trsPadData[i];
    UInt_t ntbk = digPair.size();
    UInt_t tb   = digPair.time();
    UInt_t isIdt= digPair.isIdt();
    for (UInt_t j = 0; j < ntbk; j++, tb++) {
      ADCs[tb] = digPair.adc()[j];
      if (isIdt) IDTs[tb] = digPair.idt()[j];
    }
  }
  return nTimeSeqs;
}
//________________________________________________________________________________
Int_t StTpcDigitalSector::getTimeAdc(Int_t row, Int_t pad, 
				     UChar_t ADCs[__MaxNumberOfTimeBins__], 
				     UShort_t IDTs[__MaxNumberOfTimeBins__]) {
  // 10-> 8 conversion
  // no conversion
  UInt_t nTimeSeqs = 0;
  memset (ADCs, 0, __MaxNumberOfTimeBins__*sizeof(UChar_t));
  memset (IDTs, 0, __MaxNumberOfTimeBins__*sizeof(UShort_t));
  StDigitalTimeBins* TrsPadData = timeBinsOfRowAndPad(row,pad);
  if (! TrsPadData) return nTimeSeqs;
  StDigitalTimeBins &trsPadData = *TrsPadData;
  nTimeSeqs = trsPadData.size();
  if (! nTimeSeqs) return nTimeSeqs;
  for (UInt_t i = 0; i < nTimeSeqs; i++) {
    StDigitalPair &digPair = trsPadData[i];
    UInt_t ntbk = digPair.size();
    UInt_t tb   = digPair.time();
    UInt_t isIdt= digPair.isIdt();
    for (UInt_t j = 0; j < ntbk; j++, tb++) {
      if (digPair.adc()[j] <= 0) continue;
      ADCs[tb] = log10to8_table[digPair.adc()[j]];
      if (isIdt) IDTs[tb] = digPair.idt()[j];
    }
  }
  return nTimeSeqs;
 }
//________________________________________________________________________________
Int_t StTpcDigitalSector::PrintTimeAdc(Int_t row, Int_t pad) const {
  UInt_t nTimeSeqs = 0;
  const StDigitalTimeBins* TrsPadData = timeBinsOfRowAndPad(row,pad);
  if (! TrsPadData) return nTimeSeqs;
  const StDigitalTimeBins &trsPadData = *TrsPadData;
  nTimeSeqs = trsPadData.size();
  if (! nTimeSeqs) return nTimeSeqs;
  cout << "Time/Adc/IdTruth for row " << row << "\tpad " << pad << endl;
  for (UInt_t i = 0; i < nTimeSeqs; i++) {
    StDigitalPair digPair = trsPadData[i];
    UInt_t ntbk = digPair.size();
    UInt_t tb   = digPair.time();
    for (UInt_t j = 0; j < ntbk; j++, tb++) {
      if (digPair.adc()[j] <= 0) continue;
      cout << "\t" << tb << "\t" << digPair.adc()[j] << "\t" << digPair.idt()[j] << endl;
    }
  }
  return nTimeSeqs;
 }
//________________________________________________________________________________
StTpcDigitalSector &StTpcDigitalSector::operator+= (StTpcDigitalSector& v) {
  static Short_t ADCs1[__MaxNumberOfTimeBins__], ADCs2[__MaxNumberOfTimeBins__];
  static UShort_t IDTs1[__MaxNumberOfTimeBins__], IDTs2[__MaxNumberOfTimeBins__];
  for (Int_t row = 1; row <= mNoRows; row++) {
    Int_t npad2 = v.numberOfPadsInRow(row);
    if (! npad2) continue;
    for (Int_t pad = 1; pad <= numberOfPadsAtRow(row); pad++) {
      Int_t ntb2 =  v.numberOfTimeBins(row,pad);
      if (! ntb2) continue;
      Int_t ntb1 =    numberOfTimeBins(row,pad);
      if (! ntb1) {
	StDigitalTimeBins tbins2 = *v.timeBinsOfRowAndPad(row,pad);
	assignTimeBins(row,pad,&tbins2);
	continue;
      }
      getTimeAdc(row,pad,ADCs1,IDTs1);
      v.getTimeAdc(row,pad,ADCs2,IDTs2);
      for (Int_t i = 0; i < __MaxNumberOfTimeBins__; i++) {
	if ((IDTs1[i] || IDTs2[i]) && ADCs1[i] < ADCs2[i]) IDTs1[i] = IDTs2[i];
	ADCs1[i] += ADCs2[i];
      }
      putTimeAdc(row, pad, ADCs1, IDTs1);
    }
  }
  return *this;
}
//________________________________________________________________________________
StTpcDigitalSector &StTpcDigitalSector::operator= (const StTpcDigitalSector& v) {
  for (Int_t row = 1; row <= mNoRows; row++) {
    Int_t npad2 = v.numberOfPadsInRow(row);
    if (! npad2) continue;
    for (Int_t pad = 1; pad <= numberOfPadsAtRow(row); pad++) {
      Int_t ntb2 =  v.numberOfTimeBins(row,pad);
      if (! ntb2) continue;
      StDigitalTimeBins tbins2 = *v.timeBinsOfRowAndPad(row,pad);
      assignTimeBins(row,pad,&tbins2);
      continue;
    }
  }
  return *this;
}
//________________________________________________________________________________
void StTpcDigitalSector::Print(const Option_t *opt) const {
  TString Opt(opt);
  for (Int_t row = 1; row <= mNoRows; row++) {
    //    cout << "sector/row " << mSector << "/" << row << endl;
    Int_t npads = numberOfPadsInRow(row);
    for (Int_t pad = 1; pad <= npads; pad++) {
      //      cout << "sector/row/pad = " << mSector << "/" << row << "/" << pad << endl;
      Int_t ntb = numberOfTimeBins(row,pad);
      if (! ntb) continue;
      cout << "sector/row/pad = " << mSector << "/" << row << "/" << pad << " = " << ntb << " time sequences" << endl;
      if (Opt.Contains("all",TString::kIgnoreCase)) PrintTimeAdc(row,pad);
    }
  }
}
//________________________________________________________________________________
void StTpcRawData::setSector(UInt_t  sector, StTpcDigitalSector* digitSector) {    
  if (sector > 0 && sector <= mSectors.size()) {
    if (mSectors[sector-1]) delete mSectors[sector-1];
    digitSector->setSector(sector);
    mSectors[sector-1] = digitSector;
  }
}
//________________________________________________________________________________
void StTpcRawData::Clear(const Option_t*) {
  for (UInt_t ii=0; ii<mSectors.size(); ii++) {SafeDelete(mSectors[ii]);}
}
//________________________________________________________________________________
Int_t StTpcRawData::getVecOfPixels(StVectPixel &pixels, Int_t sector, Int_t row, Int_t padMin, Int_t padMax, Int_t tMin, Int_t tMax) {
  pixels.clear();
  StTpcDigitalSector *s =   GetSector(sector);
  if (s) {
    static Short_t  ADCs[__MaxNumberOfTimeBins__];
    static UShort_t IDTs[__MaxNumberOfTimeBins__];
    Int_t npads = s->numberOfPadsInRow(row);
    if (npads) {
      if (padMin <      1) padMin = 1;
      if (padMax < padMin) padMax = s->numberOfPadsAtRow(row);
      if (tMin   <      0) tMin   = 0;
      if (tMax   <   tMin) tMax =  __MaxNumberOfTimeBins__ - 1;
      tMax   = TMath::Min(tMax,  __MaxNumberOfTimeBins__ - 1);
      for (Int_t pad = padMin; pad <= padMax; pad++) {
	Int_t ntbs =    s->numberOfTimeBins(row,pad);
	if (ntbs) {
	  s->getTimeAdc(row,pad,ADCs,IDTs);
	  for (Int_t tb = tMin; tb <= tMax; tb++) {
	    if (ADCs[tb])	pixels.push_back(StTpcPixel(kTpcId,sector,row,pad,tb,ADCs[tb],IDTs[tb],0));
	  }
	}
      }
    }
  }
  return pixels.size();
}
//________________________________________________________________________________
void StTpcRawData::Print(const Option_t *opt) const {
  Int_t N = ((StTpcRawData *) this)->size();
  for (Int_t i = 0; i < N; i++) {
    StTpcDigitalSector *sector =  ((StTpcDigitalSector* )mSectors[i]);
    if (sector) sector->Print(opt);
  }
}
//________________________________________________________________________________
StTpcRawData &StTpcRawData::operator+= (StTpcRawData& v) {
  for (Int_t sec = 1; sec <= 24; sec++) {
    StTpcDigitalSector *a = getSector(sec);
    StTpcDigitalSector *b = v.getSector(sec);
    if (!b ) continue;
    if (!a) {
      a = new StTpcDigitalSector();
      *a = *b;
      setSector(sec, a);
      continue;
    }
    *a += *b;
  }
  return *this;
}
