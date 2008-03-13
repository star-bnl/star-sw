/***************************************************************************
 *
 * $Id: StTpcRawData.cxx,v 2.1 2008/03/13 16:42:24 ullrich Exp $
 *
 * Author: Yuri Fisyak, Mar 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcRawData.cxx,v $
 * Revision 2.1  2008/03/13 16:42:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcRawData.h"
#include "Riostream.h"
#include <assert.h>
#include "TMath.h"
#include "StDaqLib/TPC/trans_table.hh"

static const int NumberOfRows = 45;
static const int NumberOfPadsAtRow[45] = {
    88, 96,104,112,118,126,134,142,150,158,
    166,174,182,  
    98,100,102,104,106,106,108,
    110,112,112,114,116,118,120,122,122,124,
    126,128,128,130,132,134,136,138,138,140,
    142,144,144,144,144
};

ClassImp(StTpcDigitalSector);
ClassImp(StTpcRawData);

//________________________________________________________________________________
StTpcDigitalSector::StTpcDigitalSector(void *db) {
    StDigitalTimeBins  timeBins;
    StDigitalPadRow    padRow;
    int     irow;
    
    for(irow=0; irow< NumberOfRows; irow++) {
        padRow.resize(NumberOfPadsAtRow[irow], timeBins);
        mData.push_back(padRow);
    }
}

//________________________________________________________________________________
void StTpcDigitalSector::clear() // clears only the time bins
{
    //  cout << "StTpcDigitalSector::clear()" << endl;
    for(unsigned int irow=0; irow<mData.size(); irow++) {
        for(unsigned int ipad=0; ipad<mData[irow].size(); ipad++) {
	  mData[irow][ipad].clear();
        }
    }
}

//________________________________________________________________________________
void StTpcDigitalSector::assignTimeBins(int rowN, int padN, StDigitalTimeBins* tbins)
{
    if (mData[(rowN-1)][(padN-1)].size())
        mData[(rowN-1)][(padN-1)].clear();
    mData[(rowN-1)][(padN-1)].swap(*tbins);
}

//________________________________________________________________________________
int StTpcDigitalSector::cleanup()
{
    unsigned int numberOfEmptyRows=0;
    for (unsigned int iRow=0; iRow<mData.size(); iRow++) {
        unsigned int numberOfEmptyPads=0;
        for (unsigned int iPad=0; iPad<mData[iRow].size(); iPad++) {
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
int StTpcDigitalSector::getSequences(int PadRow, int Pad, int *nSeq, StSequence** Seq, unsigned short ***Ids) {
    *Seq=0;
    if (Ids) *Ids=0;*nSeq=0;
    mSequence.clear();
    mIds.clear();
    StDigitalTimeBins* TrsPadData = timeBinsOfRowAndPad(PadRow,Pad);
    if (!TrsPadData) return 1;
    StDigitalTimeBins &trsPadData = *TrsPadData;
    int nTimeBins = trsPadData.size();
    if (!nTimeBins) return 2;
    // Construct the sequences:
    StSequence aSequence;
    static unsigned short ids[32] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
    for (int ibin=0;ibin<nTimeBins;ibin++)  {
        aSequence.length       = trsPadData[ibin].size();
        if (aSequence.length > 31) aSequence.length = 31;
        aSequence.startTimeBin = trsPadData[ibin].time();
        aSequence.firstAdc     = trsPadData[ibin].adc();
        mSequence.push_back(aSequence);
        if (trsPadData[ibin].isIdt()) mIds.push_back(trsPadData[ibin].idt());
        else                          mIds.push_back(&ids[0]);
    }
    *nSeq = mSequence.size();
    *Seq = &mSequence[0];
    if (Ids) *Ids = &mIds[0];
    
    return 0;
}

//________________________________________________________________________________
int StTpcDigitalSector::getPadList(int padRow, unsigned char **padList) {
    mPadList.clear();
    assert( padRow>=1 && padRow <=45);
    // Loop over all the pads:
    for(int ii = 1; ii<=NumberOfPadsAtRow[padRow-1]; ii++) {
        if (numberOfTimeBins(padRow,ii) > 0) {
	  mPadList.push_back(ii);
        }
    }
    *padList = &mPadList[0];
    return mPadList.size();
}

//________________________________________________________________________________
int StTpcDigitalSector::putTimeAdc(int row, int pad, unsigned short *ADCs, unsigned short *IDTs) {// 10 -> 8 conversion
    int ntimebins = 0;
    StDigitalTimeBins  digPadData;
    int tbC = -999;
    for (int tb = 0; tb < 512; tb++) {
        if (! ADCs[tb]) continue;
        if (tb != tbC+1) digPadData.push_back(StDigitalPair(tb));
        tbC = tb;
        unsigned char adc = log10to8_table[ADCs[tb]];
        if (IDTs) digPadData.back().add(adc,IDTs[tb]);
        else      digPadData.back().add(adc);
        ntimebins++;
    }
    if (ntimebins) assignTimeBins(row,pad,&digPadData);
    return ntimebins;
}

//________________________________________________________________________________
int StTpcDigitalSector::putTimeAdc(int row, int pad, unsigned char *ADCs, unsigned short *IDTs) {// no conversion
    int ntimebins = 0;
    StDigitalTimeBins  digPadData;
    int tbC = -999;
    for (int tb = 0; tb < 512; tb++) {
        if (! ADCs[tb]) continue;
        if (tb != tbC+1) digPadData.push_back(StDigitalPair(tb));
        tbC = tb;
        unsigned char adc = ADCs[tb];
        if (IDTs) digPadData.back().add(adc,IDTs[tb]);
        else      digPadData.back().add(adc);
        ntimebins++;
    }
    assignTimeBins(row,pad,&digPadData);
    return ntimebins;
}

//________________________________________________________________________________
int StTpcDigitalSector::getTimeAdc(int row, int pad, unsigned short ADCs[512], unsigned short IDTs[512]) { // 8->10 conversion
    int ntimebins = 0;
    memset (ADCs, 0, sizeof(ADCs));
    memset (IDTs, 0, sizeof(IDTs));
    StSequence *list;
    unsigned short  **listOfIdt = 0;
    int       nseq = 0;
    if (getSequences(row,pad,&nseq,&list,&listOfIdt)) return ntimebins;
    for (int i = 0; i < nseq; i++) {
        int tb = list[i].startTimeBin;
        tb = TMath::Max(0,TMath::Min(511,tb));
        int seqLen=list[i].length;
        for (int j = 0; j < seqLen; j++) {
	  ntimebins++;
	  ADCs[tb+j] = log8to10_table[list[i].firstAdc[j]];
	  IDTs[tb+j] = (listOfIdt) ? listOfIdt[i][j] : 0;
        }
    }
    return ntimebins;
}

//________________________________________________________________________________
int StTpcDigitalSector::getTimeAdc(int row, int pad, unsigned char ADCs[512], unsigned short IDTs[512]) {// no conversion
    int ntimebins = 0;
    memset (ADCs, 0, sizeof(ADCs));
    memset (IDTs, 0, sizeof(IDTs));
    StSequence *list;
    unsigned short  **listOfIdt = 0;
    int       nseq = 0;
    if (getSequences(row,pad,&nseq,&list,&listOfIdt)) return ntimebins;
    for (int i = 0; i < nseq; i++) {
        int tb = list[i].startTimeBin;
        tb = TMath::Max(0,TMath::Min(511,tb));
        int seqLen=list[i].length;
        for (int j = 0; j < seqLen; j++) {
	  ntimebins++;
	  ADCs[tb+j] = list[i].firstAdc[j];
	  IDTs[tb+j] = (listOfIdt) ? listOfIdt[i][j] : 0;
        }
    }
    return ntimebins;
}

//________________________________________________________________________________
StTpcDigitalSector &StTpcDigitalSector::operator+= (StTpcDigitalSector& v) {
    static unsigned short ADCs1[512], ADCs2[512];
    static unsigned short IDTs1[512], IDTs2[512];
    for (int row = 1; row <= 45; row++) {
        int npad2 = v.numberOfPadsInRow(row);
        if (! npad2) continue;
        for (int pad = 1; pad <= NumberOfPadsAtRow[row-1]; pad++) {
	  int ntb2 =  v.numberOfTimeBins(row,pad);
	  if (! ntb2) continue;
	  int ntb1 =    numberOfTimeBins(row,pad);
	  if (! ntb1) {
	      StDigitalTimeBins *tbins2 = v.timeBinsOfRowAndPad(row,pad);
	      assignTimeBins(row,pad,tbins2);
	      continue;
	  }
	  getTimeAdc(row,pad,ADCs1,IDTs1);
	  v.getTimeAdc(row,pad,ADCs2,IDTs2);
	  Bool_t ifIDT = false;
	  for (int i = 0; i < 512; i++) {
	      if (! ifIDT && (IDTs1[i] || IDTs2[i])) ifIDT = true;
	      if ((IDTs1[i] || IDTs2[i]) && ADCs1[i] < ADCs2[i]) IDTs1[i] = IDTs2[i];
	      ADCs1[i] += ADCs2[i];
	  }
	  if (ifIDT) putTimeAdc(row, pad, ADCs1, IDTs1);
	  else       putTimeAdc(row, pad, ADCs1);
        }
    }
    return *this;
}

//________________________________________________________________________________
void StTpcDigitalSector::Print(const Option_t *opt) const {
    //  cout << "sector " << mSector << endl;
    int nrows = numberOfRows();
    for (int row = 1; row <= nrows; row++) {
        //    cout << "sector/row " << mSector << "/" << row << endl;
        int npads = numberOfPadsInRow(row);
        for (int pad = 1; pad <= npads; pad++) {
	  //      cout << "sector/row/pad = " << mSector << "/" << row << "/" << pad << endl;
	  int ntb = numberOfTimeBins(row,pad);
	  if (! ntb) continue;
	  cout << "sector/row/pad = " << mSector << "/" << row << "/" << pad << " = " << ntb << " time sequences" << endl;
        }
    }
}

//________________________________________________________________________________
void StTpcRawData::setSector(unsigned int  sector, StTpcDigitalSector* digitSector) {    
    if (sector > 0 && sector <= mSectors.size()) {
        if (mSectors[sector-1]) delete mSectors[sector-1];
        digitSector->setSector(sector);
        mSectors[sector-1] = digitSector;
    }
}

//________________________________________________________________________________
void StTpcRawData::Clear(const Option_t*) {
    for (unsigned int ii=0; ii<mSectors.size(); ii++) {SafeDelete(mSectors[ii]);}
}

//________________________________________________________________________________
StTpcRawData &StTpcRawData::operator+=(StTpcRawData &v) {
    for (int i = 1; i <= 24; i++) {
        StTpcDigitalSector *s1 =   GetSector(i);
        StTpcDigitalSector *s2 = v.GetSector(i);
        if (! s2) continue;
        if (! s1) {
	  setSector(i, s2);
	  continue;
        }
        *s1 += *s2;
    }
    return *this;
}

//________________________________________________________________________________
int StTpcRawData::getVecOfPixels(StVectPixel &pixels, int sector, int row, int padMin, int padMax, int tMin, int tMax) {
    pixels.clear();
    StTpcDigitalSector *s =   GetSector(sector);
    static unsigned short ADCs[512],  IDTs[512];
    int npads = s->numberOfPadsInRow(row);
    if (npads) {
        for (int pad = padMin; pad <= padMax; pad++) {
	  int ntbs =    s->numberOfTimeBins(row,pad);
	  if (ntbs) {
	      s->getTimeAdc(row,pad,ADCs,IDTs);
	      for (int tb = tMin; tb <= tMax; tb++) {
		if (ADCs[tb])	pixels.push_back(StTpcPixel(kTpcId,sector,row,pad,tb,ADCs[tb],IDTs[tb],0));
	      }
	  }
        }
    }
    return pixels.size();
}

//________________________________________________________________________________
void StTpcRawData::Print(const Option_t *opt) const {
    int N = ((StTpcRawData *) this)->size();
    for (int i = 0; i < N; i++) ((StTpcDigitalSector* )mSectors[i])->Print(opt);
}
