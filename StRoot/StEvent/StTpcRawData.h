/***************************************************************************
 *
 * $Id: StTpcRawData.h,v 2.14 2018/09/27 22:01:24 ullrich Exp $
 *
 * Author: Yuri Fisyak, Mar 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcRawData.h,v $
 * Revision 2.14  2018/09/27 22:01:24  ullrich
 * Added missing inheritance, SObject, for StDigitalPair
 *
 * Revision 2.13  2018/09/27 20:03:33  ullrich
 * Added ClassDef for StDigitalPair
 *
 * Revision 2.12  2018/04/05 03:16:20  smirnovd
 * Make StTpcDigitalSector compatible with iTPC
 *
 * Revision 2.11  2018/02/18 23:18:45  perev
 * Remove iTPC related update
 *
 * Revision 2.9  2012/05/07 14:41:59  fisyak
 * Remove hardcoded separation between Inner and Outer Sectors
 *
 * Revision 2.8  2011/03/31 19:27:47  fisyak
 * Add more safety for work with pixel data
 *
 * Revision 2.7  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.6  2009/10/12 23:52:32  fisyak
 * Fix relation npad from pad row
 *
 * Revision 2.5  2008/07/31 20:47:27  fisyak
 * Modify operator += and =
 *
 * Revision 2.4  2008/06/20 14:56:34  fisyak
 * Add protection for pad no.
 *
 * Revision 2.3  2008/05/27 14:40:08  fisyak
 * keep pixel raw data as short istead of uchar
 *
 * Revision 2.2  2008/04/24 16:06:25  fisyak
 * Clean up before next move
 *
 * Revision 2.1  2008/03/13 16:42:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcRawData_h
#define StTpcRawData_h

#include "StObject.h"
#include <vector>
#include <utility>
#include "StSequence.hh"
#include "StTpcPixel.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#define __MaxNumberOfTimeBins__ 512
typedef std::vector<Short_t>  StVectorADC;
typedef std::vector<UShort_t> StVectorIDT;

class StDigitalPair : public StObject {
public:
    StDigitalPair(UShort_t time=0)      {mTime=time;}
    virtual ~StDigitalPair() {}
    void add(Short_t adc)               {mAdc.push_back(adc);}
    void add(Short_t adc,Int_t idt)     {mAdc.push_back(adc); mIdt.push_back(idt);}
    
    Short_t* adc()   const {return (Short_t*)&mAdc[0];}			
    Bool_t   isIdt() const {return mAdc.size() == mIdt.size();}
    UShort_t*idt()   const {return (UShort_t*) (isIdt() ? &mIdt[0] : 0);}			
    Int_t    size()  const {return mAdc.size();}
    UShort_t time()  const {return mTime;}

private:
    UShort_t    mTime;
    StVectorADC mAdc;
    StVectorIDT mIdt; 
    ClassDef(StDigitalPair,1)
};

typedef std::vector<StDigitalPair>           StDigitalTimeBins;
typedef std::vector<StDigitalTimeBins>       StDigitalPadRow;
typedef std::vector<StDigitalPadRow>         StDigitalSector;

typedef std::vector<StDigitalPair>::iterator StDigitalTimeBinIterator;
typedef StDigitalTimeBins::iterator          StDigitalTimeBinsIterator;
typedef StDigitalPadRow::iterator            StDigitalPadRowIterator;
typedef StDigitalSector::iterator            StDigitalRowIterator;

typedef std::vector<StSequence>              StVecSequence;
typedef std::vector<UShort_t*>               StVecIds;
typedef std::vector<UChar_t>                 StVecPads;
typedef std::vector<UChar_t> 	               StVecUChar;
typedef std::vector<Int_t> 	               StVecInt;
typedef std::vector<StTpcPixel>              StVectPixel;

class StTpcDigitalSector : public StObject {
public:
    StTpcDigitalSector(void *db = 0);
    StTpcDigitalSector(int sector);
    virtual ~StTpcDigitalSector() {}
    // access functions
    const StDigitalTimeBins* timeBinsOfRowAndPad(Int_t rowN, Int_t padN) const { return (&mData[(rowN-1)][(padN-1)]);}
    StDigitalTimeBins*       timeBinsOfRowAndPad(Int_t rowN, Int_t padN)       { return (&mData[(rowN-1)][(padN-1)]);}
    StDigitalPadRow*         padsOfRow(Int_t rowN)                             { return (&mData[(rowN-1)]);}
    StDigitalSector*         rows()                                            { return (&mData);}
    
    Int_t  numberOfRows()             		        const    { return mData.size();}
    Int_t  numberOfPadsInRow(Int_t rowN)		const    { return mData[(rowN-1)].size();}
    Int_t  numberOfTimeBins(Int_t rowN, Int_t padN) 	const    { return mData[(rowN-1)][(padN-1)].size();}
    
    // Adding
    void   assignTimeBins(int row , int pad, StDigitalTimeBins*);
    Int_t  getSequences(Int_t row, Int_t pad, Int_t *nSeq, StSequence** seq, UShort_t ***Id);
    Int_t  getPadList(Int_t row, UChar_t **padList);
    Int_t  getTimeAdc(Int_t row, Int_t pad, Short_t ADCs[__MaxNumberOfTimeBins__], 
		      UShort_t IDTs[__MaxNumberOfTimeBins__]); // with  8 => 10 conversion
    Int_t  getTimeAdc(Int_t row, Int_t pad, UChar_t  ADCs[__MaxNumberOfTimeBins__], 
		      UShort_t IDTs[__MaxNumberOfTimeBins__]);
    Int_t  putTimeAdc(Int_t row, Int_t pad, Short_t *ADCs, UShort_t *IDTs = 0);     // with 10 =>  8 conversion
    Int_t  putTimeAdc(Int_t row, Int_t pad, UChar_t  *ADCs, UShort_t *IDTs = 0);
    void   setSector(Int_t sector) {mSector = sector;} 
    void   clear();
    Int_t  cleanup();
    virtual void   Print(const Option_t *opt="") const;
    virtual Int_t  PrintTimeAdc(Int_t row, Int_t pad) const;
    StTpcDigitalSector &operator+= (StTpcDigitalSector& v);
    Int_t numberOfPadsAtRow(Int_t row) {return (row > 0 && row <= mNoRows) ? St_tpcPadConfigC::instance()->padsPerRow(mSector, row) : 0;}
    StTpcDigitalSector& operator=(const StTpcDigitalSector&);
    Int_t sector() {return mSector;}
    Int_t numberOfRows() {return mNoRows;}
private:
    StTpcDigitalSector(const StTpcDigitalSector&);

private:
    StDigitalSector       mData;
    Int_t                 mSector;
    StVecPads             mPadList;
    StVecSequence         mSequence;
    StVecIds              mIds;
    Int_t                 mNoRows;
    ClassDef(StTpcDigitalSector,2)
};

class StTpcRawData : public StObject {
public:
    StTpcRawData(Int_t noSectors = 24) {setNoSectors(noSectors);}
    virtual ~StTpcRawData() {clear();}
    UInt_t size() {return mSectors.size();}
    UInt_t getNoSectors() {return size();}
    StTpcDigitalSector *GetSector(UInt_t sector) {return sector > 0 && sector <= size() ? mSectors[sector-1] : 0;}
    StTpcDigitalSector *getSector(UInt_t sector) {return GetSector(sector);}
    Int_t  getVecOfPixels(StVectPixel &pixels, Int_t sector, Int_t row, Int_t padMin = 1, Int_t padMax = -1, 
			  Int_t tMin = 0, Int_t tMax = -1);
    void   setNoSectors(UInt_t noSectors = 0) {mSectors.resize(noSectors); for (UInt_t i = 0; i < noSectors; i++) mSectors[i] = 0;} 
    void   setSector(UInt_t sector, StTpcDigitalSector* digitSector);
    void   clear() {Clear();}
    void   Clear(const Option_t *opt = ""); 
    StTpcRawData &operator+= (StTpcRawData& v);
    virtual void Print(const Option_t *opt="") const; 
private:
    std::vector<StTpcDigitalSector*> mSectors;
    ClassDef(StTpcRawData,1)
 };
#endif
