/***************************************************************************
 *
 * $Id: StTpcRawData.h,v 2.2 2008/04/24 16:06:25 fisyak Exp $
 *
 * Author: Yuri Fisyak, Mar 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcRawData.h,v $
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
#define __NumberOfRows__ 45
#define __MaxNumberOfTimeBins__ 512
typedef std::vector<UChar_t>  StVectorADC;
typedef std::vector<UShort_t> StVectorIDT;

class StDigitalPair {
public:
    StDigitalPair(UShort_t time=0)      {mTime=time;}
    void add(UChar_t adc)               {mAdc.push_back(adc);}
    void add(UChar_t adc,Int_t idt)     {mAdc.push_back(adc); mIdt.push_back(idt);}
    
    UChar_t* adc()   const {return (UChar_t*)&mAdc[0];}			
    Bool_t   isIdt() const {return mAdc.size() == mIdt.size();}
    UShort_t*idt()   const {return (UShort_t*) (isIdt() ? &mIdt[0] : 0);}			
    Int_t    size()  const {return mAdc.size();}
    UShort_t time()  const {return mTime;}

private:
    UShort_t    mTime;
    StVectorADC mAdc;
    StVectorIDT mIdt; 
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
typedef std::vector<UChar_t> 	             StVecUChar;
typedef std::vector<Int_t> 	             StVecInt;
typedef std::vector<StTpcPixel>              StVectPixel;

class StTpcDigitalSector : public StObject {
public:
    StTpcDigitalSector(void *db = 0);
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
    void   assignTimeBins(int, int, StDigitalTimeBins*);
    Int_t  getSequences(Int_t row, Int_t pad, Int_t *nSeq, StSequence** seq, UShort_t ***Id);
    Int_t  getPadList(Int_t row, UChar_t **padList);
    Int_t  getTimeAdc(Int_t row, Int_t pad, UShort_t ADCs[__MaxNumberOfTimeBins__], 
		      UShort_t IDTs[__MaxNumberOfTimeBins__]); // with  8 => 10 conversion
    Int_t  getTimeAdc(Int_t row, Int_t pad, UChar_t  ADCs[__MaxNumberOfTimeBins__], 
		      UShort_t IDTs[__MaxNumberOfTimeBins__]);
    Int_t  putTimeAdc(Int_t row, Int_t pad, UShort_t *ADCs, UShort_t *IDTs = 0);     // with 10 =>  8 conversion
    Int_t  putTimeAdc(Int_t row, Int_t pad, UChar_t  *ADCs, UShort_t *IDTs = 0);
    void   setSector(Int_t sector) {mSector = sector;} 
    void   clear();
    Int_t  cleanup();
    virtual void   Print(const Option_t *opt="") const;
    StTpcDigitalSector &operator+= (StTpcDigitalSector& v);
    static Int_t numberOfPadsAtRow(Int_t row);
private:
    StTpcDigitalSector(const StTpcDigitalSector&);
    StTpcDigitalSector& operator=(const StTpcDigitalSector&);

private:
    StDigitalSector       mData;
    Int_t                 mSector;
    StVecPads             mPadList;
    StVecSequence         mSequence;
    StVecIds              mIds;
    
    ClassDef(StTpcDigitalSector,1)
};

class StTpcRawData : public StObject {
public:
    StTpcRawData(Int_t noSectors = 0) {setNoSectors(noSectors);}
    virtual ~StTpcRawData() {clear();}
    UInt_t size() {return mSectors.size();}
    UInt_t getNoSectors() {return size();}
    StTpcDigitalSector *GetSector(UInt_t sector) {return sector > 0 && sector <= size() ? mSectors[sector-1] : 0;}
    StTpcDigitalSector *getSector(UInt_t sector) {return GetSector(sector);}
    Int_t  getVecOfPixels(StVectPixel &pixels, Int_t sector, Int_t row, Int_t padMin, Int_t padMax, Int_t tMin, Int_t tMax);
    void   setNoSectors(UInt_t noSectors = 0) {mSectors.resize(noSectors); for (UInt_t i = 0; i < noSectors; i++) mSectors[i] = 0;} 
    void   setSector(UInt_t sector, StTpcDigitalSector* digitSector);
    void   clear() {Clear();}
    void   Clear(const Option_t *opt = ""); 
    virtual void Print(const Option_t *opt="") const; 
    StTpcRawData& operator+=(StTpcRawData& v);
private:
    std::vector<StTpcDigitalSector*> mSectors;
    ClassDef(StTpcRawData,1)
 };
#endif
