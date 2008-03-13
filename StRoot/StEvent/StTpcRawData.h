/***************************************************************************
 *
 * $Id: StTpcRawData.h,v 2.1 2008/03/13 16:42:24 ullrich Exp $
 *
 * Author: Yuri Fisyak, Mar 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcRawData.h,v $
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

typedef std::vector<unsigned char>  StVectorADC;
typedef std::vector<unsigned short> StVectorIDT;

class StDigitalPair {
public:
    StDigitalPair(unsigned short time=0)        {mTime=time;}
    void add(unsigned char adc)               {mAdc.push_back(adc);}
    void add(unsigned char adc,int idt)     {mAdc.push_back(adc); mIdt.push_back(idt);}
    
    unsigned char* adc()   const {return (unsigned char*)&mAdc[0];}			
    Bool_t   isIdt() const {return mAdc.size() == mIdt.size();}
    unsigned short*idt()   const {return (unsigned short*) (isIdt() ? &mIdt[0] : 0);}			
    int    size()  const {return mAdc.size();}
    unsigned short time()  const {return mTime;}

private:
    UShort_t  mTime;
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

typedef std::vector<StSequence> StVecSequence;
typedef std::vector<UShort_t*>  StVecIds;
typedef std::vector<UChar_t>    StVecPads;
typedef std::vector<UChar_t> 	  StVecUChar;
typedef std::vector<Int_t> 	  StVecInt;
typedef std::vector<StTpcPixel> StVectPixel;

class StTpcDigitalSector : public StObject {
public:
    StTpcDigitalSector(void *db = 0);
    virtual ~StTpcDigitalSector() {}
    // access functions
    const StDigitalTimeBins* timeBinsOfRowAndPad(int rowN, int padN) const { return (&mData[(rowN-1)][(padN-1)]);}
    StDigitalTimeBins*   timeBinsOfRowAndPad(int rowN, int padN) { return (&mData[(rowN-1)][(padN-1)]);}
    StDigitalPadRow*     padsOfRow(int rowN)                     { return (&mData[(rowN-1)]);}
    StDigitalSector*     rows()                                  { return (&mData);}
    
    int  numberOfRows()             		const    { return mData.size();}
    int  numberOfPadsInRow(int rowN)		const    { return mData[(rowN-1)].size();}
    int  numberOfTimeBins(int rowN, int padN) 	const    { return mData[(rowN-1)][(padN-1)].size();}
    
    // Adding
    void assignTimeBins(int, int, StDigitalTimeBins*);
    int  getSequences(int row, int pad, int *nSeq, StSequence** seq, unsigned short ***Id);
    int  getPadList(int row, unsigned char **padList);
    int  getTimeAdc(int row, int pad, unsigned short ADCs[512], unsigned short IDTs[512]); // with  8 => 10 conversion
    int  getTimeAdc(int row, int pad, unsigned char  ADCs[512], unsigned short IDTs[512]);
    int  putTimeAdc(int row, int pad, unsigned short *ADCs, unsigned short *IDTs = 0);     // with 10 =>  8 conversion
    int  putTimeAdc(int row, int pad, unsigned char  *ADCs, unsigned short *IDTs = 0);
    void setSector(int sector) {mSector = sector;} 
    void clear();
    int  cleanup();
    virtual void   Print(const Option_t *opt="") const;
    StTpcDigitalSector &operator+= (StTpcDigitalSector& v);
    
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
    StTpcRawData(int noSectors = 0) {setNoSectors(noSectors);}
    virtual ~StTpcRawData() {clear();}
    unsigned int size() {return mSectors.size();}
    unsigned int getNoSectors() {return size();}
    StTpcDigitalSector *GetSector(unsigned int sector) {return sector > 0 && sector <= size() ? mSectors[sector-1] : 0;}
    StTpcDigitalSector *getSector(unsigned int sector) {return GetSector(sector);}
    int    getVecOfPixels(StVectPixel &pixels, int sector, int row, int padMin, int padMax, int tMin, int tMax);
    void   setNoSectors(unsigned int noSectors = 0) {mSectors.resize(noSectors); for (unsigned int i = 0; i < noSectors; i++) mSectors[i] = 0;} 
    void   setSector(unsigned int sector, StTpcDigitalSector* digitSector);
    void   clear() {Clear();}
    void   Clear(const Option_t *opt = ""); 
    virtual void Print(const Option_t *opt="") const; 
    StTpcRawData& operator+=(StTpcRawData& v);

private:
    std::vector<StTpcDigitalSector*> mSectors;
    ClassDef(StTpcRawData,1)
 };
#endif
