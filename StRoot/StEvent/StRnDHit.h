/*!
 * \class StRnDHit 
 * \author Mike Miller and Andrew Rose, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StRnDHit.h,v 2.3 2017/05/04 01:06:46 perev Exp $
 *
 * Author: Mike Miller and Andrew Rose, Jan 2006
 ***************************************************************************
 *
 * Description:  This is an experimental class and not final yet
 *
 ***************************************************************************
 *
 * $Log: StRnDHit.h,v $
 * Revision 2.3  2017/05/04 01:06:46  perev
 * Own err matrix added
 *
 * Revision 2.2  2006/09/27 18:31:43  ullrich
 * Fixed setDouble() interface. Was sooo wrong.
 *
 * Revision 2.1  2006/01/19 21:42:06  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StRnDHit_hh
#define StRnDHit_hh
#include "StHit.h"
#include "StMemoryPool.hh"
#include "StEnumerations.h"

class StRnDHit : public StHit
{
public:
    StRnDHit();
    StRnDHit(const StThreeVectorF& position,
	   const StThreeVectorF& error,
	   unsigned int hwPosition, float charge, unsigned char trackRefCount = 0,
	   unsigned short idTruth=0,  unsigned short quality=0,  unsigned short id =0,
	   StDetectorId = kUnknownId);
    ~StRnDHit();

    StDetectorId detector() const;
    unsigned int    layer() const;
    unsigned int    ladder() const;
    unsigned int    wafer() const;
    
    int    extraByte0() const;
    int    extraByte1() const;
    
    int    key() const;
    int    volumeId() const;
    
    double double0() const;
    double double1() const;
    double double2() const;
    double double3() const;
    double double4() const;
    
    void setLayer(short);
    void setLadder(short);
    void setWafer(short);
    void setExtraByte0(int);
    void setExtraByte1(int);
    void setDetectorId(StDetectorId);
    
    void setKey(int);
    void setVolumeId(int);
    
    void setDouble0(double);
    void setDouble1(double);
    void setDouble2(double);
    void setDouble3(double);
    void setDouble4(double);

  void setErrorMatrix(const float* M);

  StMatrixF       covariantMatrix() const;//{ return mErrMatrix; }

    void* operator new(size_t sz,void *p)     { return p;}
    void* operator new(size_t)     { return mPool.alloc(); }
    void  operator delete(void* p) { mPool.free(p); }
    
    friend ostream& operator<<(ostream& os, const StRnDHit& h);
    
protected:
    Short_t mLayer; ///Layer
    Short_t mLadder; ///Ladder
    Short_t mWafer; ///Wafer
    
    //Extras
    Int_t mExtraByte0;
    Int_t mExtraByte1;
    
    //info to get back to StMcHit pointer:
    Int_t mKey; ///key from StMcHit
    Int_t mVolumeId; ///VolumeId from StMcHit
    
    //and 5 overflow doubles
    Double_t mDouble0;
    Double_t mDouble1;
    Double_t mDouble2;
    Double_t mDouble3;
    Double_t mDouble4;

    // this has to go once the playing and testing is over.
    // should be hard wired in member function.
    StDetectorId mDetectorId;

  float mErrorMatrix[9];
    
    static StMemoryPool mPool;  //!
    
    ClassDef(StRnDHit,2)        
};

inline unsigned int  StRnDHit::layer() const {return mLayer;}
inline unsigned int  StRnDHit::ladder() const {return mLadder;}
inline unsigned int  StRnDHit::wafer() const {return mWafer;}
inline int    StRnDHit::extraByte0() const {return mExtraByte0;}
inline int    StRnDHit::extraByte1() const {return mExtraByte1;}
inline int    StRnDHit::key() const {return mKey;}
inline int    StRnDHit::volumeId() const {return mVolumeId;}
inline double StRnDHit::double0() const {return mDouble0;}
inline double StRnDHit::double1() const {return mDouble1;}
inline double StRnDHit::double2() const {return mDouble2;}
inline double StRnDHit::double3() const {return mDouble3;}
inline double StRnDHit::double4() const {return mDouble4;}
    
inline void StRnDHit::setLayer(short v) {mLayer = v;}
inline void StRnDHit::setLadder(short v) {mLadder = v;}
inline void StRnDHit::setWafer(short v) {mWafer = v;}
inline void StRnDHit::setExtraByte0(int v) {mExtraByte0=v;}
inline void StRnDHit::setExtraByte1(int v) {mExtraByte1=v;}
inline void StRnDHit::setKey(int v) {mKey = v;}
inline void StRnDHit::setVolumeId(int v) {mVolumeId=v;}
inline void StRnDHit::setDouble0(double val) {mDouble0 = val;}
inline void StRnDHit::setDouble1(double val) {mDouble1 = val;}
inline void StRnDHit::setDouble2(double val) {mDouble2 = val;}
inline void StRnDHit::setDouble3(double val) {mDouble3 = val;}
inline void StRnDHit::setDouble4(double val) {mDouble4 = val;}

#endif
