/***************************************************************************
 *
 * $Id: StFcsHit.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: Akio Ogawa, Aug 2018
 ***************************************************************************
 *
 * Description: StFcsHit is data for individual cell 
 *
 ***************************************************************************
 *
 * $Log: StFcsHit.cxx,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFcsHit.h"

ClassImp(StFcsHit)

StFcsHit::StFcsHit() { /* no operation */}

StFcsHit::StFcsHit(unsigned short zs, unsigned short det, unsigned short id,
                   unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
		   int ntimebin, unsigned short* data)
{
    setFcsHit(zs, det, id, ns, ehp, dep, ch, ntimebin, data);
} 
StFcsHit::StFcsHit(unsigned short zs, unsigned short det, unsigned short id,
                   unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
		   float e)
{
    setFcsHit(zs, det, id, ns, ehp, dep, ch, e);
} 

StFcsHit::~StFcsHit() {
    if(mData) delete mData;
}

unsigned short StFcsHit::zs()         const {return (mDetId >> 15 ) & 0x0001;}
unsigned short StFcsHit::detectorId() const {return (mDetId >> 12 ) & 0x0007;}
unsigned short StFcsHit::id()         const {return (mDetId       ) & 0x0fff;}
unsigned short StFcsHit::ns()         const {return (mDepCh >> 15 ) & 0x01;}
unsigned short StFcsHit::ehp()        const {return (mDepCh >> 13 ) & 0x03;}
unsigned short StFcsHit::dep()        const {return (mDepCh >> 8  ) & 0x1f;}
unsigned short StFcsHit::channel()    const {return (mDepCh       ) & 0xff;}
unsigned int StFcsHit::nTimeBin()     const {
    if(mData) {
      if(zs()) return mData->GetSize()/2;
      return mData->GetSize();
    }
    return 0;
}
unsigned short StFcsHit::data(int i) const {return mData->At(i);}
unsigned short StFcsHit::timebin(int i) const {
    if(zs()) return mData->At(i*2+1);
    return i;
}
unsigned short StFcsHit::adc(int i) const {
    if(zs()) return mData->At(i*2  ) & 0xfff;
    return mData->At(i) & 0xfff;
}
unsigned short StFcsHit::flag(int i) const {
    if(zs()) return mData->At(i*2  ) >> 12;
    return mData->At(i) >> 12;
}

int   StFcsHit::adcSum()   const {return mAdcSum;}
float StFcsHit::fitPeak()  const {return mFitPeak;}
float StFcsHit::fitSigma() const {return mFitSigma;}
float StFcsHit::fitChi2()  const {return mFitChi2;}
int   StFcsHit::nPeak()    const {return mNPeak;}
float StFcsHit::energy()   const {return mEnergy;}

void StFcsHit::setDepCh(unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch) { 
    mDepCh = (ns << 15) | (ehp<<13) | (dep<<8) | ch; 
}
void StFcsHit::setNS(unsigned short val)       { setDepCh(val,ehp(),dep(),channel());}
void StFcsHit::setEHP(unsigned short val)      { setDepCh(ns(),val,dep(),channel());}
void StFcsHit::setDep(unsigned short val)      { setDepCh(ns(),ehp(),val,channel());}
void StFcsHit::setChannel(unsigned short val)  { setDepCh(ns(),ehp(),dep(),val);}

void StFcsHit::setDetId(unsigned short zs, unsigned short det, unsigned short id) { 
    mDetId = (zs & 0x1)<<15 | (det & 0x7)<<12 | (id & 0xfff); 
}
void StFcsHit::setZS(unsigned short val)                       { setDetId(val,detectorId(),id()); }
void StFcsHit::setDetectorId(unsigned short val)               { setDetId(zs(),val,id()); }
void StFcsHit::setId(unsigned short val)                       { setDetId(zs(),detectorId(),val); }

void StFcsHit::setData(int ntimebin, const unsigned short* data) {
    if(!mData) {
        mData = new TArrayS(ntimebin,(const short*)data);
    }
    else {
        mData->Set(ntimebin,(const short*)data);
  }
}
void StFcsHit::setDataAt(int i, unsigned short val)                       { mData->AddAt(val,i); }
void StFcsHit::setAdcFlag(int i, unsigned short adc, unsigned short flag) { mData->AddAt(((flag&0xf)<<12) + adc, i); }
void StFcsHit::setAdc(int i, unsigned short val)                          { setAdcFlag(i,val,flag(i)); }
void StFcsHit::setFlag(int i, unsigned short val)                         { setAdcFlag(i,adc(i),val); }

void StFcsHit::setAdcSum(int val)              { mAdcSum   = val; }
void StFcsHit::setFitPeak(float val)           { mFitPeak  = val; }
void StFcsHit::setFitSigma(float val)          { mFitSigma = val; }
void StFcsHit::setFitChi2(float val)           { mFitChi2  = val; }
void StFcsHit::setNPeak(int val)               { mNPeak    = val; }
void StFcsHit::setEnergy(float val)            { mEnergy   = val; }

void StFcsHit::setFcsHit(unsigned short zs, unsigned short det, unsigned short id,
			 unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
			 int ntimebin, unsigned short* data) {
    setDetId(zs, det,id);
    setDepCh(ns,ehp,dep,ch);
    setData(ntimebin,data);
}
void StFcsHit::setFcsHit(unsigned short zs, unsigned short det, unsigned short id,
			 unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
			 float e) {
    setDetId(zs, det,id);
    setDepCh(ns,ehp,dep,ch);
    unsigned short data[2]={0,0};
    if(zs==0) {
        setData(1,data);
    }
    else {
        setData(2,data);
    }
    setEnergy(e);  
}

void StFcsHit::print(Option_t *option) const {
    cout << Form("StFcsHit: det=%2d id=%3d | ns=%1d ehp=%1d dep=%2d ch=%2d | Ntb=%3d Sum=%6d Fit=%6.2f %6.2f E=%6.2f | ",
		 detectorId(),id(),ns(),ehp(),dep(),channel(),
		 nTimeBin(),adcSum(),fitPeak(),fitSigma(),energy());
    for(unsigned int i=0; i<nTimeBin(); i++) {
	//if(timebin(i)>30 && timebin(i)<60)
	cout << Form("%4d (%3d) ",adc(i),timebin(i));
    }
    cout << endl;
}
