/***************************************************************************
 *
 * $Id: StMuFcsHit.cxx,v 1.0 2021/11/17 03:57:39 jdb Exp $
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: StMuFcsHit is data for individual cell 
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StMuFcsHit.h"
#include "TString.h"

#include "StMuFcsCluster.h"
ClassImp(StMuFcsHit)

StMuFcsHit::StMuFcsHit() { /* no operation */}

StMuFcsHit::StMuFcsHit(unsigned short zs, unsigned short det, unsigned short id,
                   unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
           int ntimebin, unsigned short* data)
{
    setFcsHit(zs, det, id, ns, ehp, dep, ch, ntimebin, data);
} 
StMuFcsHit::StMuFcsHit(unsigned short zs, unsigned short det, unsigned short id,
                   unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
           float e)
{
    setFcsHit(zs, det, id, ns, ehp, dep, ch, e);
} 

StMuFcsHit::~StMuFcsHit() {
    mData.Set(0);
}

unsigned short StMuFcsHit::zs()         const {return (mDetId >> 15 ) & 0x0001;}
unsigned short StMuFcsHit::detectorId() const {return (mDetId >> 12 ) & 0x0007;}
unsigned short StMuFcsHit::id()         const {return (mDetId       ) & 0x0fff;}
unsigned short StMuFcsHit::ns()         const {return (mDepCh >> 15 ) & 0x01;}
unsigned short StMuFcsHit::ehp()        const {return (mDepCh >> 13 ) & 0x03;}
unsigned short StMuFcsHit::dep()        const {return (mDepCh >> 8  ) & 0x1f;}
unsigned short StMuFcsHit::channel()    const {return (mDepCh       ) & 0xff;}
unsigned int StMuFcsHit::nTimeBin()     const {
    if(zs()) return mData.GetSize()/2;
    return mData.GetSize();
}
unsigned short StMuFcsHit::data(int i) const {return mData.At(i);}
unsigned short StMuFcsHit::timebin(int i) const {
    if(zs()) return mData.At(i*2+1);
    return i;
}
unsigned short StMuFcsHit::adc(int i) const {
    if(zs()) return mData.At(i*2  ) & 0xfff;
    return mData.At(i) & 0xfff;
}
unsigned short StMuFcsHit::flag(int i) const {
    if(zs()) return mData.At(i*2  ) >> 12;
    return mData.At(i) >> 12;
}

int   StMuFcsHit::adcSum()   const {return mAdcSum;}
float StMuFcsHit::fitPeak()  const {return mFitPeak;}
float StMuFcsHit::fitSigma() const {return mFitSigma;}
float StMuFcsHit::fitChi2()  const {return mFitChi2;}
int   StMuFcsHit::nPeak()    const {return mNPeak;}
float StMuFcsHit::energy()   const {return mEnergy;}

void StMuFcsHit::setDepCh(unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch) { 
    mDepCh = (ns << 15) | (ehp<<13) | (dep<<8) | ch; 
}
void StMuFcsHit::setNS(unsigned short val)       { setDepCh(val,ehp(),dep(),channel());}
void StMuFcsHit::setEHP(unsigned short val)      { setDepCh(ns(),val,dep(),channel());}
void StMuFcsHit::setDep(unsigned short val)      { setDepCh(ns(),ehp(),val,channel());}
void StMuFcsHit::setChannel(unsigned short val)  { setDepCh(ns(),ehp(),dep(),val);}

void StMuFcsHit::setDetId(unsigned short zs, unsigned short det, unsigned short id) { 
    mDetId = (zs & 0x1)<<15 | (det & 0x7)<<12 | (id & 0xfff); 
}
void StMuFcsHit::setZS(unsigned short val)                       { setDetId(val,detectorId(),id()); }
void StMuFcsHit::setDetectorId(unsigned short val)               { setDetId(zs(),val,id()); }
void StMuFcsHit::setId(unsigned short val)                       { setDetId(zs(),detectorId(),val); }

void StMuFcsHit::setData(int ndata, const unsigned short* data) {
    mData.Set(ndata,(const short*)data);
}
void StMuFcsHit::setDataAt(int i, unsigned short val)                       { mData.AddAt(val,i); }
void StMuFcsHit::setAdcFlag(int i, unsigned short adc, unsigned short flag) { mData.AddAt(((flag&0xf)<<12) + adc, i); }
void StMuFcsHit::setAdc(int i, unsigned short val)                          { setAdcFlag(i,val,flag(i)); }
void StMuFcsHit::setFlag(int i, unsigned short val)                         { setAdcFlag(i,adc(i),val); }

void StMuFcsHit::setAdcSum(int val)              { mAdcSum   = val; }
void StMuFcsHit::setFitPeak(float val)           { mFitPeak  = val; }
void StMuFcsHit::setFitSigma(float val)          { mFitSigma = val; }
void StMuFcsHit::setFitChi2(float val)           { mFitChi2  = val; }
void StMuFcsHit::setNPeak(int val)               { mNPeak    = val; }
void StMuFcsHit::setEnergy(float val)            { mEnergy   = val; }

void StMuFcsHit::setFcsHit(unsigned short zs, unsigned short det, unsigned short id,
             unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
             int ntimebin, unsigned short* data) {
    setDetId(zs, det,id);
    setDepCh(ns,ehp,dep,ch);
    setData(ntimebin,data);
}
void StMuFcsHit::setFcsHit(unsigned short zs, unsigned short det, unsigned short id,
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

void StMuFcsHit::print(Option_t *option) const {
    cout << Form("StMuFcsHit: det=%2d id=%3d | ns=%1d ehp=%1d dep=%2d ch=%2d | Ntb=%3d Sum=%6d Fit=%6.2f %6.2f E=%6.2f | ",
         detectorId(),id(),ns(),ehp(),dep(),channel(),
         nTimeBin(),adcSum(),fitPeak(),fitSigma(),energy());
    for(unsigned int i=0; i<nTimeBin(); i++) {
    //if(timebin(i)>30 && timebin(i)<60)
    cout << Form("%4d (%3d) ",adc(i),timebin(i));
    }
    cout << endl;
}


void StMuFcsHit::setCluster( StMuFcsCluster* cluster ){
    mCluster = cluster;
}

StMuFcsCluster* StMuFcsHit::cluster() {
    return static_cast<StMuFcsCluster*>( mCluster.GetObject() );
}

const StMuFcsCluster* StMuFcsHit::cluster() const {
    return static_cast<StMuFcsCluster*>( mCluster.GetObject() );
}