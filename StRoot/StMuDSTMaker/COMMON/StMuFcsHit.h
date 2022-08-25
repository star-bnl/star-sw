/***************************************************************************
 *
 * $Id: StMuFcsHit.h,v 1.0 2021/11/17 16:07:31 jdb Exp $
 *
 * Author: Daniel Brandenburg, 2021
 ***************************************************************************
 *
 * Description: StMuFcsHit is data for individual cell 
 *
 ***************************************************************************/

#ifndef StMuFcsHit_hh
#define StMuFcsHit_hh

#include <TObject.h>
#include "TArrayS.h"
#include "Stiostream.h"
#include <TRef.h>

class StMuFcsCluster;

class StMuFcsHit : public TObject {
public:
    StMuFcsHit();
    StMuFcsHit(unsigned short zs, unsigned short det, unsigned short id,
             unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
             int ntimebin, unsigned short* data);
    StMuFcsHit(unsigned short zs, unsigned short det, unsigned short id,
             unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
             float e);
    ~StMuFcsHit();

    virtual void Clear (Option_t * opt=""){
        mData.Set(0);
    }
    
    unsigned short zs() const;
    unsigned short detectorId() const;
    unsigned short id() const; 
    unsigned short ns() const;         //from DEP
    unsigned short ehp() const;        //from DEP
    unsigned short dep() const;        //from DEP
    unsigned short channel() const;    //from DEP
    unsigned int   nTimeBin() const;
    unsigned short timebin(int i) const;
    unsigned short data(int i) const;
    unsigned short * data() const { return (unsigned short*)mData.GetArray();}
    unsigned int nData() const { return mData.GetSize(); };
    unsigned short adc(int i) const;
    unsigned short flag(int i) const;
    int   adcSum() const;
    float fitPeak() const;
    float fitSigma() const;
    float fitChi2() const;
    int   nPeak() const;
    float energy() const;
    
    void setDepCh(unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch);
    void setNS(unsigned short val);
    void setEHP(unsigned short val);
    void setDep(unsigned short val);
    void setChannel(unsigned short val);

    void setDetId(unsigned short zs, unsigned short det, unsigned short id);
    void setZS(unsigned short val);
    void setDetId(unsigned short val);
    void setDetectorId(unsigned short val);
    void setId(unsigned short val);

    void setData(int ndata, const unsigned short* d);
    void setDataAt(int tb, unsigned short val);
    void setAdcFlag(int tb, unsigned short adc, unsigned short flag);
    void setAdc(int tb, unsigned short val);
    void setFlag(int tb, unsigned short val);

    void setAdcSum(int v);
    void setFitPeak(float v);
    void setFitSigma(float v);
    void setFitChi2(float v);
    void setNPeak(int v);
    void setEnergy(float v);

    void setFcsHit(unsigned short zs, unsigned short det, unsigned short id,
                   unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
           int ntimebin, unsigned short* data);
    void setFcsHit(unsigned short zs, unsigned short det, unsigned short id,
                   unsigned short ns, unsigned short ehp, unsigned short dep, unsigned short ch, 
           float e);
    
    void setCluster(StMuFcsCluster* clu);
    const StMuFcsCluster *cluster() const;
    StMuFcsCluster *cluster();

    void print(Option_t *option="") const;

protected:
    UShort_t mDetId=0;        // 1 bit ZS, 3 bits DetectorId, 12 bits id
    UShort_t mDepCh=0;        // 1 bit for NS, 2 bits for EHP, 5 bits for DEP, 8 bits for channal
    UInt_t   mAdcSum=0;       // ADC sum 
    Float_t  mFitPeak=0.0;    // fit peak position
    Float_t  mFitSigma=0.0;   // fit sigma
    Float_t  mFitChi2=0.0;    // fit chi2
    UInt_t   mNPeak=0;        // number of peaks found
    Float_t  mEnergy=0.0;     // corrected energy
    // StMuFcsCluster* mCluster=0; // pointer to cluster this hit belongs
    TRef mCluster;
    TArrayS mData=0;         // 12bit ADC values + flag at highest 4 bits, array of timebin
    
    ClassDef(StMuFcsHit,2)
};

ostream& operator<<(ostream&, const StMuFcsHit&);
#endif
