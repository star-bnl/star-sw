#ifndef STAR_StPmtSignal
#define STAR_StPmtSignal

// $Id: StPmtSignal.h,v 1.3 2007/09/11 21:49:15 kocolosk Exp $

#include "TNamed.h"
#include "TRandom3.h"

/*****************************************************************************
 * @class StPmtSignal
 * @author V.Rykov -> A.Pavlinov -> A.Suaide -> A.Kocoloski
 *
 * Simulation of the digitized PMT signals with a given number of 
 * photoelectrons emerging from a photocathode, taking into account statistics
 * of secondary electrons and noise.  Use this class by instantiating an
 * object, setting the total gain (ADC counts / photoElectron emitted from
 * cathode) and pedestal properties, and then calling getAdc for a given # of
 * photoelectrons. 
 *****************************************************************************/
class StPmtSignal : public TNamed
{
private:
    /// number of PMT amplification stages (dynodes)
    static const int mNumDynodes = 11;
    
    /// Approximate PMT amplification. This parameter is used solely for estimations of the widths of
    /// the signal distributions with the fixed numbers of photoelectrons, and it DOES NOT affect (at
    /// all!) the overall gain which is FULLY DEFINED by the mTotalGain parameter. Actually, even quite
    /// sizeable variations of the "pmtgain" just slightly affect the charateristics of the simu-
    /// lated signals. For example, if "pmtgain" was changed by a factor of 2, it would lead to just
    /// to ~6-7% variation of widths of the "fixed-phe" peaks in a PMT with 10-12 dynodes. Therefore, IT
    /// IS NOT NECESSARY to change "pmtgain" parameter in this routine every time when high-voltage (and
    /// the actual PMT gain) is corrected. For most application, the accuracy should be sufficient if
    /// "pmtgain" is kept just "somewhere in the range deviating by no more than 2-3 times from the
    /// actual amplification of the PMT which is intended to be simulated.
    float mPmtGain;
    
    /// A probability for a thermal electron (or the average number of thermal electrons per pulse) to
    /// spontaneously emerge from the photocathode within the ADC gate.
    float mCathodeNoise;
    
    /// A probability for a thermal electron (or the average number of thermal electrons per pulse) to
    /// spontaneously emerge from a dynode within the ADC gate. This probability is assumed to be the same
    /// for all dynodes.
    float mDynodeNoise;
    
    /// Overall gain of the chain "PMT-...-ADC", ADC-counts/phe.  Must be supplied by the user.
    float mTotalGain;
    
    /// pedestal in ADCs.  Must be supplied by the user.
    float mPedestalMean;
    
    /// sigma of the Gaussian pedestal noise in ADCs.  Must be supplied by the user.
    float mPedestalRMS;
    
    
    /// Internal relative voltage distribution between dynodes, starting from the "photocathode-dynode#1" gap.
    float mNodeVoltage[mNumDynodes];
    
    /// Internal secondary electron conversion coefficients
    float mSecondaryCoeff[mNumDynodes];
    
    /// Internal inverted partial gains after each dynode
    float mDynodeGain[mNumDynodes + 1];
    
    /// Internal variable only used in fast simulator
    float mG1[mNumDynodes + 1];
    
    /// Internal variable only used in fast simulator
    float mDNW[mNumDynodes + 1];
    
    /// Internal random number generator used to obtain Poisson and normal distributions
    TRandom3  mRandom;
    
public:
    StPmtSignal(float pmtGain=1.5e+6, float cathodeNoise=0.0, float dynodeNoise=0.0);
    ~StPmtSignal() { /* Nothing */ }
    
    /// fastSimulator approximates mean>100 Poisson distributions by Gaussians.  Statistically
    /// consistent with fullSimulator at statistics up to 10^5 and an agreement is 
    /// acceptable up to 10^6.
    enum simulatorVersion { kFastSimulator, kFullSimulator };
    
    /// set the gain of the full PMT chain in ADC counts / photoelectron emitted from cathode
    void setTotalGain(float val) { mTotalGain = val; }
    
    /// set the pedestal in ADC counts
    void setPedestalMean(float val) { mPedestalMean = val; }
    
    /// set the width of the pedestal in ADC counts
    void setPedestalRMS(float val) { mPedestalRMS = val; }
    
    /// return ADC counts for a given # of incident photoelectrons
    int getAdc(int nPhotoElectrons, simulatorVersion version);

    ClassDef(StPmtSignal, 2)
};

#endif

/*****************************************************************************
 * $Log: StPmtSignal.h,v $
 * Revision 1.3  2007/09/11 21:49:15  kocolosk
 * complete overhaul of the BEMC simulator
 * http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *****************************************************************************/
