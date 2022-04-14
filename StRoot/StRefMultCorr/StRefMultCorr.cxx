// C++ headers
#include <algorithm>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <limits>

// StRefMultCorr headers
#include "StRefMultCorr.h"
#include "Param.h"

// ROOT headers
#include "TError.h"
#include "TRandom.h"
#include "TMath.h"

ClassImp(StRefMultCorr)

namespace {
  typedef std::pair<Double_t, Int_t> keys;
}

//_________________
// Default constructor
StRefMultCorr::StRefMultCorr(const TString name, const TString subname,
                             const TString libname) : 
                             mName(name), mSubName(subname), mLibName(libname) {
  mRefMult = 0 ;
  mVz = -9999. ;
  mRefMult_corr = -1.0;
  mIsRu = false;
  mIsZr = false;

  std::cout << mSubName.Data() <<"  "<< mLibName.Data() << std::endl;

  // Clear all data members
  clear() ;

  readHeaderFile() ;
  readBadRunsFromHeaderFile() ;
}

//_________________
// Default destructor
StRefMultCorr::~StRefMultCorr() {
  /* empty */
}

//_________________
Int_t StRefMultCorr::getBeginRun(const Double_t energy, const Int_t year) {
  keys key(std::make_pair(energy, year));

  // Make sure key exists
  std::multimap<keys, Int_t>::iterator iterCheck = mBeginRun.find(key);
  if ( iterCheck == mBeginRun.end() ) {
    Error("StRefMultCorr::getBeginRun", "can't find energy = %1.1f, year = %d", energy, year);
    return -1;
  }

  std::pair<std::multimap<keys, Int_t>::iterator, std::multimap<keys, Int_t>::iterator> iterRange = mBeginRun.equal_range(key);

  return (*(iterRange.first)).second ;
}

//________________
Int_t StRefMultCorr::getEndRun(const Double_t energy, const Int_t year) {
  keys key(std::make_pair(energy, year));

  // Make sure key exists
  std::multimap<keys, Int_t>::iterator iterCheck = mEndRun.find(key);
  if (iterCheck == mEndRun.end()) {
    Error("StRefMultCorr::getEndRun", "can't find energy = %1.1f, year = %d", energy, year);
    return -1;
  }

  std::pair<std::multimap<keys, Int_t>::iterator, std::multimap<keys, Int_t>::iterator> iterRange = mEndRun.equal_range(key);
  std::multimap<keys, Int_t>::iterator iter = iterRange.second;
  iter--;

  return (*iter).second;
}

//_________________
void StRefMultCorr::clear() {
  // Clear all arrays, and set parameter index = -1

  mYear.clear() ;
  mStart_runId.clear() ;
  mStop_runId.clear() ;
  mStart_zvertex.clear() ;
  mStop_zvertex.clear() ;
  mNormalize_stop.clear() ;

  for(Int_t i=0; i<mNCentrality; i++) {
    mCentrality_bins[i].clear() ;
  }
	
  mParameterIndex = -1 ;

  for(Int_t i=0;i<mNPar_z_vertex;i++) {
    mPar_z_vertex[i].clear() ;
  }

  for(Int_t i=0;i<mNPar_weight;i++) {
    mPar_weight[i].clear();
  }

  for(Int_t i=0;i<mNPar_luminosity;i++) {
    mPar_luminosity[i].clear();
  }

  mBeginRun.clear() ;
  mEndRun.clear() ;
  mBadRun.clear() ;

  mnVzBinForWeight = 0 ;
  mVzEdgeForWeight.clear();
  mgRefMultTriggerCorrDiffVzScaleRatio.clear() ;
}

//_________________
Bool_t StRefMultCorr::isBadRun(const Int_t RunId) {
  // Return true if a given run id is bad run
  std::vector<Int_t>::iterator iter = std::find(mBadRun.begin(), mBadRun.end(), RunId);
#if 0
  if ( iter != mBadRun.end() ) {
    // QA
    std::cout << "StRefMultCorr::isBadRun  Find bad run = " << (*iter) << std::endl;
  }
#endif

  return ( iter != mBadRun.end() ) ;
}

//_________________
void StRefMultCorr::initEvent(const UShort_t RefMult, const Double_t z, 
                              const Double_t zdcCoincidenceRate) {
  // Set refmult, vz and corrected refmult if current (refmult,vz) are different from inputs
  // User must call this function event-by-event before 
  // calling any other public functions
  if ( mRefMult != RefMult || mVz != z || mZdcCoincidenceRate != zdcCoincidenceRate ) {
    mRefMult            = RefMult ;
    mVz                 = z ;
    mZdcCoincidenceRate = zdcCoincidenceRate ;
    mRefMult_corr       = getRefMultCorr(mRefMult, mVz, mZdcCoincidenceRate) ;
  }
}

//_________________
Bool_t StRefMultCorr::passnTofMatchRefmultCut(Double_t refmult, Double_t ntofmatch, 
                                              Double_t vz /* default = 0*/) const {

  if( mParameterIndex>=30 && mParameterIndex<=35 ) { // Au+Au 27 GeV 2018
    const Double_t min = 4.0;
    const Double_t max = 5.0;

    if(ntofmatch<=2)  return false;

    const double a0 = -0.704787625248525;
    const double a1 = 0.99026234637141;
    const double a2 = -0.000680713101607504;
    const double a3 = 2.77035215460421e-06;
    const double a4 = -4.04096185674966e-09;
    const double b0 = 2.52126730672253;
    const double b1 = 0.128066911940844;
    const double b2 = -0.000538959206681944;
    const double b3 = 1.21531743671716e-06;
    const double b4 = -1.01886685404478e-09;
    const double c0 = 4.79427731664144;
    const double c1 = 0.187601372159186;
    const double c2 = -0.000849856673886957;
    const double c3 = 1.9359155975421e-06;
    const double c4 = -1.61214724626684e-09;

    double refmultCenter = a0+a1*(ntofmatch)+a2*pow(ntofmatch,2)+a3*pow(ntofmatch,3)+a4*pow(ntofmatch,4);
    double refmultLower  = b0+b1*(ntofmatch)+b2*pow(ntofmatch,2)+b3*pow(ntofmatch,3)+b4*pow(ntofmatch,4);
    double refmultUpper  = c0+c1*(ntofmatch)+c2*pow(ntofmatch,2)+c3*pow(ntofmatch,3)+c4*pow(ntofmatch,4);

    double refmultCutMin = refmultCenter - min*refmultLower;
    double refmultCutMax = refmultCenter + max*refmultUpper;
    return ( refmultCutMin < refmult && refmult < refmultCutMax );
  }
  else if( mParameterIndex>=36 && mParameterIndex<=37 ) { // Isobars 200 GeV 2018

    double b0, b1, b2, b3, b4;
    double c0, c1, c2, c3, c4;
    // Zr+Zr
    if(mParameterIndex==36) {
      b0=13.5244327901538;
      b1=1.4429201808933;
      b2=-0.002873496957537;
      b3=7.29172798142226e-06;
      b4=-7.45759942317285e-09;
      c0=-11.2781454979572;
      c1=0.420728494449501;
      c2=0.00184005031913895;
      c3=-4.61008765754698e-06;
      c4=4.28291905929182e-09;
    }
    // Ru+Ru
    else if(mParameterIndex==37) {
      b0=13.5426221755897;
      b1=1.44261201539344;
      b2=-0.00288428931227279;
      b3=7.35384541646783e-06;
      b4=-7.53407759526067e-09;
      c0=-11.2591376113937;
      c1=0.419541462167548;
      c2=0.00185578651291454;
      c3=-4.68933832723005e-06;
      c4=4.4151761900593e-09;
    }
    double refmultcutmax = ( b0 + b1*(ntofmatch) + b2*pow(ntofmatch,2) + b3*pow(ntofmatch,3) + b4*pow(ntofmatch,4) );
    double refmultcutmin = ( c0 + c1*(ntofmatch) + c2*pow(ntofmatch,2) + c3*pow(ntofmatch,3) + c4*pow(ntofmatch,4) );
    return ( refmultcutmin < refmult && refmult<refmultcutmax );
  }
  else if ( mParameterIndex==38 ) { // Au+Au 19.6 GeV 2019
    double b0, b1, b2, b3, b4;
    double c0, c1, c2, c3, c4;

    if ( -145. <= vz && vz < -87. ) {
      b0=33.7732676854599;
      b1=1.75937881368933;
      b2=-0.00285868075552296;
      b3=8.50344260510873e-06;
      b4=-1.19215380537174e-08;
      c0=-22.7060232139884;
      c1=0.863809402986806;
      c2=-0.000368119767293671;
      c3=5.97122714011036e-06;
      c4=-1.27438638584224e-08;
    }
    else if ( -87. <= vz && vz < -29. ) {
      b0=20.5875336291458;
      b1=1.67371122493626;
      b2=-0.00307534477962496;
      b3=7.93755518827246e-06;
      b4=-8.46257293600085e-09;
      c0=-15.5923736275086;
      c1=0.604206551537668;
      c2=0.00131805594121643;
      c3=-2.04753779335401e-06;
      c4=5.73181898751325e-10;
    }
    else if ( -29. <= vz && vz < 29. ) {
      b0=15.1015102672534;
      b1=1.53929151189229;
      b2=-0.00269203062814483;
      b3=6.488759952638e-06;
      b4=-6.06073586314757e-09;
      c0=-13.1157864223955;
      c1=0.504707692972168;
      c2=0.00187997948645203;
      c3=-4.7317012773039e-06;
      c4=4.8234194091071e-09;
    }
    else if ( 29. <= vz && vz < 87. ) {
      b0=20.7718852504153;
      b1=1.67316129891511;
      b2=-0.00315093393202473;
      b3=8.35823870487966e-06;
      b4=-9.14822467807924e-09;
      c0=-15.9411138444366;
      c1=0.61506063963685;
      c2=0.0011824174541949;
      c3=-1.48902496972716e-06;
      c4=-2.29371463231934e-10;
    }
    else if ( 87. <= vz && vz <= 145. ) {
      b0=33.4926150575549;
      b1=1.79372677959986;
      b2=-0.00319461487211403;
      b3=9.56612691680003e-06;
      b4=-1.31049413530369e-08;;
      c0=-22.4679773305418;
      c1=0.83906220918966;
      c2=-0.000106213494253586;
      c3=4.93946486222714e-06;
      c4=-1.1450298089717e-08;
    }

    double refmultcutmax = ( b0 + b1*(ntofmatch) + b2*pow(ntofmatch,2) + b3*pow(ntofmatch,3) + b4*pow(ntofmatch,4) );
    double refmultcutmin = ( c0 + c1*(ntofmatch) + c2*pow(ntofmatch,2) + c3*pow(ntofmatch,3) + c4*pow(ntofmatch,4) );
    return ( refmultcutmin < refmult && refmult < refmultcutmax );
  }
  else {
    return true;
  }
}

//_________________
Bool_t StRefMultCorr::isIndexOk() const {
  // mParameterIndex not initialized (-1)
  if ( mParameterIndex == -1 ) {
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. Call init(const Int_t RunId) function to initialize centrality bins, corrections");
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. or use valid run numbers defined in Centrality_def_%s.txt", mName.Data());
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. exit");
    std::cout << std::endl;
    // Stop the process if invalid run number found
    // exit(0);
    return kFALSE;
  }

  // Out of bounds
  if ( mParameterIndex >= (Int_t)mStart_runId.size() ) {
    Error("StRefMultCorr::isIndexOk",
	  Form("mParameterIndex = %d > max number of parameter set = %d. Make sure you put correct index for this energy",
	       mParameterIndex, mStart_runId.size()));
    return kFALSE ;
  }

  return kTRUE ;
}

//_________________
Bool_t StRefMultCorr::isZvertexOk() const {
  // Primary z-vertex check
  return (mVz > mStart_zvertex[mParameterIndex] &&
          mVz < mStop_zvertex[mParameterIndex]);
}

//_________________
Bool_t StRefMultCorr::isRefMultOk() const {
  // Invalid index
  if ( !isIndexOk() ) return kFALSE ;

  // select 0-80%
  return (mRefMult_corr > mCentrality_bins[0][mParameterIndex] &&
          mRefMult_corr < mCentrality_bins[mNCentrality][mParameterIndex]);
}

//_________________
Bool_t StRefMultCorr::isCentralityOk(const Int_t icent) const {
  // Invalid centrality id
  if ( icent < -1 || icent >= mNCentrality+1 ) return kFALSE ;

  // Invalid index
  if ( !isIndexOk() ) return kFALSE ;

  // Special case
  // 1. 80-100% for icent=-1
  if ( icent == -1 ) return ( mRefMult_corr <= mCentrality_bins[0][mParameterIndex] );

  // 2. icent = mNCentrality
  if ( icent == mNCentrality ) return ( mRefMult_corr <= mCentrality_bins[mNCentrality][mParameterIndex] );

  const Bool_t isOK = ( mRefMult_corr > mCentrality_bins[icent][mParameterIndex] && 
			mRefMult_corr <= mCentrality_bins[icent+1][mParameterIndex] );
  //  if(isOK)
  //  {
  //    std::cout << "StRefMultCorr::isCentralityOk  refmultcorr = " << mRefMult_corr
  //      << "  min. bin = " << mCentrality_bins[icent][mParameterIndex]
  //      << "  max. bin = " << mCentrality_bins[icent+1][mParameterIndex]
  //      << std::endl;
  //  }
  return isOK ;
}

//_________________
void StRefMultCorr::init(const Int_t RunId) {
  // Reset mParameterIndex
  mParameterIndex = -1 ;

  // call setParameterIndex
  setParameterIndex(RunId) ;
}

//_________________
Int_t StRefMultCorr::setParameterIndex(const Int_t RunId) {
  // Determine the corresponding parameter set for the input RunId
  for(UInt_t npar = 0; npar < mStart_runId.size(); npar++) {
    if(RunId >= mStart_runId[npar] && RunId <= mStop_runId[npar]) {
      mParameterIndex = npar ;
      //      std::cout << "StRefMultCorr::setParameterIndex  Parameter set = " << mParameterIndex << " for RUN " << RunId << std::endl;
      break ;
    }
  }
	
  // Multiple parameters/definitions for Run14/16 data
  // Set mParameterIndex by hand
  // For Run14 P16id production
  // For Run16 P16ij production
  if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) { 
    if ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 ) {
      if(RunId/1000000==15 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 ) {
        mParameterIndex = 0;
        if (mVzEdgeForWeight.empty()) {
          setVzForWeight(nWeightVzBin_Run14_P16id,
                         WeightVzEdgeMin_Run14_P16id,
                         WeightVzEdgeMax_Run14_P16id);
        }
        if (mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
          readScaleForWeight(nWeightgRefmultBin_Run14_P16id,
                             weight_VpdMB5ToVpdMB30_Run14_P16id);
        }
      } // if(RunId/1000000==15 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
    } // if ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 )
    else if ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0) {
        // prod.1
        if (RunId / 1000 >= 17039 && RunId / 1000 <= 17130) {
          mParameterIndex = 4;
          if (mVzEdgeForWeight.empty()) {
            setVzForWeight(nWeightVzBin_Run16_P16ij_prod1,
                           WeightVzEdgeMin_Run16_P16ij_prod1,
                           WeightVzEdgeMax_Run16_P16ij_prod1);
          }
          if (mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
            readScaleForWeight(nWeightgRefmultBin_Run16_P16ij_prod1,
                               weight_VpdMB5ToVpdMBnoVtx_Run16_P16ij_prod1);
          }
        } // if (RunId / 1000 >= 17039 && RunId / 1000 <= 17130)
        // prod.2
        else if (RunId / 1000 >= 17169 && RunId / 1000 <= 17179) {
          mParameterIndex = 5;
          if (mVzEdgeForWeight.empty()) {
            setVzForWeight(nWeightVzBin_Run16_P16ij_prod2,
                           WeightVzEdgeMin_Run16_P16ij_prod2,
                           WeightVzEdgeMax_Run16_P16ij_prod2);
          }
          if (mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
            readScaleForWeight(nWeightgRefmultBin_Run16_P16ij_prod2,
                               weight_VpdMB5ToVpdMBnoVtx_Run16_P16ij_prod2);
          }
        } // else if (RunId / 1000 >= 17169 && RunId / 1000 <= 17179)
      } // if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0)
    } // else if ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 )
    else if ( mSubName.CompareTo("Run14_AuAu200_VpdMB30", TString::kIgnoreCase) == 0 ) {
      if (mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0) {
        mParameterIndex = 1;
      }
    }
    else if ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_LowMid", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0) {
        mParameterIndex = 2;
      }
    }
    else if ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_High", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P15ic", TString::kIgnoreCase) == 0) {
        mParameterIndex = 3;
      }
    }
    else if ( mSubName.CompareTo("Run16_AuAu200_VpdMBnoVtx", TString::kIgnoreCase) == 0 ) {
      if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0) {
        mParameterIndex = 6;
      }
    }
    else{
      mParameterIndex = -1;
    }
  }	

  // Run numbers for isobar data are not successive
  if ( mName.CompareTo("refmult", TString::kIgnoreCase) == 0 &&
       mSubName.CompareTo("Isobar", TString::kIgnoreCase) == 0 ) {

    //	std::cout <<"This is isobaric runs"<< std::endl;
    mIsZr = mIsRu = kFALSE;
    for (Int_t i = 0; i < nRunId_Zr; i++) {
      if (RunId == IsobarRunId_Zr[i]) {
        mIsZr = kTRUE;
        mParameterIndex = 36;
        //	std::cout <<"--> Zr+Zr"<< std::endl;
        break;
      }
    }
    for (Int_t i = 0; i < nRunId_Ru; i++) {
      if (RunId == IsobarRunId_Ru[i]) {
        mIsRu = kTRUE;
        mParameterIndex = 37;
        //	std::cout <<"--> Ru+Ru"<< std::endl;
        break;
      }
    }
    if(!mIsZr && !mIsRu) {
      Error("StRefMultCorr::setParameterIndex", "RUN %d is not isobaric data", RunId);
    }
  }

  //	std::cout <<"mParameterIndex = "<< mParameterIndex << std::endl;

  if(mParameterIndex == -1) {
    Error("StRefMultCorr::setParameterIndex", "Parameter set does not exist for RUN %d", RunId);
  }
  //else std::cout << "Parameter set = " << npar_set << std::endl;

  return mParameterIndex ;
}

//_________________
Double_t StRefMultCorr::getRefMultCorr() const {
  // Call initEvent() first
  return mRefMult_corr ;
}

//________________
Double_t StRefMultCorr::luminosityCorrection(Double_t zdcCoincidenceRate) const {

  Double_t lumiCorr = 1.;
  
  // 200 GeV only. correction = 1 for all the other energies for BES-I
  // the above statement may not true for BES-II, since the luminosity is much higher than BES-I, add by Zaochen
  // better to check the <Refmult> vs ZDCX to see whether they are flat or not, add by Zaochen
  const Double_t par0_lum = mPar_luminosity[0][mParameterIndex] ;
  const Double_t par1_lum = mPar_luminosity[1][mParameterIndex] ;
  
  if( mParameterIndex>=36 && mParameterIndex<=37 ) {
    // if(mYear[mParameterIndex] == 2018 && mIsZr) zdcmean = 96.9914;
    // if(mYear[mParameterIndex] == 2018 && mIsRu) zdcmean = 97.9927;
    Double_t b_prime = 1.;
    if(mParameterIndex==36) b_prime = 96.9914; // Zr
    if(mParameterIndex==37) b_prime = 97.9927; // Ru
    lumiCorr = (par0_lum<std::numeric_limits<double>::epsilon() ) ? 1.0 : b_prime/(par0_lum+zdcCoincidenceRate*par1_lum);
  }
  else {
    lumiCorr = (par0_lum < std::numeric_limits<double>::epsilon() ) ? 1.0 : 1.0/(1.0 + par1_lum/par0_lum*zdcCoincidenceRate/1000.);
  }

  // from Run14, P16id, for VpdMB5/VPDMB30/VPDMB-noVtx, use refMult at ZdcX=30, other is at ZdcX=0;  
  // -->changed by xlchen@lbl.gov, Run16 ~ 50kHz
  if( 
     ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run14_AuAu200_VpdMB30", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_LowMid", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_High", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P15ic", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0 )
     || ( mSubName.CompareTo("Run16_AuAu200_VpdMBnoVtx", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0 )
      ) {
    float zdcmean = 0;
    if(mYear[mParameterIndex] == 2014) zdcmean = 30.;
    if(mYear[mParameterIndex] == 2016) zdcmean = 50.;
    lumiCorr = (par0_lum==0.0) ? lumiCorr : lumiCorr*(par0_lum+par1_lum*zdcmean)/par0_lum; 
  }

  return lumiCorr;
}

//________________
Double_t StRefMultCorr::vzCorrection(Double_t z) const {

  Double_t vzCorr = 1.;
  if ( mParameterIndex < 38 ) { 
    // Old correction based on the 6th-order polynomial fit of the high-end point 
    // fit of refMult for the given Vz range

    // par0 to par5 define the parameters of a polynomial to parametrize z_vertex dependence of RefMult
    const Double_t par0 = mPar_z_vertex[0][mParameterIndex];
    const Double_t par1 = mPar_z_vertex[1][mParameterIndex];
    const Double_t par2 = mPar_z_vertex[2][mParameterIndex];
    const Double_t par3 = mPar_z_vertex[3][mParameterIndex];
    const Double_t par4 = mPar_z_vertex[4][mParameterIndex];
    const Double_t par5 = mPar_z_vertex[5][mParameterIndex];
    const Double_t par6 = mPar_z_vertex[6][mParameterIndex];
    // This parameter is usually 0, it takes care for an additional efficiency, 
    // usually difference between phase A and phase B parameter 0
    const Double_t par7 = mPar_z_vertex[7][mParameterIndex]; 

    const Double_t  RefMult_ref = par0; // Reference mean RefMult at z=0
    const Double_t  RefMult_z   = par0 + par1*z + par2*z*z + par3*z*z*z + par4*z*z*z*z + par5*z*z*z*z*z + par6*z*z*z*z*z*z; // Parametrization of mean RefMult vs. z_vertex position
    if(RefMult_z > 0.0) {
      vzCorr = (RefMult_ref + par7)/RefMult_z;
    }
  }
  else if ( mParameterIndex == 38 ) { 
    // New Vz correction. All vz bins bins are normalize to that at the center
    vzCorr = auau19_run19_vzCorr[ getVzWindowForVzDepCentDef() ];

  } // else if ( mParameterIndex == 38 )

  return vzCorr;
}

//________________
Double_t StRefMultCorr::sampleRefMult(Int_t refMult) const {

  Double_t refMult_d = -9999.;
  if( mParameterIndex>=30 && mParameterIndex<=38 ) {
    refMult_d = (Double_t)refMult - 0.5 + gRandom->Rndm();
  }
  else {
    refMult_d = (Double_t)refMult + gRandom->Rndm();
  }

  return refMult_d;
}

//________________
Double_t StRefMultCorr::getRefMultCorr(const UShort_t refMult,
                                       const Double_t z,
                                       const Double_t zdcCoincidenceRate,
                                       const UInt_t flag) const {
  
  // Apply correction if parameter index & z-vertex are ok
  if (!isIndexOk() || !isZvertexOk()) return refMult ;

  // Correction function for RefMult, takes into account z_vertex dependence

  Double_t lumiCorr = luminosityCorrection(zdcCoincidenceRate);
  Double_t vzCorr = vzCorrection(z);
  Double_t refMult_d = sampleRefMult( refMult );
  Double_t refMultCorr  = -9999. ;
  switch (flag) {
  case 0: refMultCorr = refMult_d * lumiCorr; break;
  case 1: refMultCorr =  refMult_d * vzCorr; break;
  case 2: refMultCorr = refMult_d * vzCorr * lumiCorr; break;
  default: {
    Error("StRefMultCorr::getRefMultCorr", "invalid flag, flag=%d, should be 0, 1 or 2", flag);
    refMultCorr = -9999.;
  }
  } // switch ( flag )

  return refMultCorr;
}

//_________________
void StRefMultCorr::readScaleForWeight(const Char_t* input) {
  std::ifstream fin(input) ;
  if(!fin) {
    Error("StRefMultCorr::readScaleForWeight", "can't open %s", input);
    return;
  }

  // Users must set the vz bin size by setVzForWeight() (see below)
  if(mnVzBinForWeight==0) {
    Error("StRefMultCorr::readScaleForWeight",
	  "Please call setVzForWeight() to set vz bin size");
    return;
  }

  // Do not allow multiple calls
  if(!mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
    Error("StRefMultCorr::readScaleForWeight",
	  "scale factor has already set in the array");
    return;
  }

  std::cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..." << std::flush;
	
  while(fin) {
    Double_t scale[mnVzBinForWeight] ;
    for(Int_t i=0; i<mnVzBinForWeight; i++) {
      fin >> scale[i] ;
    }
		
    if(fin.eof()) break ;

    for(Int_t i=0; i<mnVzBinForWeight; i++) {
      mgRefMultTriggerCorrDiffVzScaleRatio.push_back(scale[i]);
    }
  }
  std::cout << " [OK]" << std::endl;
}

// NEW version to read Vz dependent weights from header
// Implemented inside StRefMultCorr::setParameterIndex(RunId)
//_________________
void StRefMultCorr::readScaleForWeight(const Int_t nRefmultbin, const Double_t *weight) {

  // Users must set the vz bin size by setVzForWeight() (see below)
  if( mnVzBinForWeight==0 ) {
    Error("StRefMultCorr::readScaleForWeight",
	  "Please call setVzForWeight() to set vz bin size");
    return;
  }

  // Do not allow multiple calls
  if(!mgRefMultTriggerCorrDiffVzScaleRatio.empty()) {
    Error("StRefMultCorr::readScaleForWeight",
	  "scale factor has already set in the array");
    return;
  }

  std::cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..." << std::flush;
	

  for(Int_t i=0; i<nRefmultbin*mnVzBinForWeight; i++) {
    mgRefMultTriggerCorrDiffVzScaleRatio.push_back(weight[i]);
  }


  std::cout << " [OK]" << std::endl;
}


// In NEW version, setVzForWeight() is implemented inside StRefMultCorr::setParameterIndex(RunId)
// It does not need to be called by users.
//_________________
void StRefMultCorr::setVzForWeight(const Int_t nbin, const Double_t min, const Double_t max) {
  // Do not allow multiple calls
  if ( !mVzEdgeForWeight.empty() ) {
    Error("StRefMultCorr::setVzForWeight",
	  "z-vertex range for weight has already been defined");
    return;
  }

  mnVzBinForWeight = nbin ;
  // calculate increment size
  const Double_t step = (max-min)/(Double_t)nbin;
	
  for(Int_t i=0; i<mnVzBinForWeight+1; i++) {
    mVzEdgeForWeight.push_back( min + step*i );
  }
	
  // Debug
  for(Int_t i=0; i<mnVzBinForWeight; i++) {
    std::cout << i << " " << step << " " << mVzEdgeForWeight[i]
	 << ", " << mVzEdgeForWeight[i+1] << std::endl;
  }
}

//_________________
Double_t StRefMultCorr::getScaleForWeight() const {
  // Special scale factor for global refmult in Run14 (Run16)
  // to account for the relative difference of VPDMB5 w.r.t VPDMB30 (VPDMBnoVtx) 

  // return 1 if mgRefMultTriggerCorrDiffVzScaleRatio array is empty
  if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()) return 1.0 ;

  //  const Int_t nVzBins =6;
  //  Double_t VzEdge[nVzBins+1]={-6., -4., -2., 0., 2., 4., 6.};

  Double_t VPD5weight=1.0;
  for(Int_t j=0;j<mnVzBinForWeight;j++) {
    if(mVz>mVzEdgeForWeight[j] && mVz<=mVzEdgeForWeight[j+1]) {
      /*
      //refMultbin=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->FindBin(mRefMult_corr+1e-6);
      //VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->GetBinContent(refMultbin);
      const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
      //VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[j][refMultbin];
      VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
      const Double_t tmpContent=VPD5weight;
      if(tmpContent==0 || (mRefMult_corr>500 && tmpContent<=0.65)) VPD5weight=1.15;//Just because the value of the weight is around 1.15
      if(mRefMult_corr>500 && tmpContent>=1.35) VPD5weight=1.15;//Remove those Too large weight factor,gRefmult>500
      // this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
      */

      const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
      VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
      const Double_t tmpContent=VPD5weight;
      // 1) Ratios fluctuate too much at very high gRefmult due to low statistics
      // 2) Avoid some events with too high weight
      if(mRefMult_corr>550 && (tmpContent>3.0||tmpContent<0.3)) VPD5weight=1.0;
      // this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
    }
  }

  return 1.0/VPD5weight;
}

//_________________
// For Run18 27 GeV
Double_t StRefMultCorr::getShapeWeight_SubVz2Center() const {

  Double_t weight = 1.;
  Int_t iVzBinIndex = getVzWindowForVzDepCentDef();

  if ( mParameterIndex>=30 && mParameterIndex<=35 ) { // Run18 27 GeV MB

    if(mRefMult_corr>=500.) return 1.0; // almost no Refmult>500 for this collision energy
		
    Int_t iRunPdIndex = mParameterIndex-30;
    Int_t iRefmultBin = (Int_t)(mRefMult_corr/2.); //find the refmult bin matching to the Parameter bin, if binWidth=2
    //Int_t iRefmultBin = (Int_t)(mRefMult_corr);  //find the refmult bin matching to the Parameter bin, if binWidth=1
		
    if(iRunPdIndex<0 || iRunPdIndex>5)  return 1.0;
    if(iVzBinIndex<0 || iVzBinIndex>13) return 1.0;
		
    //----------------------------------------------------------
    //load the ShapeReweight factors in given RunPd and VzBin
    //----------------------------------------------------------
    std::vector<std::string> sParam_ShapeWeight = StringSplit(getParamX_ShapeWeight(iRunPdIndex,iVzBinIndex),',');
    if( iRefmultBin>=(Int_t)sParam_ShapeWeight.size() ) {
      std::cout<<"ERROR: sParam_ShapeWeight is out of ranges!!!!!"<<std::endl;
      return 1.0;
    }
	
    //std::cout<<"sParam_ShapeWeight.size(): "<<sParam_ShapeWeight.size()<<std::endl;
    //for(UInt_t is=0; is<sParam_ShapeWeight.size(); is++) std::cout<<"sParam_ShapeWeight[is]: "<<sParam_ShapeWeight[is]<<std::endl;

    Double_t ShapeReweight = 1.0;

    Double_t tem_ShapeReweight = std::stod( sParam_ShapeWeight[iRefmultBin] );
    //prevent the crazy numbers for the large fluctuations
    if( tem_ShapeReweight<0.1 ) {
      ShapeReweight = 0.1;
    }
    else if(tem_ShapeReweight>10) {
      ShapeReweight = 10.;
    }
    else if(tem_ShapeReweight>=0.1 && tem_ShapeReweight<=10.) {
      ShapeReweight = tem_ShapeReweight;
    }
		
    weight = 1./ShapeReweight;

  } // if ( mParameterIndex>=30 && mParameterIndex<=35 ) { // Run18 27 GeV MB
  else if ( mParameterIndex>=36 && mParameterIndex<=37 ) { // Isobar collision 200 GeV 2018

    if (mVz >= -9 && mVz <= 9) {
      return 1.;
    }

    Int_t mShapeIndex = 0;
    if (mIsZr) {
      mShapeIndex = 1;
    }

    //retrive shape weight
    if (iVzBinIndex >= 22) {
      weight = ShapeWeightArray[mShapeIndex][iVzBinIndex - 9][TMath::Nint(mRefMult_corr)];
    }
    else {
      weight = ShapeWeightArray[mShapeIndex][iVzBinIndex][TMath::Nint(mRefMult_corr)];
    }
    //handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  }                                 // else if ( mParameterIndex>=36 && mParameterIndex<=37 ) { // Isobar collision 200 GeV 2018
  else if (mParameterIndex == 38) { // Au+Au 19.6 GeV 2019

    if (iVzBinIndex < 0 || iVzBinIndex > auau19_run19_nVzBins) return 1.0;

    weight = auau19_run19_shapeWeightArray[iVzBinIndex][TMath::Nint(mRefMult_corr)];
    // Handle bad weight
    if (weight == 0 || TMath::IsNaN(weight)) {
      weight = 1.;
    }
  } // else if ( mParameterIndex == 38 ) {  // Au+Au 19.6 GeV 2019
  else {
    weight = 1.0;
  }
  return weight;
}

//________________
Double_t StRefMultCorr::triggerWeight() const {
  
  Double_t weight = 1.;

  const Double_t par0 =   mPar_weight[0][mParameterIndex];
  const Double_t par1 =   mPar_weight[1][mParameterIndex];
  const Double_t par2 =   mPar_weight[2][mParameterIndex];
  const Double_t par3 =   mPar_weight[3][mParameterIndex];
  const Double_t par4 =   mPar_weight[4][mParameterIndex];
  const Double_t A    =   mPar_weight[5][mParameterIndex]; // Set to 0 if no z-dependent trigger weight is set
  const Double_t par6 =   mPar_weight[6][mParameterIndex];
  const Double_t par7 =   mPar_weight[7][mParameterIndex];

  // Additional z-vetex dependent correction
  //const Double_t A = ((1.27/1.21))/(30.0*30.0); // Don't ask...
  //const Double_t A = (0.05/0.21)/(30.0*30.0); // Don't ask...


  if ( isRefMultOk() // 0-80%
       && mRefMult_corr < mNormalize_stop[mParameterIndex] // reweighting only apply up to normalization point
       && mRefMult_corr != -(par3/par2)  ) { // avoid denominator = 0 

    // Parametrization of MC/data RefMult ratio
    weight = (par0 +
              par1 / (par2 * mRefMult_corr + par3) +
              par4 * (par2 * mRefMult_corr + par3) +
              par7 / TMath::Power(par2 * mRefMult_corr + par3, 2) +
              par6 * TMath::Power(par2 * mRefMult_corr + par3, 2));
    /*
    std::cout << "par0: " << par0 << " par1: " << par1 << " par2: " << par2
              << " par3: " << par3 << " par4: " << par4 << " A: " << A
              << " par6: " << par6 << " par7: " << par7 << "\n"
              << "refMultCorr: " << mRefMult_corr << " weight: " << weight << std::endl;
              */

    weight = weight + (weight-1.0)*(A*mVz*mVz); // z-dependent weight correction
  }

  return weight;
}

//_________________
Double_t StRefMultCorr::getWeight() const {

  Double_t Weight = 1.0;

  // Invalid index
  if( !isIndexOk() )   return Weight ;
  // Invalid z-vertex
  if( !isZvertexOk() ) return Weight ;

  // Trigger efficiency
  Weight = triggerWeight();

  //------------for Run14 and Run16----------------
  // Special scale factor for global refmult depending on Vz window
  // for others, scale factor = 1
  Weight *= getScaleForWeight();

  // Shape correction
  Weight *= getShapeWeight_SubVz2Center();

  return Weight;
}

//_________________
Int_t StRefMultCorr::getCentralityBin16() const {
  Int_t CentBin16 = -1;

  // Invalid index
  if( !isIndexOk() ) return CentBin16 ;
  
  while( CentBin16 < mNCentrality && !isCentralityOk(CentBin16) ) {
    CentBin16++;
  }

  // Vz dependent centrality definition for Vz<-27 and Vz>25
  // Run17 54.4 GeV, trigid = 580001
  if( mParameterIndex==28&&(mVz<-27||mVz>25) ) {
    CentBin16 = getCentralityBin16VzDep();
    // if(CentBin16==-1) std::cout <<"Vz="<< mVz <<" RefMult="<< mRefMult <<" RefMultCorr="<< mRefMult_corr << std::endl;
    // if(CentBin16==9999) std::cout << "Invalide number"<< std::endl; 
    // std::cout <<"Vz dependent centrality definition for Vz<-27 and Vz>25 ... Vz="<< mVz <<" RefMult="<< mRefMult <<" RefMultCorr="<< mRefMult_corr <<" iCent="<< CentBin16 << std::endl;
  }

  // return -1 if CentBin16 = 16 (very large refmult, refmult>5000)
  return ( CentBin16==16 ) ? -1 : CentBin16;
}

//_________________
Int_t StRefMultCorr::getCentralityBin9() const {
  Int_t CentBin9 = -1;

  // Invalid index
  if ( !isIndexOk() ) return CentBin9 ;

  const Int_t  CentBin16 = getCentralityBin16(); // Centrality bin 16
  const Bool_t isCentralityOk = CentBin16 >= 0 && CentBin16 < mNCentrality ;

  // No centrality is defined
  if ( !isCentralityOk ) return CentBin9;

  // First handle the exceptions
  if( mRefMult_corr > mCentrality_bins[15][mParameterIndex] && 
      mRefMult_corr <= mCentrality_bins[16][mParameterIndex] ) {
    CentBin9 = 8; // most central 5%
  }
  else if( mRefMult_corr > mCentrality_bins[14][mParameterIndex] && 
	   mRefMult_corr <= mCentrality_bins[15][mParameterIndex]) {
    CentBin9 = 7; // most central 5-10%
  }
  else {
    CentBin9 = (Int_t)(0.5*CentBin16);
  }

  // Vz dependent centrality definition for Vz<-27 and Vz>25
  // Run17 54.4 GeV, trigid = 580001
  // std::cout << mParameterIndex << std::endl;
  if ( mParameterIndex == 28 && ( mVz<-27 || mVz>25 ) ) {
    CentBin9 = getCentralityBin9VzDep();
  }

  return CentBin9;
}

//_________________
Int_t StRefMultCorr::getVzWindowForVzDepCentDef() const {
  Int_t iBinVz = -1;

  if( mParameterIndex==28 ) {	//  54.4 GeV, RefMult, 580001
    if( mVz>-30 && mVz<-29 )      iBinVz = 0;
    else if( mVz>-29 && mVz<-27 ) iBinVz = 1;
    else if( mVz>25 && mVz<27 )   iBinVz = 2;
    else if( mVz>27 && mVz<29 )   iBinVz = 3;
    else if( mVz>29 && mVz<30 )   iBinVz = 4;
    else iBinVz = -1;
  }
  else if( mParameterIndex>=30 && mParameterIndex<=35 ) { //Run18 27 GeV MB, 6 triggerIds
    if( mVz>=-70. && mVz<-60. ) iBinVz = 0;
    else if( mVz>=-60.&&mVz<-50.) iBinVz = 1;
    else if( mVz>=-50.&&mVz<-40.) iBinVz = 2;
    else if( mVz>=-40.&&mVz<-30.) iBinVz = 3;
    else if( mVz>=-30.&&mVz<-20.) iBinVz = 4;
    else if( mVz>=-20.&&mVz<-10.) iBinVz = 5;
    else if( mVz>=-10.&&mVz<0.0 ) iBinVz = 6;
    else if( mVz>=0.0 &&mVz<10. ) iBinVz = 7;
    else if( mVz>=10. &&mVz<20. ) iBinVz = 8;
    else if( mVz>=20. &&mVz<30. ) iBinVz = 9;
    else if( mVz>=30. &&mVz<40. ) iBinVz = 10;
    else if( mVz>=40. &&mVz<50. ) iBinVz = 11;
    else if( mVz>=50. &&mVz<60. ) iBinVz = 12;
    else if( mVz>=60. &&mVz<=70 ) iBinVz = 13;
    else iBinVz = -1;
  }
  else if( mParameterIndex>=36 && mParameterIndex<=37 ) { //Run18 200 GeV isobar 
    Double_t VtxZBinDouble = mVz/2. + 17.;
    iBinVz = 0;
    if (mVz == 25.) {
      iBinVz = 29;
    }
    else if (mVz == -35.) {
      iBinVz = 0;
    }
    else {
      iBinVz = TMath::Nint(VtxZBinDouble);
    }
  }
  /*
    else if(mParameterIndex>=36 &&mParameterIndex<=37)//Run18 200 GeV isobar 
    {
    if(     mVz>=-35.&&mVz<-33.) iBinVz = 0;
    else if(mVz>=-33.&&mVz<-31.) iBinVz = 1;
    else if(mVz>=-31.&&mVz<-29.) iBinVz = 2;
    else if(mVz>=-29.&&mVz<-27.) iBinVz = 3;
    else if(mVz>=-27.&&mVz<-25.) iBinVz = 4;
    else if(mVz>=-25.&&mVz<-23.) iBinVz = 5;
    else if(mVz>=-23.&&mVz<-21.) iBinVz = 6;
    else if(mVz>=-21.&&mVz<-19.) iBinVz = 7;
    else if(mVz>=-19.&&mVz<-17.) iBinVz = 8;
    else if(mVz>=-17.&&mVz<-15.) iBinVz = 9;
    else if(mVz>=-15.&&mVz<-13.) iBinVz = 10;
    else if(mVz>=-13.&&mVz<-11.) iBinVz = 11;
    else if(mVz>=-11.&&mVz<-9.) iBinVz = 12;
    else if(mVz>=-9.&&mVz<-7.) iBinVz = 13;
    else if(mVz>=-7.&&mVz<-5.) iBinVz = 14;
    else if(mVz>=-5.&&mVz<-3.) iBinVz = 15;
    else if(mVz>=-3.&&mVz<-1.) iBinVz = 16;
    else if(mVz>=-1.&&mVz<1.) iBinVz = 17;
    else if(mVz>=1.&&mVz<3.) iBinVz = 18;
    else if(mVz>=3.&&mVz<5.) iBinVz = 19;
    else if(mVz>=5.&&mVz<7.) iBinVz = 20;
    else if(mVz>=7.&&mVz<9.) iBinVz = 21;
    else if(mVz>=9.&&mVz<11.) iBinVz = 22;
    else if(mVz>=11.&&mVz<13.) iBinVz = 23;
    else if(mVz>=13.&&mVz<15.) iBinVz = 24;
    else if(mVz>=15.&&mVz<17.) iBinVz = 25;
    else if(mVz>=17.&&mVz<19.) iBinVz = 26;
    else if(mVz>=19.&&mVz<21.) iBinVz = 27;
    else if(mVz>=21.&&mVz<23.) iBinVz = 28;
    else if(mVz>=23.&&mVz<=25.) iBinVz = 29;
    }
  */
  else if ( mParameterIndex == 38 ) {  // Au+Au 19.6 GeV 2019

    for ( Int_t iVz=0; iVz<auau19_run19_nVzBins; iVz++ ) {
      if ( auau19_run19_vzRangeLimits[iVz][0] <= mVz && mVz < auau19_run19_vzRangeLimits[iVz][1] ) {
        iBinVz = iVz;
        break;
      }
    } // for ( Int_t iVz=0; iVz<auau19_run19_nVzBins; iVz++ )
  } // else if ( mParameterIndex == 38 )
  else {
    iBinVz = -1;
  }
	
  return iBinVz;
}

//_________________
Int_t StRefMultCorr::getCentralityBin9VzDep() const {
  const Int_t vzid = getVzWindowForVzDepCentDef();
  Int_t iCent = 9999;
  for(Int_t i=0; i<9; i++) {
    if ( i==8 ) {
      if( mRefMult_corr>CentBin9_vzdep[vzid][i] && mRefMult_corr<50000 ) iCent = i; 
    }
    else if( mRefMult_corr>CentBin9_vzdep[vzid][i] && 
	     mRefMult_corr<CentBin9_vzdep[vzid][i+1] ) iCent = i; 
  }
  // 80-100% for icent=-1
  if( mRefMult_corr>0 && mRefMult_corr<CentBin9_vzdep[vzid][0] ) iCent = -1; 
  return ( iCent==9999 ) ? -1 : iCent;
}

//_________________
Int_t StRefMultCorr::getCentralityBin16VzDep() const {
  const Int_t vzid = getVzWindowForVzDepCentDef();
  Int_t iCent = 9999;
  for(Int_t i=0; i<16; i++) {
    if (i == 15) {
      if (mRefMult_corr > CentBin16_vzdep[vzid][i] && mRefMult_corr < 50000) {
        iCent = i;
      }
    }
    else if (mRefMult_corr > CentBin16_vzdep[vzid][i] &&
             mRefMult_corr < CentBin16_vzdep[vzid][i + 1]) {
      iCent = i;
    }
  }
  // 80-100% for icent=-1
  if( mRefMult_corr>0 && mRefMult_corr<CentBin16_vzdep[vzid][0] ) {
    iCent = -1; 
  }
  return (iCent==9999) ? -1 : iCent;
}

//_________________
const Int_t StRefMultCorr::getRefX() const {
  if (      mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) return 0; 
  else if ( mName.CompareTo("refmult",  TString::kIgnoreCase) == 0 ) return 1; 
  else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) return 2; 
  else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) return 3; 
  else if ( mName.CompareTo("refmult4", TString::kIgnoreCase) == 0 ) return 4; 
  else return 9999;
}

//_________________
const Int_t StRefMultCorr::getNumberOfDatasets() const {
  if (      mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) return nID_gref; 
  else if ( mName.CompareTo("refmult",  TString::kIgnoreCase) == 0 ) return nID_ref1; 
  else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) return nID_ref2; 
  else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) return nID_ref3; 
  else if ( mName.CompareTo("refmult4", TString::kIgnoreCase) == 0 ) return nID_ref4; 
  else return 9999;
}

//_________________
void StRefMultCorr::readHeaderFile() {

  //std::vector<std::string> sParam_ShapeWeight = StringSplit(getParamX_ShapeWeight(1,1),',');
  //for(int ib=0;ib<sParam_ShapeWeight.size(); ib++) std::cout<<"sParam_ShapeWeight[i]: "<<sParam_ShapeWeight[ib]<<std::endl;
  
  const Int_t refX = getRefX();
  const Int_t nID =  getNumberOfDatasets();
  
  for(int iID=0; iID<nID; iID++) {

    //
    // Year, energy, run numbers, Vz cut
    //
    Int_t year; Double_t energy;
    std::vector<std::string> sParam = StringSplit(getParamX(refX,iID,0),':'); 
    year = stoi(sParam[0]); 
    energy = stoi(sParam[1]); 
    std::vector<std::string> sRuns = StringSplit(sParam[2],',');
		
    Int_t startRunId=0, stopRunId=0;
    startRunId = stoi(sRuns[0]); 
    stopRunId = stoi(sRuns[1]); 
		
    Double_t startZvertex=-9999., stopZvertex=-9999. ;
    std::vector<std::string> sVz = StringSplit(sParam[3],',');
    startZvertex = stod(sVz[0]);
    stopZvertex = stod(sVz[1]);

    mYear.push_back(year);
    mBeginRun.insert(std::make_pair(std::make_pair(energy, year), startRunId));
    mEndRun.insert(  std::make_pair(std::make_pair(energy, year), stopRunId));
    mStart_runId.push_back( startRunId ) ;
    mStop_runId.push_back(  stopRunId ) ;
    mStart_zvertex.push_back( startZvertex ) ;
    mStop_zvertex.push_back(  stopZvertex ) ;

    //
    // Centrality definition
    //
    std::vector<std::string> sParamCent = StringSplit(getParamX(refX,iID,1),','); 
    for(UInt_t i=0; i<sParamCent.size(); i++) {
      mCentrality_bins[i].push_back( stoi(sParamCent[i]) );
    }
    mCentrality_bins[mNCentrality].push_back( 5000 );

    //
    // Normalization range
    //
    Double_t normalize_stop=-1.0 ;
    normalize_stop = stod(getParamX(refX,iID,2)) ;
    mNormalize_stop.push_back( normalize_stop );

    //
    // Acceptance (vz) correction
    //
    std::vector<std::string> sParamVz = StringSplit(getParamX(refX,iID,3),','); 
    for(UInt_t i=0; i<mNPar_z_vertex; i++) {
      Double_t val = -9999.; 
      if(i<sParamVz.size()) val = stod(sParamVz[i]);
      else                  val = 0.0;
      mPar_z_vertex[i].push_back( val );
    }

    //
    // Trigger inefficiency correction
    //
    std::vector<std::string> sParamTrig = StringSplit(getParamX(refX,iID,4),','); 
    for(UInt_t i=0; i<mNPar_weight; i++) {
      Double_t val = -9999.;
      if(i<sParamTrig.size()) val = stod(sParamTrig[i]);
      else                    val = 0.0;
      mPar_weight[i].push_back( val );
    }

    //
    // Luminosity correction
    //
    std::vector<std::string> sParamLumi = StringSplit(getParamX(refX,iID,5),','); 
    for(UInt_t i=0; i<mNPar_luminosity; i++) {
      Double_t val = -9999.;
      if(i<sParamLumi.size()) val = stod(sParamLumi[i]);
      else                    val = 0.0;
      mPar_luminosity[i].push_back( val );
    }

    //	std::cout << refX <<"  "<< iID <<"/"<< nID << std::endl;
  }

  std::cout << "StRefMultCorr::readHeaderFile  [" << mName 
       << "] Correction parameters and centrality definitions have been successfully read."
       << std::endl;
}

//_________________
void StRefMultCorr::readBadRunsFromHeaderFile() {

  //
  // TODO: Modify to read only bad runs associated with the current at least year
  //

  for(Int_t i=0; i<nBadRun_refmult_2010; i++) {
    mBadRun.push_back(badrun_refmult_2010[i]);
  }
  std::cout << "read in nBadRun_refmult_2010: " << nBadRun_refmult_2010 << std::endl;

  for(Int_t i=0; i<nBadRun_refmult_2011; i++) {
    mBadRun.push_back(badrun_refmult_2011[i]);
  }
  std::cout << "read in nBadRun_refmult_2011: " << nBadRun_refmult_2011 << std::endl;

  for(Int_t i=0; i<nBadRun_grefmult_2014; i++) {
    mBadRun.push_back(badrun_grefmult_2014[i]);
  }
  std::cout << "read in nBadRun_grefmult_2014: " << nBadRun_grefmult_2014 << std::endl;

  for(Int_t i=0; i<nBadRun_grefmult_2016; i++) {
    mBadRun.push_back(badrun_grefmult_2016[i]);
  }
  std::cout << "read in nBadRun_grefmult_2016: " << nBadRun_grefmult_2016 << std::endl;

  for(Int_t i=0; i<nBadRun_refmult_2017; i++) {
    mBadRun.push_back(badrun_refmult_2017[i]);
  }
  std::cout << "read in nBadRun_refmult_2017: " << nBadRun_refmult_2017 << std::endl;

  for (Int_t i = 0; i < nBadRun_refmult_2018; i++) {
    mBadRun.push_back(badrun_refmult_2018[i]);
  }
  std::cout << "read in nBadRun_refmult_2018: " << nBadRun_refmult_2018 << std::endl;

  for (Int_t i = 0; i < nBadRun_refmult_2019; i++) {
    mBadRun.push_back(badrun_refmult_2019[i]);
  }
  std::cout << "read in nBadRun_refmult_2019: " << nBadRun_refmult_2019 << std::endl;

  //// notification only one
  if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) {
    std::cout << "StRefMultCorr::readBadRunsFromHeaderFile  Bad runs for year 2010, 2011, 2017, 2018 and 2019 have been read." << std::endl;
  }
}

//_________________
void StRefMultCorr::print(const Option_t* option) const {
  std::cout << "StRefMultCorr::print  Print input parameters for " 
       << mName << " ========================================" << std::endl << std::endl;
  // Option switched off, can be used to specify parameters
  //  const TString opt(option);
  
  //  Int_t input_counter = 0;
  for(UInt_t id=0; id<mStart_runId.size(); id++) {
    //std::cout << "Data line = " << input_counter << ", Start_runId = " << Start_runId[input_counter] << ", Stop_runId = " << Stop_runId[input_counter] << std::endl;
    //    const UInt_t id = mStart_runId.size()-1;
    
    // Removed line break
    std::cout << "  Index=" << id;
    std::cout << Form(" Run=[%8d, %8d]", mStart_runId[id], mStop_runId[id]);
    std::cout << Form(" z-vertex=[%1.1f, %1.1f]", mStart_zvertex[id], mStop_zvertex[id]);
    std::cout << ", Normalize_stop=" << mNormalize_stop[id];
    std::cout << std::endl;
    
    //    if(opt.IsWhitespace()){
    //      continue ;
    //    }
    
    std::cout << "Centrality:  ";
		
    for(Int_t i=0;i<mNCentrality;i++) {
      std::cout << Form("  >%2d%%", 80-5*i);
    }
    std::cout << std::endl;
    std::cout << "RefMult:     ";
		
    for(Int_t i=0;i<mNCentrality;i++) {
      //      std::cout << Form("StRefMultCorr::read  Centrality %3d-%3d %%, refmult > %4d", 75-5*i, 80-5*i, mCentrality_bins[i][id]) << std::endl;
      const TString tmp(">");
      const TString centrality = tmp + Form("%d", mCentrality_bins[i][id]);
      std::cout << Form("%6s", centrality.Data());
    }
    std::cout << std::endl;

    for(Int_t i=0;i<mNPar_z_vertex;i++) {
      std::cout << "  mPar_z_vertex[" << i << "] = " << mPar_z_vertex[i][id];
    }
    std::cout << std::endl;
		
    for(Int_t i=0;i<mNPar_weight;i++) {
      std::cout << "  mPar_weight[" << i << "] = " << mPar_weight[i][id];
    }
    std::cout << std::endl;
    
    for(Int_t i=0;i<mNPar_luminosity;i++) {
      std::cout << "  mPar_luminosity[" << i << "] = " << mPar_luminosity[i][id];
    }
    std::cout << std::endl << std::endl;
  }
  std::cout << "=====================================================================================" << std::endl;
}

//_________________
std::vector<std::string> StRefMultCorr::StringSplit( const std::string str, const char sep ) const {
  std::vector<std::string> vstr;
  std::stringstream ss(str);
  std::string buffer;
  while( getline(ss,buffer,sep) ) {
    vstr.push_back(buffer);
  }
  return vstr;
}

