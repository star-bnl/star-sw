//------------------------------------------------------------------------------
// $Id: StRefMultCorr.cxx,v 1.14 2015/05/22 06:52:05 hmasui Exp $
// $Log: StRefMultCorr.cxx,v $
// Revision 1.14  2015/05/22 06:52:05  hmasui
// Add grefmult for Run14 Au+Au 200 GeV
//
// Revision 1.13  2013/05/10 18:33:33  hmasui
// Add TOF tray mult, preliminary update for Run12 U+U
//
// Revision 1.12  2012/05/19 00:48:20  hmasui
// Update refmult3
//
// Revision 1.11  2012/05/14 00:40:25  hmasui
// Exit code if no valid run number found. Fix the initialization of mParameterIndex
//
// Revision 1.10  2012/05/09 22:26:46  hmasui
// Commented out option use in the print() function
//
// Revision 1.9  2012/05/08 03:19:49  hmasui
// Move parameters to Centrality_def_refmult.txt
//
// Revision 1.8  2012/04/23 21:29:37  hmasui
// Added isBadRun() function for outlier rejection, getBeginRun() and getEndRun() to obtain the run range for a given (energy,year)
//
// Revision 1.7  2011/11/30 00:25:07  hmasui
// Additional check for ifstream to avoid reading one extra line from input file
//
// Revision 1.6  2011/11/08 19:11:05  hmasui
// Add luminosity corrections for 200 GeV
//
// Revision 1.5  2011/10/11 19:35:20  hmasui
// Fix typo. Add z-vertex check in getWeight() function
//
// Revision 1.4  2011/10/10 21:30:37  hmasui
// Replaced hard coded parameters for z-vertex and weight corrections by input parameters from text file
//
//
// Revision 1.3  2011/08/12 20:28:07  hmasui
// Avoid varying corrected refmult in the same event by random number
//
// Revision 1.2  2011/08/11 23:51:10  hmasui
// Suppress cout in the setParameterIndex function. Use TError for error messages.
//
// Revision 1.1  2011/08/11 18:38:28  hmasui
// First version of Refmult correction class
//
//------------------------------------------------------------------------------

#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include "StRefMultCorr.h"
#include "TError.h"
#include "TRandom.h"
#include "TMath.h"

ClassImp(StRefMultCorr)

using namespace std ;

  namespace {
    typedef pair<Double_t, Int_t> keys;
  }

//______________________________________________________________________________
// Default constructor
StRefMultCorr::StRefMultCorr(const TString name)
 : mName(name)
{
  mRefMult = 0 ;
  mVz = -9999. ;
  mRefMult_corr = -1.0 ;

  // Clear all data members
  clear() ;

  // Read parameters
  read() ;
  readBadRuns() ;
}

//______________________________________________________________________________
// Default destructor
StRefMultCorr::~StRefMultCorr()
{
}

//______________________________________________________________________________
Int_t StRefMultCorr::getBeginRun(const Double_t energy, const Int_t year)
{
  keys key(std::make_pair(energy, year));

  // Make sure key exists
  multimap<keys, Int_t>::iterator iterCheck = mBeginRun.find(key);
  if ( iterCheck == mBeginRun.end() ) {
    Error("StRefMultCorr::getBeginRun", "can't find energy = %1.1f, year = %d", energy, year);
    return -1;
  }

  pair<multimap<keys, Int_t>::iterator, multimap<keys, Int_t>::iterator> iterRange = mBeginRun.equal_range(key);

  return (*(iterRange.first)).second ;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getEndRun(const Double_t energy, const Int_t year)
{
  keys key(std::make_pair(energy, year));

  // Make sure key exists
  multimap<keys, Int_t>::iterator iterCheck = mEndRun.find(key);
  if ( iterCheck == mEndRun.end() ) {
    Error("StRefMultCorr::getEndRun", "can't find energy = %1.1f, year = %d", energy, year);
    return -1;
  }

  pair<multimap<keys, Int_t>::iterator, multimap<keys, Int_t>::iterator> iterRange = mEndRun.equal_range(key);
  multimap<keys, Int_t>::iterator iter = iterRange.second ;
  iter--;

  return (*iter).second ;
}

//______________________________________________________________________________
void StRefMultCorr::clear()
{
  // Clear all arrays, and set parameter index = -1

  mYear.clear() ;
  mStart_runId.clear() ;
  mStop_runId.clear() ;
  mStart_zvertex.clear() ;
  mStop_zvertex.clear() ;
  mNormalize_stop.clear() ;

  for(Int_t i=0;i<mNCentrality;i++) {
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

//______________________________________________________________________________
Bool_t StRefMultCorr::isBadRun(const Int_t RunId)
{
  // Return true if a given run id is bad run
  vector<Int_t>::iterator iter = std::find(mBadRun.begin(), mBadRun.end(), RunId);
#if 0
  if ( iter != mBadRun.end() ) {
    // QA
    cout << "StRefMultCorr::isBadRun  Find bad run = " << (*iter) << endl;
  }
#endif

  return ( iter != mBadRun.end() ) ;
}

//______________________________________________________________________________
void StRefMultCorr::initEvent(const UShort_t RefMult, const Double_t z, const Double_t zdcCoincidenceRate)
{
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

//______________________________________________________________________________
Bool_t StRefMultCorr::isIndexOk() const
{
  // mParameterIndex not initialized (-1)
  if ( mParameterIndex == -1 ) {
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. Call init(const Int_t RunId) function to initialize centrality bins, corrections");
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. or use valid run numbers defined in Centrality_def_%s.txt", mName.Data());
    Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. exit");
    cout << endl;
    // Stop the process if invalid run number found
    exit(0);
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

//______________________________________________________________________________
Bool_t StRefMultCorr::isZvertexOk() const
{
  // Primary z-vertex check
  return ( mVz > mStart_zvertex[mParameterIndex] && mVz < mStop_zvertex[mParameterIndex] ) ;
}

//______________________________________________________________________________
Bool_t StRefMultCorr::isRefMultOk() const
{
  // Invalid index
  if ( !isIndexOk() ) return kFALSE ;

  // select 0-80%
  return (mRefMult_corr > mCentrality_bins[0][mParameterIndex] && mRefMult_corr < mCentrality_bins[mNCentrality][mParameterIndex]);
}

//______________________________________________________________________________
Bool_t StRefMultCorr::isCentralityOk(const Int_t icent) const
{
  // Invalid centrality id
  if ( icent < -1 || icent >= mNCentrality+1 ) return kFALSE ;

  // Invalid index
  if ( !isIndexOk() ) return kFALSE ;

  // Special case
  // 1. 80-100% for icent=-1
  if ( icent == -1 ) return (mRefMult_corr <= mCentrality_bins[0][mParameterIndex]);

  // 2. icent = mNCentrality
  if ( icent == mNCentrality ) return (mRefMult_corr <= mCentrality_bins[mNCentrality][mParameterIndex]);

  const Bool_t ok = (mRefMult_corr > mCentrality_bins[icent][mParameterIndex] && mRefMult_corr <= mCentrality_bins[icent+1][mParameterIndex]);
//  if(ok){
//    cout << "StRefMultCorr::isCentralityOk  refmultcorr = " << mRefMult_corr
//      << "  min. bin = " << mCentrality_bins[icent][mParameterIndex]
//      << "  max. bin = " << mCentrality_bins[icent+1][mParameterIndex]
//      << endl;
//  }
  return ok ;
}

//______________________________________________________________________________
void StRefMultCorr::init(const Int_t RunId)
{
  // Reset mParameterIndex
  mParameterIndex = -1 ;

  // call setParameterIndex
  setParameterIndex(RunId) ;
}

//______________________________________________________________________________
Int_t StRefMultCorr::setParameterIndex(const Int_t RunId)
{
  // Determine the corresponding parameter set for the input RunId
  for(UInt_t npar = 0; npar < mStart_runId.size(); npar++)
  {
    if(RunId >= mStart_runId[npar] && RunId <= mStop_runId[npar])
    {
      mParameterIndex = npar ;
      //cout << "StRefMultCorr::setParameterIndex  Parameter set = " << mParameterIndex << " for RUN " << RunId << endl;
      break ;
    }
  }

  if(mParameterIndex == -1){
    Error("StRefMultCorr::setParameterIndex", "Parameter set does not exist for RUN %d", RunId);
  }
  //else cout << "Parameter set = " << npar_set << endl;
  
  return mParameterIndex ;
}

//______________________________________________________________________________
Double_t StRefMultCorr::getRefMultCorr() const
{
  // Call initEvent() first
  return mRefMult_corr ;
}

//______________________________________________________________________________
Double_t StRefMultCorr::getRefMultCorr(const UShort_t RefMult, const Double_t z,
    const Double_t zdcCoincidenceRate, const UInt_t flag) const
{
  // Apply correction if parameter index & z-vertex are ok
  if (!isIndexOk() || !isZvertexOk()) return RefMult ;

  // Correction function for RefMult, takes into account z_vertex dependence

  // Luminosity corrections
  // 200 GeV only. correction = 1 for all the other energies
  const Double_t par0l = mPar_luminosity[0][mParameterIndex] ;
  const Double_t par1l = mPar_luminosity[1][mParameterIndex] ;
  Double_t correction_luminosity = (par0l==0.0) ? 1.0 : 1.0/(1.0 + par1l/par0l*zdcCoincidenceRate/1000.);
  if(mName.CompareTo("grefmult_P16id", TString::kIgnoreCase) == 0 ||
     mName.CompareTo("grefmult_VpdMB30", TString::kIgnoreCase) == 0 ||
     mName.CompareTo("grefmult_VpdMBnoVtx", TString::kIgnoreCase) == 0 ) {
      float zdcmean = 0;
      if(mYear[mParameterIndex] == 2014) zdcmean = 30.;
      if(mYear[mParameterIndex] == 2016) zdcmean = 50.;
      correction_luminosity = (par0l==0.0) ? correction_luminosity : correction_luminosity*(par0l+par1l*zdcmean)/par0l; // from Run14, P16id, for VpdMB5/VPDMB30/VPDMB-noVtx, use refMult at ZdcX=30, other is at ZdcX=0;  -->changed by xlchen@lbl.gov, Run16 ~ 50kHz
  }

  // par0 to par5 define the parameters of a polynomial to parametrize z_vertex dependence of RefMult
  const Double_t par0 = mPar_z_vertex[0][mParameterIndex];
  const Double_t par1 = mPar_z_vertex[1][mParameterIndex];
  const Double_t par2 = mPar_z_vertex[2][mParameterIndex];
  const Double_t par3 = mPar_z_vertex[3][mParameterIndex];
  const Double_t par4 = mPar_z_vertex[4][mParameterIndex];
  const Double_t par5 = mPar_z_vertex[5][mParameterIndex];
  const Double_t par6 = mPar_z_vertex[6][mParameterIndex];
  const Double_t par7 = mPar_z_vertex[7][mParameterIndex]; // this parameter is usually 0, it takes care for an additional efficiency, usually difference between phase A and phase B parameter 0

  const Double_t  RefMult_ref = par0; // Reference mean RefMult at z=0
  const Double_t  RefMult_z = par0 + par1*z + par2*z*z + par3*z*z*z + par4*z*z*z*z + par5*z*z*z*z*z + par6*z*z*z*z*z*z; // Parametrization of mean RefMult vs. z_vertex position
  Double_t  Hovno = 1.0; // Correction factor for RefMult, takes into account z_vertex dependence
  if(RefMult_z > 0.0)
  {
    Hovno = (RefMult_ref + par7)/RefMult_z;
  }

  Double_t RefMult_d = (Double_t)(RefMult)+gRandom->Rndm(); // random sampling over bin width -> avoid peak structures in corrected distribution
  Double_t RefMult_corr  = -9999. ;
  switch ( flag ) {
    case 0: return RefMult_d*correction_luminosity;
    case 1: return RefMult_d*Hovno;
    case 2: return RefMult_d*Hovno*correction_luminosity;
    default:
      {
        Error("StRefMultCorr::getRefMultCorr", "invalid flag, flag=%d, should be 0,1 or 2", flag);
	return -9999.;
      }
  }
//  cout << "Input RefMult = " << RefMult << ", input z = " << z << ", RefMult_corr = " << RefMult_corr << endl;
  return RefMult_corr ;
}

//______________________________________________________________________________
void StRefMultCorr::readScaleForWeight(const Char_t* input)
{
  ifstream fin(input) ;
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

  cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..."
    << flush;
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
  cout << " [OK]" << endl;
}

//______________________________________________________________________________
void StRefMultCorr::setVzForWeight(const Int_t nbin, const Double_t min,
    const Double_t max)
{
  // Do not allow multiple calls
  if(!mVzEdgeForWeight.empty()) {
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
    cout << i << " " << step << " " << mVzEdgeForWeight[i]
      << ", " << mVzEdgeForWeight[i+1] << endl;
  }
}

//______________________________________________________________________________
Double_t StRefMultCorr::getScaleForWeight() const
{
  // Special scale factor for global refmult in Run14
  // to account for the difference between 
  // VPDMB-30 and VPDMB-5

  // return 1 if mgRefMultTriggerCorrDiffVzScaleRatio array is empty
  if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()) return 1.0 ;

//  const Int_t nVzBins =6;
//  Double_t VzEdge[nVzBins+1]={-6., -4., -2., 0., 2., 4., 6.};

  Double_t VPD5weight=1.0;
  for(Int_t j=0;j<mnVzBinForWeight;j++) {
    if(mVz>mVzEdgeForWeight[j] && mVz<=mVzEdgeForWeight[j+1]) {
      //			refMultbin=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->FindBin(mRefMult_corr+1e-6);
      //			VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->GetBinContent(refMultbin);
      const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
//      VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[j][refMultbin];
      VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
      const Double_t tmpContent=VPD5weight;
      if(mName.CompareTo("grefmult", TString::kIgnoreCase) == 0) {
        if(tmpContent==0 || (mRefMult_corr>500 && tmpContent<=0.65)) VPD5weight=1.15;//Just because the value of the weight is around 1.15
        if(mRefMult_corr>500 && tmpContent>=1.35) VPD5weight=1.15;//Remove those Too large weight factor,gRefmult>500
        // this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
      }
      if(mName.CompareTo("grefmult_P16id", TString::kIgnoreCase) == 0) {
        if(VPD5weight==0) VPD5weight=1;
      }
    }
  }


  return 1.0/VPD5weight;
}

//______________________________________________________________________________
Double_t StRefMultCorr::getWeight() const
{
  Double_t Weight = 1.0;

  // Invalid index
  if( !isIndexOk() ) return Weight ;

  // Invalid z-vertex
  if( !isZvertexOk() ) return Weight ;

  const Double_t par0 =   mPar_weight[0][mParameterIndex];
  const Double_t par1 =   mPar_weight[1][mParameterIndex];
  const Double_t par2 =   mPar_weight[2][mParameterIndex];
  const Double_t par3 =   mPar_weight[3][mParameterIndex];
  const Double_t par4 =   mPar_weight[4][mParameterIndex];
  const Double_t A    =   mPar_weight[5][mParameterIndex];
  const Double_t par6 =   mPar_weight[6][mParameterIndex];//Add by guannan for run14
  const Double_t par7 =   mPar_weight[7][mParameterIndex];//Add by guannan for run14

  // Additional z-vetex dependent correction
  //const Double_t A = ((1.27/1.21))/(30.0*30.0); // Don't ask...
  //const Double_t A = (0.05/0.21)/(30.0*30.0); // Don't ask...

  if(isRefMultOk() // 0-80%
      && mRefMult_corr < mNormalize_stop[mParameterIndex] // re-weighting only apply up to normalization point
      && mRefMult_corr != -(par3/par2) // avoid denominator = 0
    )
  {
    Weight = par0 + par1/(par2*mRefMult_corr + par3) + par4*(par2*mRefMult_corr + par3) + par6/TMath::Power(par2*mRefMult_corr+par3 ,2) + par7*TMath::Power(par2*mRefMult_corr+par3 ,2); // Parametrization of MC/data RefMult ratio
    Weight = Weight + (Weight-1.0)*(A*mVz*mVz); // z-dependent weight correction
  }

  // Special scale factor for global refmult
  // for others, scale factor = 1
  const Double_t scale = getScaleForWeight() ;
  Weight *= scale ;

  return Weight ;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getCentralityBin16() const
{
  Int_t CentBin16 = -1;

  // Invalid index
  if( !isIndexOk() ) return CentBin16 ;

  while(CentBin16 < mNCentrality && !isCentralityOk(CentBin16) )
  {
    CentBin16++;
  }

  // return -1 if CentBin16 = 16 (very large refmult, refmult>5000)
  return (CentBin16==16) ? -1 : CentBin16;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getCentralityBin9() const
{
  Int_t CentBin9 = -1;

  // Invalid index
  if ( !isIndexOk() ) return CentBin9 ;

  const Int_t CentBin16 = getCentralityBin16(); // Centrality bin 16
  const Bool_t isCentralityOk = CentBin16 >= 0 && CentBin16 < mNCentrality ;

  // No centrality is defined
  if (!isCentralityOk) return CentBin9 ;

  // First handle the exceptions
  if(mRefMult_corr > mCentrality_bins[15][mParameterIndex] && mRefMult_corr <= mCentrality_bins[16][mParameterIndex])
  {
    CentBin9 = 8; // most central 5%
  }
  else if(mRefMult_corr > mCentrality_bins[14][mParameterIndex] && mRefMult_corr <= mCentrality_bins[15][mParameterIndex])
  {
    CentBin9 = 7; // most central 5-10%
  }
  else
  {
    CentBin9 = (Int_t)(0.5*CentBin16);
  }

  return CentBin9;
}

//______________________________________________________________________________
const Char_t* StRefMultCorr::getTable() const
{
  if ( mName.CompareTo("refmult", TString::kIgnoreCase) == 0 ) {
    return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_refmult.txt";
  }
  else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) {
    return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_refmult2.txt";
  }
  else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) {
    return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_refmult3.txt";
  }
  else if ( mName.CompareTo("toftray", TString::kIgnoreCase) == 0 ) {
    return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_toftray.txt";
  }
  else if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) {
    return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_grefmult.txt";
  }
  else if ( mName.CompareTo("grefmult_P16id", TString::kIgnoreCase) == 0 ) {
      return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_grefmult_P16id.txt";
  }
  else if ( mName.CompareTo("grefmult_VpdMB30", TString::kIgnoreCase) == 0 ) {
      return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_grefmult_VpdMB30.txt";
  }
  else if ( mName.CompareTo("grefmult_VpdMBnoVtx", TString::kIgnoreCase) == 0 ) {
      return "/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/Centrality_def_grefmult_VpdMBnoVtx.txt";
  }
  else{
    Error("StRefMultCorr::getTable", "No implementation for %s", mName.Data());
    cout << "Current available option is refmult or refmult2 or refmult3 or toftray" << endl;
    return "";
  }
}
//______________________________________________________________________________
void StRefMultCorr::read()
{
  // Open the parameter file and read the data
  const Char_t* inputFileName(getTable());
  ifstream ParamFile(inputFileName);
  if(!ParamFile){
    Error("StRefMultCorr::read", "cannot open %s", inputFileName);
    return;
  }
  cout << "StRefMultCorr::read  Open " << inputFileName << flush ;

  string line ;
  getline(ParamFile,line);

  if(line.find("Start_runId")!=string::npos)
  {
    while(ParamFile.good())
    {
      Int_t year;
      Double_t energy;
      ParamFile >> year >> energy ;
      Int_t startRunId=0, stopRunId=0 ;
      Double_t startZvertex=-9999., stopZvertex=-9999. ;
      ParamFile >> startRunId >> stopRunId >> startZvertex >> stopZvertex ;
      // Error check
      if(ParamFile.eof()) break;

      mYear.push_back(year) ;
      mBeginRun.insert(std::make_pair(std::make_pair(energy, year), startRunId));
      mEndRun.insert(std::make_pair(std::make_pair(energy, year), stopRunId));

      mStart_runId.push_back( startRunId ) ;
      mStop_runId.push_back( stopRunId ) ;
      mStart_zvertex.push_back( startZvertex ) ;
      mStop_zvertex.push_back( stopZvertex ) ;
      for(Int_t i=0;i<mNCentrality;i++) {
	Int_t centralitybins=-1;
	ParamFile >> centralitybins;
	mCentrality_bins[i].push_back( centralitybins );
      }
      Double_t normalize_stop=-1.0 ;
      ParamFile >> normalize_stop ;
      mNormalize_stop.push_back( normalize_stop );
      for(Int_t i=0;i<mNPar_z_vertex;i++) {
	Double_t param=-9999.;
	ParamFile >> param;
	mPar_z_vertex[i].push_back( param );
      }
      for(Int_t i=0;i<mNPar_weight;i++) {
	Double_t param=-9999.;
	ParamFile >> param;
	mPar_weight[i].push_back( param );
      }

      for(Int_t i=0;i<mNPar_luminosity;i++) {
	Double_t param=-9999.;
	ParamFile >> param;
	mPar_luminosity[i].push_back( param );
      }
      mCentrality_bins[mNCentrality].push_back( 5000 );
    }
  }
  else
  {
    cout << endl;
    Error("StRefMultCorr::read", "Input file is not correct! Wrong structure.");
    return;
  }
  ParamFile.close();

  cout << " [OK]" << endl;
}

//______________________________________________________________________________
void StRefMultCorr::readBadRuns()
{
  // Read bad run numbers
  //   - From year 2010 - 2016
  //   - If input file doesn't exist, skip to the next year without warning
  for(Int_t i=0; i<7; i++) {
    cout << "StRefMultCorr::readBadRuns  For " << mName << ": open " << flush ;
    const Int_t year = 2010 + i ;
    Char_t* inputFileName(Form("/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/bad_runs_refmult_year%d.txt", year));
    if(mName.CompareTo("grefmult_P16id", TString::kIgnoreCase) == 0) //read bad runs for VPDMB5, add bad runs for Run16 (now no bad runs for Run16 available)
        sprintf(inputFileName,"/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/bad_runs_refmult_year%d_P16id.txt",year);
    else if(mName.CompareTo("grefmult_VpdMB30", TString::kIgnoreCase) == 0) //read bad runs for VPDMB30
        sprintf(inputFileName,"/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/bad_runs_refmult_year%d_VpdMB30.txt",year);
    else if(mName.CompareTo("grefmult_VpdMBnoVtx", TString::kIgnoreCase) == 0) //read bad runs for VPDMB-noVtx
        sprintf(inputFileName,"/gpfs01/star/pwg/mzyzak/Femto/Template/StRoot/StRefMultCorr/bad_runs_refmult_year%d_VpdMBnoVtx.txt",year);
    else
        ;//
    cout << "bad run file: " << inputFileName << endl;
    ifstream fin(inputFileName);
    if(!fin){
      //      Error("StRefMultCorr::readBadRuns", "can't open %s", inputFileName);
      cout << endl;
      continue;
    }
    cout << "  " << inputFileName << flush;

    Int_t runId = 0 ;
    while( fin >> runId ) {
      mBadRun.push_back(runId);
    }
    cout << " [OK]" << endl;
  }
}

//______________________________________________________________________________
void StRefMultCorr::print(const Option_t* option) const
{
  cout << "StRefMultCorr::print  Print input parameters for " << mName << " ========================================" << endl << endl;
  // Option switched off, can be used to specify parameters
  //  const TString opt(option);

  //  Int_t input_counter = 0;
  for(UInt_t id=0; id<mStart_runId.size(); id++) {
    //cout << "Data line = " << input_counter << ", Start_runId = " << Start_runId[input_counter] << ", Stop_runId = " << Stop_runId[input_counter] << endl;
    //    const UInt_t id = mStart_runId.size()-1;

    // Removed line break
    cout << "  Index=" << id;
    cout << Form(" Run=[%8d, %8d]", mStart_runId[id], mStop_runId[id]);
    cout << Form(" z-vertex=[%1.1f, %1.1f]", mStart_zvertex[id], mStop_zvertex[id]);
    cout << ", Normalize_stop=" << mNormalize_stop[id];
    cout << endl;

    //    if(opt.IsWhitespace()){
    //      continue ;
    //    }

    cout << "Centrality:  ";
    for(Int_t i=0;i<mNCentrality;i++){
      cout << Form("  >%2d%%", 80-5*i);
    }
    cout << endl;
    cout << "RefMult:     ";
    for(Int_t i=0;i<mNCentrality;i++){
      //      cout << Form("StRefMultCorr::read  Centrality %3d-%3d %%, refmult > %4d", 75-5*i, 80-5*i, mCentrality_bins[i][id]) << endl;
      const TString tmp(">");
      const TString centrality = tmp + Form("%d", mCentrality_bins[i][id]);
      cout << Form("%6s", centrality.Data());
    }
    cout << endl;

    for(Int_t i=0;i<mNPar_z_vertex;i++) {
      cout << "  mPar_z_vertex[" << i << "] = " << mPar_z_vertex[i][id];
    }
    cout << endl;
    for(Int_t i=0;i<mNPar_weight;i++) {
      cout << "  mPar_weight[" << i << "] = " << mPar_weight[i][id];
    }
    cout << endl;
    for(Int_t i=0;i<mNPar_luminosity;i++) {
      cout << "  mPar_luminosity[" << i << "] = " << mPar_luminosity[i][id];
    }
    cout << endl << endl;
  }
  cout << "=====================================================================================" << endl;
}

