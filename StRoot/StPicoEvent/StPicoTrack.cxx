//
// StPicoTrack holds information about the reconstructed tracks
//

// C+++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoTrack.h"

#if defined (__TFG__VERSION__)
#include "TF1.h"
#include "St_base/StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StBichsel/Bichsel.h"
#include "StBichsel/StdEdxModel.h"
#include "StBichsel/StdEdxPull.h"
#endif /* __TFG__VERSION__ */

ClassImp(StPicoTrack)

//_________________
StPicoTrack::StPicoTrack() : TObject(),
  mId(0),
  mChi2(std::numeric_limits<unsigned short>::max()),
  mPMomentumX(0), mPMomentumY(0), mPMomentumZ(0),
  mGMomentumX(0), mGMomentumY(0), mGMomentumZ(0),
  mOriginX(0),mOriginY(0), mOriginZ(0),
  mDedx(0), mDedxError(0),
#if defined (__TFG__VERSION__)
  mDnDx(0),mDnDxError(0),
#endif
  mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0),
  mNSigmaPion( std::numeric_limits<short>::min() ),
  mNSigmaKaon( std::numeric_limits<short>::min() ),
  mNSigmaProton( std::numeric_limits<short>::min() ),
  mNSigmaElectron( std::numeric_limits<short>::min() ),
  mTopologyMap{}, mBEmcPidTraitsIndex(-1), mBTofPidTraitsIndex(-1),
  mMtdPidTraitsIndex(-1), mETofPidTraitsIndex(-1),
  mBEmcMatchedTowerIndex(-1)
#if !defined (__TFG__VERSION__)
  , mTopoMap_iTpc(0),
#endif
  mIdTruth(0), mQATruth(0), mVertexIndex(-1) {
  // Default constructor
  /* empty */
}

//_________________
StPicoTrack::StPicoTrack(const StPicoTrack &track) : TObject() {

  mId = track.mId;
  mChi2 = track.mChi2;
  mPMomentumX = track.mPMomentumX;
  mPMomentumY = track.mPMomentumY;
  mPMomentumZ = track.mPMomentumZ;
  mGMomentumX = track.mGMomentumX;
  mGMomentumY = track.mGMomentumY;
  mGMomentumZ = track.mGMomentumZ;
  mOriginX = track.mOriginX;
  mOriginY = track.mOriginY;
  mOriginZ = track.mOriginZ;
  mDedx = track.mDedx;
  mDedxError = track.mDedxError;
#if defined (__TFG__VERSION__)
  mDnDx = track.mDnDx;
  mDnDxError = track.mDnDxError;
#endif
  mNHitsFit = track.mNHitsFit;
  mNHitsMax = track.mNHitsMax;
  mNHitsDedx = track.mNHitsDedx;
  mNSigmaPion = track.mNSigmaPion;
  mNSigmaKaon = track.mNSigmaKaon;
  mNSigmaProton = track.mNSigmaProton;
  mNSigmaElectron = track.mNSigmaElectron;
  for(int iIter=0; iIter<eTopologyMap; iIter++) {
    mTopologyMap[iIter] = track.mTopologyMap[iIter];
  }
  mBEmcPidTraitsIndex = track.mBEmcPidTraitsIndex;
  mBTofPidTraitsIndex = track.mBTofPidTraitsIndex;
  mMtdPidTraitsIndex = track.mMtdPidTraitsIndex;
  mETofPidTraitsIndex = track.mETofPidTraitsIndex;
#if !defined (__TFG__VERSION__)
  mTopoMap_iTpc = track.mTopoMap_iTpc;
#endif
  mIdTruth = track.mIdTruth;
  mQATruth = track.mQATruth;
  mVertexIndex = track.mVertexIndex;
}

//_________________
StPicoTrack::~StPicoTrack() {
  /* emtpy */
}

//_________________
void StPicoTrack::Print(const Char_t* option __attribute__((unused))) const {
  LOG_INFO << "id: " << id() << " chi2: " << chi2() << "\n"
           << "pMom: " << pMom().X() << " " << pMom().Y() << " " << pMom().Z() << "\n"
           << "gMom: " << gMom().X() << " " << gMom().Y() << " " << gMom().Z() << "\n"
           << "origin: " << origin().X() << " " << origin().Y() << " " << origin().Z() << "\n"
           << "nHitsFit: " << nHitsFit()
           << " nHitsdEdx: " << nHitsDedx() << "\n"
           << "nSigma pi/K/p/e: " << nSigmaPion()   << "/" << nSigmaKaon() << "/"
           << nSigmaProton() << "/" << nSigmaElectron() << "\n"
           << "Hit index in BEMC/BTof/MTD/ETof: " << mBEmcPidTraitsIndex << "/"
           << mBTofPidTraitsIndex << "/" << mMtdPidTraitsIndex << "/" << mETofPidTraitsIndex << "\n"
           << "idTruth: " << idTruth() << " qaTruth: " << qaTruth() << "\n"
           << endm;
}

//_________________
Float_t StPicoTrack::gDCAxy(Float_t x, Float_t y) const {
  return TMath::Sqrt( (mOriginX-x)*(mOriginX-x) + (mOriginY-y)*(mOriginY-y) );
}

//_________________
Float_t StPicoTrack::gDCA(Float_t x, Float_t y, Float_t z) const {
  return TMath::Sqrt( (mOriginX-x)*(mOriginX-x) +
		      (mOriginY-y)*(mOriginY-y) +
		      (mOriginZ-z)*(mOriginZ-z) );
}

//_________________
TVector3 StPicoTrack::gDCA(TVector3 pVtx) const {
  return (origin() - pVtx);
}

//_________________
void StPicoTrack::setChi2(Float_t chi2) {
  mChi2 = ( (chi2 * 1000.) > std::numeric_limits<unsigned short>::max() ?
	    std::numeric_limits<unsigned short>::max() :
	    (UShort_t)( TMath::Nint( chi2 * 1000. ) ) );
}

//_________________
void StPicoTrack::setDedx(Float_t dEdx) {
  // In KeV/cm
  mDedx = dEdx * 1.e6;
}

//_________________
void StPicoTrack::setNHitsMax(Int_t nhits) {
  mNHitsMax = (UChar_t)nhits;
}

//_________________
void StPicoTrack::setNHitsPossible(Int_t nhits) {
  // For those who wants to have standard terminology
  setNHitsMax(nhits);
}

//_________________
void StPicoTrack::setNHitsDedx(Int_t nhits) {
  mNHitsDedx = (UChar_t)nhits;
}

//_________________
void StPicoTrack::setTopologyMap(Int_t id, UInt_t word) {
  if(id==0 || id==1 || id==2) {
    mTopologyMap[id] = word;
  }
  else {
    // Shouldn't here be a protection?
  }
}

//_________________
void StPicoTrack::setNSigmaPion(Float_t ns) {
  mNSigmaPion = ( fabs(ns * 1000.) > std::numeric_limits<short>::max() ?
		  ( (ns > 0) ? std::numeric_limits<short>::max() :
		    std::numeric_limits<short>::min() ) :
		  (Short_t)( TMath::Nint( ns * 1000. ) ) );
}

//_________________
void StPicoTrack::setNSigmaKaon(Float_t ns) {
  mNSigmaKaon = ( fabs(ns * 1000.) > std::numeric_limits<short>::max() ?
		  ( (ns > 0) ? std::numeric_limits<short>::max() :
		    std::numeric_limits<short>::min() ) :
		  (Short_t)( TMath::Nint( ns * 1000. ) ) );
}

//_________________
void StPicoTrack::setNSigmaProton(Float_t ns) {
  mNSigmaProton = ( fabs(ns * 1000.) > std::numeric_limits<short>::max() ?
		    ( (ns > 0) ? std::numeric_limits<short>::max() :
		      std::numeric_limits<short>::min() ) :
		    (Short_t)( TMath::Nint( ns * 1000. ) ) );
}

//_________________
void StPicoTrack::setNSigmaElectron(Float_t ns) {
  mNSigmaElectron = ( fabs(ns * 1000.) > std::numeric_limits<short>::max() ?
		      ( (ns > 0) ? std::numeric_limits<short>::max() :
			std::numeric_limits<short>::min() ) :
		      (Short_t)( TMath::Nint( ns * 1000. ) ) );
}

//_________________
TVector3 StPicoTrack::gMom(TVector3 pVtx, Float_t const B) const {
  StPicoPhysicalHelix gHelix = helix(B);
  return gHelix.momentumAt( gHelix.pathLength( pVtx ), B * kilogauss );
}

//_________________
StPicoPhysicalHelix StPicoTrack::helix(Float_t const B) const {
  return StPicoPhysicalHelix( gMom(), origin(), B * kilogauss,
			      static_cast<float>( charge() ) );
}

#if defined (__TFG__VERSION__)
//_________________
Float_t StPicoTrack::dEdxPull(Float_t mass, UChar_t fit, Int_t charge) const {
  Float_t z = -999.;
  Float_t momentum  = gMom().Mag();
  Float_t betagamma = momentum * TMath::Abs(charge) / mass;
  Float_t dedx_measured, dedx_resolution = -1;
  if (! fit) { // I70
    dedx_measured = 1e-6*dEdx();
    dedx_resolution = dEdxError();
  }
  else if ( fit == 1) {     // Ifit
    dedx_measured = 1e-6*dEdx();
    dedx_resolution = dEdxError();
  }
  else {     // dNdx
    dedx_measured = dNdx();
    dedx_resolution = dNdxError();
  }
  if (dedx_resolution <= 0) return z;
  z = StdEdxPull::Eval(dedx_measured,dedx_resolution,betagamma,fit,charge);
  return z;
}

//_________________
Float_t StPicoTrack::dEdxPullToF(Float_t mass, UChar_t fit, Int_t charge) const {
  Float_t z = -999.;
  Float_t momentum  = gMom().Mag();
  Float_t betagamma = momentum * TMath::Abs(charge) / mass;
  Float_t dedx_measured, dedx_resolution = -1;
  Float_t dedx_predicted = -1;
  static TF1 *ToFCor[2][3] = {0};
  
#if 1
  if (! ToFCor[0][0]) {
    /* Maksym, 11/22/17
       dE/dx correction Pion:
       p0                        =   -0.0583149   +/-   0.000381198 
       p1                        =     0.418013   +/-   0.00692832  
       p2                        =     -2.13934   +/-   0.0494599   
       p3                        =      8.50604   +/-   0.182424    
       p4                        =     -19.3807   +/-   0.384702    
       p5                        =       24.777   +/-   0.480388    
       p6                        =     -17.7556   +/-   0.350305    
       p7                        =      6.67151   +/-   0.137513    
       p8                        =     -1.02484   +/-   0.0224013

       ToFCor[0][0] = new TF1("dEdxCorPion","pol8",-0.05,1.7); 
       ToFCor[0][0]->SetParameters(-0.0583149, 0.418013, -2.13934,  8.50604, -19.3807, 
       24.777, -17.7556,  6.67151, -1.02484);
    */

    ToFCor[0][0] = new TF1("dEdxCorPion","pol8",0,1.4);
    ToFCor[0][0]->SetParameters(-0.0686941, 1.00512, -7.01202, 26.5077, -55.6186,
				67.2198, -46.5966, 17.194, -2.61537);
    /*
      Kaon:
      p0                        =    0.0159834   +/-   5.0974e-05  
      p1                        =    -0.115402   +/-   0.00053059  
      p2                        =     0.429126   +/-   0.00363743  
      p3                        =     0.569058   +/-   0.021112    
      p4                        =     -3.00964   +/-   0.0443932   
      p5                        =      1.96151   +/-   0.223594    
      p6                        =      4.58199   +/-   0.54901     
      p7                        =     -7.40423   +/-   0.54138     
      p8                        =      3.00975   +/-   0.190495    

      ToFCor[0][1] = new TF1("dEdxCorKaon","pol8",-0.30,1.1);
      ToFCor[0][1]->SetParameters(0.0159834,-0.115402, 0.429126, 0.569058, -3.00964,
      1.96151,  4.58199, -7.40423, 3.00975);
    */

    ToFCor[0][1] = new TF1("dEdxCorKaon","pol8",-0.30,1);
    ToFCor[0][1]->SetParameters(0.0103183, -0.113391, 0.347118, 0.939172, -2.85298,
				-2.16397,  12.1903, -12.2671, 3.9317);
    /*
      Proton:

      p0                        =    0.0224256   +/-   3.17809e-05 
      p1                        =    -0.111028   +/-   0.000293992 
      p2                        =     0.389417   +/-   0.00153563  
      p3                        =     0.714018   +/-   0.00741038  
      p4                        =     -3.01122   +/-   0.0184805   
      p5                        =    -0.463794   +/-   0.0523714   
      p6                        =      6.12037   +/-   0.082       
      p7                        =     -1.32531   +/-   0.100664    
      p8                        =     -2.51178   +/-   0.128703

      ToFCor[0][2] = new TF1("dEdxCorProt","pol8",-0.65,0.9);
      ToFCor[0][2]->SetParameters(0.0224256,-0.111028, 0.389417, 0.714018, -3.01122,
      -0.463794,  6.12037, -1.32531, -2.51178);
    */
    
    ToFCor[0][2] = new TF1("dEdxCorProt","pol8",-0.5,0.75);
    ToFCor[0][2]->SetParameters(0.0241608, -0.122085, 0.409931, 0.772181, -2.82003,
				-1.15634,  5.60995, 0.425809, -3.1966);
    /*
      ================================================================================
      dNdX correction: Pion:
      p0                        =    -0.127892   +/-   0.000352695 
      p1                        =      1.13166   +/-   0.00649171  
      p2                        =     -6.28713   +/-   0.0471639   
      p3                        =      22.8583   +/-   0.177002    
      p4                        =     -48.3574   +/-   0.379105    
      p5                        =      59.3737   +/-   0.479759    
      p6                        =     -41.8384   +/-   0.353823    
      p7                        =      15.6944   +/-   0.140222    
      p8                        =     -2.42872   +/-   0.0230262   

      ToFCor[1][0] = new TF1("dNdxCorPion","pol8",-0.05,1.7);
      ToFCor[1][0]->SetParameters(-0.127892, 1.13166, -6.28713, 22.8583, -48.3574,  59.3737,
      -41.8384,  15.6944,-2.42872);
    */

    ToFCor[1][0] = new TF1("dNdxCorPion","pol8",0,1.4);
    ToFCor[1][0]->SetParameters(-0.0790884, 0.960891, -7.18579, 29.6238, -65.671,
				82.2409, -58.5028, 22.0458, -3.41535);
    /*    
	  Kaon:

	  p0                        =   -0.0324453   +/-   6.52723e-05 
	  p1                        =     0.104644   +/-   0.000668736 
	  p2                        =     0.232821   +/-   0.00461983  
	  p3                        =    -0.205617   +/-   0.0263313   
	  p4                        =    -0.942393   +/-   0.0551658   
	  p5                        =      2.28523   +/-   0.279332    
	  p6                        =     -1.43567   +/-   0.678682    
	  p7                        =    -0.558613   +/-   0.664332    
	  p8                        =     0.608146   +/-   0.232324    
	  12/04/17
	  0.0116721,-0.106367, 0.224163,  1.12377,-0.176388,  -13.855,  27.2605, -13.9522, -1.64127

	  ToFCor[1][1] = new TF1("dNdxCorKaon","pol8",-0.30,1.1);
	  ToFCor[1][1]->SetParameters(-0.0324453, 0.104644, 0.232821, -0.205617, -0.942393,
	  2.28523, -1.43567, -0.558613, 0.608146);
	  ToFCor[1][1] = new TF1("dNdxCorKaon","pol8",-0.30,1.1);
	  ToFCor[1][1]->SetParameters(0.0116721,-0.106367, 0.224163,  1.12377,-0.176388,  -13.855,
	  27.2605, -13.9522, -1.64127);
    */

    
    ToFCor[1][1] = new TF1("dNdxCorKaon","pol8",-0.3,1);
    ToFCor[1][1]->SetParameters(-0.0165074, 0.00880377, 0.181553, 0.723291, -1.69666,
				-2.88222, 9.83642, -8.58367, 2.45608);
    /*
      Proton:
      p0                        =   -0.0216107   +/-   4.06732e-05 
      p1                        =     0.095628   +/-   0.000380259 
      p2                        =     0.221496   +/-   0.00191754  
      p3                        =    -0.281845   +/-   0.00960846  
      p4                        =     -1.81734   +/-   0.0226373   
      p5                        =      3.55535   +/-   0.0683598   
      p6                        =      1.78527   +/-   0.100257    
      p7                        =      -8.3076   +/-   0.132161    
      p8                        =      5.20165   +/-   0.161309

      ToFCor[1][2] = new TF1("dNdxCorProt","pol8",-0.65,0.9); 
      ToFCor[1][2]->SetParameters(-0.0216107,  0.095628,  0.221496, -0.281845,  -1.81734,
      3.55535,   1.78527,   -8.3076,  5.20165);  
    */    

    ToFCor[1][2] = new TF1("dNdxCorProt","pol8",-0.5,0.75);
    ToFCor[1][2]->SetParameters(-0.0254615, 0.0610245,  0.364898, 0.0783227,  -2.27553,
				0.317491,   4.55173,  -1.44629,  -1.70712);
  }
#endif
  
  Int_t hyp = -1; 
  if (mass < 1.0) hyp = 2; 
  if (mass < 0.5) hyp = 1;
  if (mass < 0.2) hyp = 0;
  if (mass < 0.1) hyp = -1; // no ocrrection for electron
  Double_t bgL10 = TMath::Log10(betagamma);
  if ( fit <= 1) { // I70
    dedx_measured = 1e-6*dEdx();
    dedx_resolution = dEdxError();
    dedx_predicted = 1.e-6 * charge * charge * TMath::Exp(Bichsel::Instance()->GetMostProbableZ(bgL10));
    if (ToFCor[0][hyp] &&hyp >= 0 && bgL10 > ToFCor[0][hyp]->GetXmin() &&
	bgL10 < ToFCor[0][hyp]->GetXmax()) {
      dedx_predicted *= TMath::Exp(ToFCor[0][hyp]->Eval(bgL10));
    }
  }
  else {     // dNdx
    dedx_measured = dNdx();
    dedx_resolution = dNdxError();
    dedx_predicted = StdEdxModel::instance()->dNdx(betagamma,charge);
    if (ToFCor[0][hyp] && hyp >= 0 && bgL10 > ToFCor[0][hyp]->GetXmin() &&
	bgL10 < ToFCor[0][hyp]->GetXmax()) {
      dedx_predicted *= TMath::Exp(ToFCor[1][hyp]->Eval(bgL10));
    }
  }
  if (dedx_resolution <= 0) return z;
  z = TMath::Log(dedx_measured/dedx_predicted) / dedx_resolution;
  return z;
}

#endif /* __TFG__VERSION__ */

//_________________
Float_t StPicoTrack::gDCAs(TVector3 point) const {
  // Signed DCA is defined for tracks with primary partners
  // and with non-zero global track momentum
  if ( (gMom().Mag() == 0 ) || ( !isPrimary() ) ) {
    return -999;
  }
  // Momentum of the global track
  TVector3 dir = gMom();
  // Unit vector
  dir = dir.Unit();
  Float_t cosl = dir.Perp();
  // Return DCA vector to the point (origin - point)
  TVector3 dca = gDCA( point );
  return -dir.Y()/cosl * dca.X() + dir.X()/cosl * dca.Y();
}

//_________________
void StPicoTrack::setVertexIndex(Int_t index) {
  if ( index<=-2 || index>std::numeric_limits<char>::max() ) {
    mVertexIndex = -2;
  }
  else {
    mVertexIndex = (Char_t)index;
  }
}
