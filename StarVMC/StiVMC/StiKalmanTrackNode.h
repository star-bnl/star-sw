#ifndef StiKalmanTrackNode_H
#define StiKalmanTrackNode_H 1
#define STI_NODE_DEBUG

#include <Stiostream.h>
#include <stdlib.h>
#include <stdexcept>
#include <math.h>
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StiHit.h"
#include "StiTreeNode.h"
#include "TRMatrix.h"
#include "FitParameters.h"
// StEvent
#include "StHelixModel.h"
class StiDetector;
typedef enum {
  kFailed = -1,         // could not find intersection
  kHit,                                
  kEdgePhiPlus, kEdgeZminus, kEdgePhiMinus, kEdgeZplus, 
  kMissPhiPlus, kMissZminus, kMissPhiMinus, kMissZplus,
  kEnded
} StiIntersection;

/*! \class StiKalmanTrackNode
  Work class used to handle Kalman filter information while
  constructing track nodes.  A node may or may not own a hit
  depending whether it lies on a measurement layer where a hit
  was found. A node can have 0, 1, or many children. 
  Nodes are nominally sequenced outside-in i.e. with decreasing 
  radius (or independent variable). The order can however be reversed.
  In anycase, the order should always be monotonically increasing 
  or decreasing.
  \author Claude A Pruneau
*/
enum eTrackNodeStatus {
  kTNReset 	= 0, // TrackNode after Reset
#if 0
  kTNRotBeg 	= 1, // TrackNode start rotation
  kTNRotEnd 	= 2, // TrackNode rotated, errmatrix 3d
  kTNProBeg     = 3, // TrackNode start propagation, errmatrix wrong
  kTNProEnd     = 4, // TrackNode end propagation, errmatrix still wrong
  kTNNudBeg     = 5, // TrackNode start nudge
#endif
  kTNFitBeg 	= 6, // TrackNode start fit
  
  kTNReady	=10, // TrackNode PropagateError called,errmatrix OK
#if 0
  kTNNudEnd     =11, // TrackNode end nudge
#endif
  kTNFitEnd 	=12, // TrackNode end fit
  kTNInit 	=13  // TrackNode Initialized
};
enum StiDirection {kOutsideIn=0, kInsideOut};

class StiKalmanTrackNode : public StiTreeNode {
 public:
  StiKalmanTrackNode(){Reset();}
  virtual ~StiKalmanTrackNode(){mId=-1;};
  const StiKalmanTrackNode& operator=(const StiKalmanTrackNode &node);  
  
  //! Resets the node to a "null" un-used state
  void Reset();
  //! Initialize this node with the given hit information
  void Initialize(StiHit*h);
  //! Sets the Kalman state of this node equal to that of the given node. 
  void SetState(const StiKalmanTrackNode * node);
  //! Extract state information from this node.
  void Get(Double_t *x, Double_t *cc, Double_t& chi2);
#if 1
  //! Extract state information from this node in Radial representation. For tpt errors
  void GlobalRadial(Double_t  x[6],Double_t  e[15]);
  //! Extract state information from this node in TPT representation.
  void GlobalTpt   (Float_t   x[6],Float_t   e[15]);
#endif
  Int_t Charge() const {return Fitted().Charge();}
  StThreeVectorF MomentumF() const {return StThreeVectorF(Fitted().PxyzG().GetArray());}
  StThreeVectorD Momentum()  const {return StThreeVectorD(Fitted().PxyzG().GetArray());}
  void Momentum(Double_t p[3], Double_t e[6]=0) const {Fitted().Momentum(p,e);}
  Double_t Curvature() const {return -Charge()/Pt()*Hz();}
  Double_t DipAngle() const {return TMath::Pi()/2 - Momentum().theta();}
  Double_t TanL() const {return TMath::Tan(DipAngle());}
  Double_t Pt() const {return Fitted().Pt();}
  Double_t P() const {return Momentum().mag();}
  //! Calculates and returns the Z mag field in the current point.
  //! units: pT[GeV] = Hz[kG]*Radcurv[cm]
  Double_t Hz() const;
  const Double_t *Gxyz() const          {return _Fitted.XyzG().GetArray();}
  Double_t x_g() const                  { return Gxyz()[0];}
  Double_t y_g() const                  { return Gxyz()[1];}
  Double_t z_g() const                  { return Gxyz()[2];}
  Double_t NormalX() const 		{ return Detector()->NormalRadius();}
  Double_t Y() const 		        { return Predicted().Y();}  
  Double_t Z() const 		        { return Predicted().Z();}
#if 0
  Double_t getEyy()   const 		{Int_t i = V().GetNrows() == 3 ? 2 : 0; return V()(i,i);}
  Double_t getEzz()   const		{Int_t i = V().GetNrows() == 3 ? 5 : 2; return V()(i,i);}
  Double_t getEyy()    			{Int_t i = V().GetNrows() == 3 ? 2 : 0; return V()(i,i);}
  Double_t getEzz()   			{Int_t i = V().GetNrows() == 3 ? 5 : 2; return V()(i,i);}
#endif
  StiHit * Hit() const 		        {return _hit;}
  void  SetHit(StiHit* hit)		{_hit   =hit;}
  const StiDetector *Detector() const   {return _detector;}
  void  SetDetector(const StiDetector *detector) {_detector = detector;}
  Double_t &chi2()                      {return *&_chi2;}
  Double_t  chi2()     const            {return _chi2;}
  Double_t Chi2 ()  const		{return _chi2;} 		
  void  setChi2(Double_t chi2)		{_chi2  =chi2;}
  Int_t State() const 		        {return _state;}
  void  SetInvalid()  			{ _state=0;}
  void  SetReady()  			{ _state=kTNReady;}
  Int_t IsValid()  const 		{return _state>=kTNReady;}
  Int_t IsPredicted() const 		{return _Predicted.P().GetSize();}
  Int_t IsFitted() const 		{return _Fitted.P().GetSize();}
  Int_t IsSmoothed() const 		{return _Smoothed.P().GetSize();}
  const Double_t *Pars() const          {return Fitted().P().GetArray();}
  Double_t getDiag(Int_t idx) const     {return Fitted().C()(idx,idx);}
  Int_t    HitCount () const		{return hitCount;}
  Int_t    NullCount() const       	{return nullCount;}
  Int_t    ContigHitCount () const 	{return contiguousHitCount ;}
  Int_t    ContigNullCount() const 	{return contiguousNullCount;}
  Char_t  &HitCount () 		        {return hitCount;}
  Char_t  &NullCount()        		{return nullCount;}
  Char_t  &ContigHitCount ()  		{return contiguousHitCount ;}
  Char_t  &ContigNullCount()  		{return contiguousNullCount;}
  Double_t TimeOfFlight(const FitParameters &Prediction);
  
  void     SetHitCand(Int_t nhits)	{mHitCand = nhits;}
  void     SetIHitCand(Int_t ihit)	{mIHitCand = ihit;}
  Int_t    HitCand() const		{return mHitCand;}
  Int_t    IHitCand() const		{return mIHitCand;}
  virtual  void Print(Option_t *opt="") const {cout << *this << endl;}
  static void Break(Int_t kase);
  static void PrintStep();
  Int_t IsDca()   const {return Fitted().Type() == FitParameters::kDca;}
  void LoadS2D(const StiKalmanTrackNode *node = 0, TRVector *refFitPar = 0);
  void LoadS2D(const StiDetector *detector,const FitParameters &FP, TRVector *refFitPar = 0);
  void LoadPrediction(StiKalmanTrackNode *node = 0);
  void LoadD2S(const StiDetector *detector, FitParameters &Prediction);
  void UpdatePrediction(const StiKalmanTrackNode *node, TRVector *refFitPar);
  //! Propagates a track encapsulated by the given node "p" to the given detector "tDet".
  //! starting node "p = 0" means "continue propagation"
  Int_t  Propagate(StiKalmanTrackNode *p, StiDetector *tDet, Int_t dir, TRVector *refFitPar = 0);
  Int_t  Propagate(StiKalmanTrackNode *p=0, Int_t dir=kOutsideIn, TRVector *refFitPar = 0);
  
  //! Propagates a track encapsulated by the given node "p" to the given vertex
  Bool_t Propagate(const StiKalmanTrackNode *p, StiHit * vertex, Int_t dir, TRVector *refFitPar = 0);
  
  Bool_t PropagateToBeam(const StiKalmanTrackNode *p, Int_t dir, TRVector *refFitPar = 0);
  Bool_t PropagateToRadius(StiKalmanTrackNode *pNode, Double_t radius,Int_t dir, TRVector *refFitPar = 0);
  //! Extrapolate the track parameters to radial position "x"  and return a point global coordinates along
  //! the track at that point.
  StThreeVectorD  PointAt(Double_t xk) const;
  Double_t EvaluateChi2(const StiHit *hit); 
  Int_t UpdateNode(); 
  Double_t Psi()     const {return Momentum().phi();} // Fitted()._eta();}
  Double_t WindowY();
  Double_t WindowZ();
  Double_t Length(const StThreeVectorD & delta, Double_t curv);
#if 0
  /*! kind controls error calculation from parameters kind = 1 => Predicted, kind = 2 => Smoothed, otherwise Fitted */
  void SetHitErrors(const StiHit *hit=0, Int_t kind = 0);
  TRSymMatrix GlobalHitErrs(const StiHit *hit) const;
#endif
  friend ostream& operator<<(ostream& os, const StiKalmanTrackNode& n);
  static Int_t  Debug()           {return _debug;}
  static void   SetDebug(Int_t m) {_debug = m;}
  static void   SetLaser(Int_t m) {_laser = m;}
  static void   PrintpT(const FitParameters &FP, Double_t chi2,const Char_t *opt="");
  static Char_t *PrintpTasString(const FitParameters &FP, Double_t chi2=-1,const Char_t *opt="");
  void          PrintpT(const Char_t *opt="");
  Int_t         FlipFlop() const 			{return mFlipFlop;}
  StTrackGeometry* Helix();
  static void   ResetComment(const Char_t *m = "") 	{comment = m; commentdEdx = "";}
  static const Char_t *Comment() 		{return comment.Data();}
  void SetdDdS(TRMatrix &p) {fdDdS = p;} // dP_k/dP_k-1
  void SetdSdD(TRMatrix &p) {fdSdD = p;}
  void SetdSdDI(TRMatrix &p) {fdSdDI = p;}
  void SetdDdD(TRMatrix &p) {fdDdD = p;}
  void SetdSdS(TRMatrix &p) {fdSdS = p;}
  
  const TRMatrix &dDdS() {return fdDdS;}
  const TRMatrix &dSdD() {return fdSdD;}
  const TRMatrix &dSdDI(){return fdSdDI;}
  const TRMatrix &dDdD() {return fdDdD;}
  const TRMatrix &dSdS() {return fdSdS;}
  const TRMatrix &F()    {return dSdS();}
  const TRVector    r() const {return _r;}
  const TRSymMatrix R() const {return _R;}
  TRVector         &r()       {return *&_r;}
  TRSymMatrix      &R()       {return *&_R;}
 public:
  const FitParameters  Fitted()    const {return _Fitted;}
  FitParameters &Fitted()          {return _Fitted;}
  const FitParameters  Predicted() const {return _Predicted;}
  FitParameters &Predicted()       {return _Predicted;}
  const FitParameters  Smoothed()  const {return _Smoothed;}
  FitParameters &Smoothed()        {return _Smoothed;}
  const TRSymMatrix &V(const FitParameters& Prediction)  const;
  const TRSymMatrix &V()                                 const {return V(_Fitted);}
 protected:   
  
  Char_t _beg[1];  
  Int_t _state;
  const StiDetector * _detector; 
  StiHit* _hit;
  Double_t _chi2;
  //!  Z mag field in units PGev = Hz*Rcm
  Char_t hitCount;
  Char_t nullCount;
  Char_t contiguousHitCount;
  Char_t contiguousNullCount;
  Char_t mFlipFlop;
  Char_t mHitCand;
  Char_t mIHitCand;
  Char_t   _end[1];
  FitParameters _Fitted;
  FitParameters _Predicted;
  FitParameters _Smoothed;
  
  TRMatrix    fdDdS; // Sti_old => PD_old  =  dPD_old /d Sti_old
  TRMatrix    fdSdD; // PD_new  => Sti_new =  dSti_new/dPD_new
  TRMatrix    fdSdDI;// PD_old  => Sti_new =  dSti_new/dPD_old
  TRMatrix    fdDdD; // PD_old  => PD_new  =  dPD_new/dPD_old (transported due to fortran convention)
  TRMatrix    fdSdS; // Sti_old => Sti_new =  dSti_new/dSti_old
  TRVector    _r;    // residual
  TRSymMatrix _R;    // cov.matrix of residuals
  //  debug variables
  static Int_t    _debug;
  static TString  comment;
  static TString  commentdEdx;
  static Int_t    _laser;
  static TRMatrix _H;
 public:
  Int_t mId;  //for debug only 
};
#endif

