//StiHit.cxx
//M.L. Miller (Yale Software)
//04/01
#include "RVersion.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StiHit.h"
#include "StiDetector.h"
#include "TMath.h"
//_____________________________________________________________________________
void StiHit::setError(const StMatrixF& matrix)
{
  enum Labels {x=1, y=2, z=3};
  if (_V_g.GetSize() < 6) _V_g = TRSymMatrix(3);
  Double_t s[6] = {
    matrix(x,x),
    matrix(x,y), matrix(y,y),
    matrix(x,z), matrix(y,z), matrix(z,z)
  };
  _V_g = TRSymMatrix(3,s);
  return;
}
//_____________________________________________________________________________
/*! Streamer for StiHit objects. */
ostream& operator<<(ostream& os, const StiHit& hit) {
  if (hit.Detector()) os << hit.Detector()->GetName();
  os << endl <<"hit L:" << hit.Measurement() << " G:" << hit.Gxyz();
  if (hit.CovMatrix().GetSize()) os  << endl << "V: " << hit.CovMatrix();
  return os;
}
//_____________________________________________________________________________
Double_t StiHit::Value(int key) const
{
  Double_t value;
  switch (key)
    {
    case kZ: value = z_g(); break;
    case kR: value = TMath::Sqrt(x_g()*x_g()+y_g()*y_g()); break;
    case kPseudoRapidity: value = PseudoRapidity(); break;
    case kPhi: value = TMath::ATan2(y_g(),x_g());break;
    default: value = -999999.; break;
    }
  return value;  
}
  
//_____________________________________________________________________________
Double_t StiHit::PseudoRapidity() const
{
  Double_t r=TMath::Sqrt(x_g()*x_g()+y_g()*y_g());
  Double_t tanTheta = ::tan(::TMath::ATan2(r,z_g())/2.);
  if (tanTheta>0.)
    return -TMath::Log(tanTheta);
  else
    return 1.e10;
}
//_____________________________________________________________________________
void StiHit::Reset() {
  memset(mBeg, 0, mEnd-mBeg);
  _M.AdoptA(0,_local);
  _DriftVelocity.AdoptA(0,&_vy);
  _M_g.AdoptA(0,_global);
  _V_g.AdoptA(0,_cov);
  static UInt_t myCount=0;  
  mCount = ++myCount;
}
//_____________________________________________________________________________
void StiHit::setGlobal(const StiDetector * detector,
			      const StMeasuredPoint * stHit,
			      Float_t gx, Float_t gy, Float_t gz) {
  Double_t xyzG[3] = {gx, gy, gz};
  _M_g = TRVector(3,xyzG);
  if (detector)    {
    Double_t xyzL[3];
    detector->GetMatrix()->MasterToLocal(xyzG, xyzL);
    //    _M = TRVector(2,&xyzL[1]);
    _M = TRVector(2,&xyzL[1]);
  }  else    {    
    _M = TRVector(3,xyzG);
  }
  mTimesUsed = 0;
  _detector = detector;  _sthit = stHit;
  if (!stHit   ) return;
  if (!detector) return;
}
//________________________________________________________________________________
void StiHit::set(const StiDetector * detector,
		 const StMeasuredPoint * stHit,
		 Float_t xx, Float_t yy, Float_t zz, 
		 Float_t sxx, Float_t sxy, Float_t sxz, Float_t syy, Float_t syz, Float_t szz) {
  Double_t xyzL[3] = { xx, yy, zz};
  if (detector)    {
    _M = TRVector(2,&xyzL[1]);
    Double_t xyzG[3];
    detector->GetMatrix()->LocalToMaster(xyzL, xyzG);
    _M_g = TRVector(3,xyzG);
  }   else    {
    _M_g = TRVector(3,xyzL);
  }
  Double_t s[6] = {sxx,
		   sxy, syy,
		   sxz, syz, szz};
  _V_g = TRSymMatrix(3,s);
  mTimesUsed = 0;
  _detector = detector;
  _sthit = stHit;
  if (!stHit) return;
  assert( TMath::Abs(stHit->position().x()-x_g())< 1.e-6 &&
	  TMath::Abs(stHit->position().y()-y_g())< 1.e-6 &&
	  TMath::Abs(stHit->position().z()-z_g())< 1.e-6);
  
  if (!detector) return;
}
//_____________________________________________________________________________
void StiHit::set(const StiDetector * detector, Float_t y, Float_t z) {
  Reset();
  _detector = detector;
  _M = TRVector(2, y, z);
}
//_____________________________________________________________________________
void StiHit::makeDca() {
  Reset();
  if (_V_g.GetSize() < 6) _V_g = TRSymMatrix(3);
  _V_g(2,2) = 1e12; // szz
}
//_____________________________________________________________________________
int StiHit::IsDca() const {
  if (_detector) 	return 0;
  if (y() || z())	return 0;
  if (_V_g(2,2)<1000)	return 0;
  return 1;
}








