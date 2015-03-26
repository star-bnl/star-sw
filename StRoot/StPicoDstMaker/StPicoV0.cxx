#include "StPicoV0.h"
#include "StPicoDst.h"
#include "StPicoConstants.h"
#include "StPicoEvent.h"
#include "StPicoTrack.h"
#include "TMath.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StPhysicalHelixD.hh"
#include "StLorentzVectorF.hh"

ClassImp(StPicoV0)

StPicoV0::StPicoV0()
{
  Clear();
}

StPicoV0::StPicoV0(StPicoV0* t)
{
  if(!t) Clear();
  else {
    mIndex2Track[pos] = (Short_t)t->index2Track(pos);
    mIndex2Track[neg] = (Short_t)t->index2Track(neg);
    mMomentum[pos] = t->momentum(pos);
    mMomentum[neg] = t->momentum(neg);
    mV0Pos = t->v0Pos();
    mDcaDaughters = (UShort_t)(t->dcaDaughters()*1000.);
    mDca2Vtx      = (UShort_t)(t->dca2Vertex()*1000.);
    mM            = t->m();
  }
}

StPicoV0::~StPicoV0()
{ /*  */ }

StPicoV0::StPicoV0(StPicoTrack *t_pos, StPicoTrack *t_neg, StMuEvent *ev, Int_t *map2Track)
{
  if(!t_pos || !t_neg || !ev) {
    Clear();
  } else {
    double B = ev->magneticField();
    StThreeVectorF primaryVertex = ev->primaryVertexPosition();

    StDcaGeometry *dcaG_pos = new StDcaGeometry();
    dcaG_pos->set(t_pos->params(),t_pos->errMatrix());
    StPhysicalHelixD helix_pos = dcaG_pos->helix();

    StDcaGeometry *dcaG_neg = new StDcaGeometry();
    dcaG_neg->set(t_neg->params(),t_neg->errMatrix());
    StPhysicalHelixD helix_neg = dcaG_neg->helix();

/*
    StPhysicalHelixD helix_pos(t_pos->gMom(), t_pos->origin(), B*kilogauss, t_pos->charge());
    StPhysicalHelixD helix_neg(t_neg->gMom(), t_neg->origin(), B*kilogauss, t_neg->charge());
*/

    double res = 3.0;  // allow resolution
    double xc_pos = helix_pos.xcenter();
    double yc_pos = helix_pos.ycenter();
    double xc_neg = helix_neg.xcenter();
    double yc_neg = helix_neg.ycenter();
    double dd = TMath::Sqrt((xc_pos-xc_neg)*(xc_pos-xc_neg)+(yc_pos-yc_neg)*(yc_pos-yc_neg));
    double r_pos = 1./helix_pos.curvature();
    double r_neg = 1./helix_neg.curvature();

    if( fabs(r_pos-r_neg)-res < dd && dd < r_pos+r_neg+res ) {

    pair<double,double> s = helix_pos.pathLengths(helix_neg);
    StThreeVectorF dcaP_pos = helix_pos.at(s.first);
    StThreeVectorF dcaP_neg = helix_neg.at(s.second);
    float dcaDaughters = (dcaP_pos - dcaP_neg).mag();
    if(dcaDaughters<Pico::mV0DcaDaughtersMax) {

    StThreeVectorF v0 = (dcaP_pos+dcaP_neg)*0.5;
    StThreeVectorF mom_pos = helix_pos.momentumAt(s.first, B*kilogauss);
    StThreeVectorF mom_neg = helix_neg.momentumAt(s.second, B*kilogauss);
    StThreeVectorF mom_v0 = mom_pos + mom_neg;
    float angle = (v0-primaryVertex).angle(mom_v0);
    float dca2vtx = (v0-primaryVertex).mag()*TMath::Sin(angle);

    mIndex2Track[pos] = map2Track[t_pos->id()];
    mIndex2Track[neg] = map2Track[t_neg->id()];
    mMomentum[pos] = mom_pos;
    mMomentum[neg] = mom_neg;
    mV0Pos = v0;
    mDcaDaughters = (dcaDaughters*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(dcaDaughters*1000.);
    mDca2Vtx      = (dca2vtx*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(dca2vtx*1000.);
    mM = 0;   // invMass need to be calculated with particle hypothesis
    } else {
    Clear();
    }

    }

    delete dcaG_pos;
    delete dcaG_neg;
  }
}

void StPicoV0::Clear(const Option_t* opt)
{
  mIndex2Track[pos] = -1;       mIndex2Track[neg] = -1;
  mMomentum[pos].set(0.,0.,0.); mMomentum[neg].set(0.,0.,0.);
  mV0Pos.set(-999.,-999.,-999.);
  mDcaDaughters = Pico::USHORTMAX;
  mDca2Vtx      = Pico::USHORTMAX;
  mM            = 0.;
}

Int_t StPicoV0::index2Track(const Int_t i) const
{
  if(i==pos||i==neg) return (Int_t)mIndex2Track[i];
  else return -1;
}

StPicoTrack* StPicoV0::track(const Int_t i) const
{
  if(i==pos||i==neg) return (StPicoTrack*)StPicoDst::picoArray(picoTrack)->UncheckedAt(mIndex2Track[i]);
  else return 0;
}

StThreeVectorF StPicoV0::momentum(const Int_t i) const
{
  StThreeVectorF ret(0.,0.,0.);
  if(i==pos||i==neg) return mMomentum[i];
  else return ret;
}

void StPicoV0::setIndex2Track(const Int_t id_pos, const Int_t id_neg)
{
  mIndex2Track[pos] = (UShort_t)id_pos;
  mIndex2Track[neg] = (UShort_t)id_neg;
}

void StPicoV0::setParticleHypothesis(const Int_t ip_pos, const Int_t ip_neg)
{
  if(ip_pos<0 || ip_pos>=nPar || ip_neg<0 || ip_neg>=nPar) return;
  StLorentzVectorF fourMom_pos(mMomentum[pos], (Float_t)TMath::Sqrt(mMomentum[pos].mag2()+Pico::mMass[ip_pos]*Pico::mMass[ip_pos]));
  StLorentzVectorF fourMom_neg(mMomentum[neg], (Float_t)TMath::Sqrt(mMomentum[neg].mag2()+Pico::mMass[ip_neg]*Pico::mMass[ip_neg]));
  mM = (fourMom_pos + fourMom_neg).m();
}

Float_t StPicoV0::decayLength() const
{
  StPicoEvent *ev = (StPicoEvent *)StPicoDst::picoArray(picoEvent)->UncheckedAt(0);
  return (mV0Pos - ev->primaryVertex()).mag();
}

StThreeVectorF StPicoV0::momentum() const
{
  return mMomentum[pos] + mMomentum[neg];
}

void StPicoV0::rotateTrack(const Int_t i)
{
  if(i!=pos&&i!=neg) {
    return;
  }
  mMomentum[i].set(-1.*mMomentum[i].x(), -1.*mMomentum[i].y(), mMomentum[i].z());
}
