// $Id: StProtoJet.cxx,v 1.15 2016/01/06 22:00:17 gdwebb Exp $

#include "StProtoJet.h"

using namespace std;

StProtoJet::StProtoJet()
  : mPx(0), mPy(0), mPz(0), mE(0)
  , mArea(-1.)
  , mAreaError(-1.)
  , _charge(0) 
{

}

StProtoJet::StProtoJet(const AbstractFourVec* particle)
  : mPx(particle->px()), mPy(particle->py()), mPz(particle->pz()), mE(particle->e())
  , mArea(-1.), mAreaError(-1.)
  , _charge(particle->charge())
{
  _particleList.push_back(particle);
}

StProtoJet::~StProtoJet()
{
}

void StProtoJet::update()
{
  mPx = mPy = mPz = mE = _charge = 0.;
  // add 4-momenta
  for (FourVecList::const_iterator particle = _particleList.begin(); particle != _particleList.end(); ++particle) {
    mPx += (*particle)->px();
    mPy += (*particle)->py();
    mPz += (*particle)->pz();
    mE  += (*particle)->e();
    _charge += (*particle)->charge();
  }
}

void StProtoJet::add(const StProtoJet& protoJet)
{
  mPx += protoJet.mPx;
  mPy += protoJet.mPy;
  mPz += protoJet.mPz;
  mE  += protoJet.mE;
  _charge += protoJet._charge;
  for (FourVecList::const_iterator particle = protoJet._particleList.begin(); particle != protoJet._particleList.end(); ++particle) {
    _particleList.push_back(*particle);
  }
}

void StProtoJet::add(const AbstractFourVec& particle)
{
  mPx += particle.px();
  mPy += particle.py();
  mPz += particle.pz();
  mE  += particle.e();
  _charge += particle.charge();
}

void StProtoJet::merge(const StProtoJet& protoJet)
{
  for (FourVecList::const_iterator particle = protoJet._particleList.begin(); particle != protoJet._particleList.end(); ++particle) {
    _particleList.push_back(*particle);
    mPx += (*particle)->px();
    mPy += (*particle)->py();
    mPz += (*particle)->pz();
    mE  += (*particle)->e();
    _charge += (*particle)->charge();
  }
}

void StProtoJet::clear()
{
  _particleList.clear();
  mPx = mPy = mPz = mE = _charge = 0;
}
