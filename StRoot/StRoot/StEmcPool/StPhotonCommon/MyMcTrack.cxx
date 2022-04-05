#include "MyMcTrack.h"

ClassImp(MyMcTrack)

MyMcTrack::MyMcTrack()
{
  fEnergy=0;
  fMomentum=TVector3();
  fPosition=TVector3();
  fPdgId=0;
  fStopRadius=0;
}
MyMcTrack::MyMcTrack(Float_t en,const TVector3& mom,const TVector3& pos,Int_t id,Float_t rad)
{
  fEnergy=en;
  fPosition=TVector3(pos);
  fMomentum=TVector3(mom);
  fPdgId=id;
  fStopRadius=rad;
}
MyMcTrack::MyMcTrack(MyMcTrack *tr)
{
  fEnergy=tr->energy();
  fPosition=TVector3(tr->position());
  fMomentum=TVector3(tr->momentum());
  fPdgId=tr->id();
  fStopRadius=tr->stopRadius();
}
MyMcTrack::~MyMcTrack()
{
  //
}
