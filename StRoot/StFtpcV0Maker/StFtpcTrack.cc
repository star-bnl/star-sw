///////////////////////////////////////////////////////
//  This is the Track Class.
//
//  Writen by Mike Heffner 10/30/98
///////////////////////////////////////////

#include "StFtpcTrack.hh"

StFtpcTrack::StFtpcTrack(StThreeVector<double> momentum,  
                  StThreeVector<double> origin,
                  double magField, double charge)
  : StPhysicalHelix(momentum,origin,magField,charge)
{
  mB=magField;
}

StFtpcTrack::StFtpcTrack()
:StPhysicalHelix(StThreeVector<double>(),StThreeVector<double>(),0,0)
{
  // cout<<"Constructing an empty Track"<<endl;
  mB=0;
}


StFtpcTrack::~StFtpcTrack()
{

}

void StFtpcTrack::display() const
{
  // a simple display of the track

  cout<<endl<<"This is a track display"<<endl;
  cout<<"-----------------------------------------"<<endl;
  cout<<"Magnetic field(kG): "<<mB/kilogauss<<endl;
  cout<<"The Helix parameters: "<<endl<<*this<<endl;
  cout<<"-----------------------------------------"<<endl;
}
