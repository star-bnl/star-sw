///////////////////////////////////////////////////////
//  This is the V0 Track Class.
//
//  Writen by Mike Heffner 10/30/98
//  Changed to StFtpcV0Track by Janet Seyboth 10/23/00
//////////////////////////////////////////////////////

#include "StFtpcV0Track.hh"

StFtpcV0Track::StFtpcV0Track(StThreeVector<double> momentum,  
                  StThreeVector<double> origin,
                  double magField, double charge)
  : StPhysicalHelix(momentum,origin,magField,charge)
{
  mB=magField;
}

StFtpcV0Track::StFtpcV0Track()
:StPhysicalHelix(StThreeVector<double>(),StThreeVector<double>(),0,0)
{
  // cout<<"Constructing an empty Track"<<endl;
  mB=0;
}


StFtpcV0Track::~StFtpcV0Track()
{

}

void StFtpcV0Track::display() const
{
  // a simple display of the track

  cout<<endl<<"This is a track display"<<endl;
  cout<<"-----------------------------------------"<<endl;
  cout<<"Magnetic field(kG): "<<mB/kilogauss<<endl;
  cout<<"The Helix parameters: "<<endl<<*this<<endl;
  cout<<"-----------------------------------------"<<endl;
}
