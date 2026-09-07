#include "RHICfField.hh"

RHICfField::RHICfField()
{}
 
RHICfField::~RHICfField()
{}
 
void RHICfField::GetFieldValue(const double Point[3],double *Bfield) const
{
  //  double By=1.7578*CLHEP::tesla; /// <-wrong? (DX magnet design is: 4.28T)
  double By=-4.255*CLHEP::tesla; /// <-correct (field reversed)
  /// To avoid G4MultiLevelLocator::EstimateIntersectionPoint() 
  /// warning and Error messages,  rmax is slightly larger than
  /// the diameter of the beam pipe. (6.99->7.00)
  /// Strong magnetic field (~5Tesla) sometimes causes these problems.
  double rmax=7.00*CLHEP::cm;
  double zmin= 975.555*CLHEP::cm;
  double zmax=1354.445*CLHEP::cm;

  Bfield[0]=0.;
  Bfield[2]=0.;
  if(Point[2]>zmin && Point[2]<zmax && 
     (Point[0]*Point[0]+Point[1]*Point[1])<rmax*rmax) {
    Bfield[1]=By;
  }else{
    Bfield[1]=0.;
  }
}
