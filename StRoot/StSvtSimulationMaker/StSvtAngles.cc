#include <iostream.h>
#include <math.h>

//#include "StThreeVector.hh"
#include "StSvtAngles.hh"

//ClassImp(StSvtAngles)

StSvtAngles::StSvtAngles()
{

}

StSvtAngles::~StSvtAngles()
{

}

void StSvtAngles::svtTheta(const StThreeVector <double> & V, const StThreeVector<double> & uz)
{

 double thetaZ = 0;

 thetaZ = (V.x()*uz.x() + V.y()*uz.y() + V.z()*uz.z())/V.mag();
 mTheta = acos(thetaZ);

}

void StSvtAngles::svtTheta(double Vx, double Vy, double Vz, double ux,double uy,double uz)
{
 double thetaZ = 0;

 thetaZ = (Vx*ux + Vy*uy + Vz*uz)/sqrt(Vx*Vx + Vy*Vy + Vz*Vz);
 mTheta = acos(thetaZ);
}

void StSvtAngles::svtPhi(const StThreeVector <double>& V, const StThreeVector <double>& ux, const StThreeVector <double>& uy)
{
 
 double csPhi = 0, Vx = 0,  Vy = 0;

 //thetaX = (V.x()*ux.x() + V.y()*ux.y() + V.z()*ux.z())/sqrt(V.x()*V.x() + V.y()*V.y() + V.z()*V.z());
 //thetaY = (V.x()*uy.x() + V.y()*uy.y() + V.z()*uy.z())/sqrt(V.x()*V.x() + V.y()*V.y() + V.z()*V.z());
 
 // Vx = sqrt(V.x()*V.x() + V.y()*V.y() + V.z()*V.z())*thetaX;
 //Vy = qrt(V.x()*V.x() + V.y()*V.y() + V.z()*V.z())*thetaY;
 
 Vx = V.x()*ux.x() + V.y()*ux.y() + V.z()*ux.z(); 
 Vy = V.x()*uy.x() + V.y()*uy.y() + V.z()*uy.z();

 csPhi = Vx/sqrt(Vx*Vx + Vy*Vy);
 mPhi = acos(csPhi);

}
