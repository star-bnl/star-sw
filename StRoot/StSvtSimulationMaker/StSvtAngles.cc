#include <iostream.h>
#include <math.h>

//#include "StThreeVector.hh"
#include "tables/St_svg_geom_Table.h"
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

void StSvtAngles::calcAngles(svg_geom_st *geom_st, double x, double y, double z, int mLayer, int mLadder, int mWafer )
{
  int hardWarePosition ,index = 0;
  StThreeVector<double> mom(0,0,0);
  StThreeVector<double> uVecN(0,0,0);
  StThreeVector<double> uVecD(0,0,0);
  StThreeVector<double> uVecT(0,0,0);

  //hardWarePosition = getLayerID()*1000 + 100*wafer + ladder;

   hardWarePosition = mLayer*1000 + 100*mWafer + mLadder;

   for( index=0; index < 216; index++){
    if( geom_st[index].id == hardWarePosition) 
       break;
     }
    
    mom.setX(x);
    mom.setY(y);
    mom.setZ(z);

    uVecN.setX(geom_st[index].n[0]);
    uVecN.setY(geom_st[index].n[1]);
    uVecN.setZ(geom_st[index].n[2]);

    uVecD.setX(geom_st[index].d[0]);
    uVecD.setY(geom_st[index].d[1]);
    uVecD.setZ(geom_st[index].d[2]);

    uVecT.setX(geom_st[index].t[0]);
    uVecT.setY(geom_st[index].t[1]);
    uVecT.setZ(geom_st[index].t[2]);

    svtTheta(mom,uVecN);
    svtPhi(mom,uVecD,uVecT);

}
