#include "StBbcGeom.h"
#include "TMath.h"
#include <iostream>

using namespace std;

ClassImp(StBbcGeom);

StBbcGeom::StBbcGeom(){
  InitializeGeometry();
}

StBbcGeom::~StBbcGeom(){
  /* no-op */
}

void StBbcGeom::InitializeGeometry(){

  double h=9.65;     // height of the hexagonal inner tiles, in cm.
  double zBBC = 380;  // 5 cm behind the EPD

  //---------- corners of a hex tile centered at (x,y)=(0,0) -------
  double xh = h/(4.0*sin(60.0*TMath::Pi()/180.0));
  double yh = h/2.0;
  mX[0] = -2.0*xh;    mY[0] = 0.0;
  mX[1] = -xh;        mY[1] = yh;
  mX[2] = xh;         mY[2] = yh;
  mX[3] = 2.0*xh;     mY[3] = 0.0;
  mX[4] = xh;         mY[4] = -yh;
  mX[5] = -xh;        mY[5] = -yh;
  //----------------------------------------------------------------

  //---------- locations of centers of tiles -----------------------  
  double radius,phi,xc,yc,zc;

  // first, East:
  for (int iTile=1; iTile<19; iTile++){
    if (iTile<7){                                                           // Tiles 1-6
      radius = h;
      phi = TMath::Pi()/2.0 + (iTile-1)*60.0*TMath::Pi()/180.0;
    }
    if ((iTile>6)&&(iTile%2==0)){                                            // tiles 8,10,12,14,16,18
      radius = 2.0*h;
      phi = TMath::Pi()/2.0 +(iTile/2-4)*60.0*TMath::Pi()/180.0;
    }
    if ((iTile>6)&&(iTile%2==1)){                                            // tiles 7,9,11,13,15,17
      radius = h*(1.0+cos(60.0*TMath::Pi()/180.0))/sin(60.0*TMath::Pi()/180.0);
      phi = TMath::Pi()*60.0/180.0 + ((iTile-1)/2-3)*60*TMath::Pi()/180.0;
    }
    xc = radius*cos(phi);
    yc = radius*sin(phi);
    zc = -zBBC;
    mEastTileCenter[iTile-1].SetXYZ(xc,yc,zc);
  }

  // then, West:
  for (int iTile=1; iTile<19; iTile++){
    if (iTile<7){                                                           // Tiles 1-6
      radius = h;
      phi = TMath::Pi()/2.0 - (iTile-1)*60.0*TMath::Pi()/180.0;
    }
    if ((iTile>6)&&(iTile%2==0)){                                            // tiles 8,10,12,14,16,18
      radius = 2.0*h;
      phi = TMath::Pi()/2.0 -(iTile/2-4)*60.0*TMath::Pi()/180.0;
    }
    if ((iTile>6)&&(iTile%2==1)){                                            // tiles 7,9,11,13,15,17
      radius = h*(1.0+cos(60.0*TMath::Pi()/180.0))/sin(60.0*TMath::Pi()/180.0);
      phi = TMath::Pi()*120.0/180.0 - ((iTile-1)/2-3)*60*TMath::Pi()/180.0;
    }
    xc = radius*cos(phi);
    yc = radius*sin(phi);
    zc = zBBC;
    mWestTileCenter[iTile-1].SetXYZ(xc,yc,zc);
  }
}
  
TVector3 StBbcGeom::TileCenter(short tileId){
  short index = abs(tileId) - 1;
  if (tileId<0){return mEastTileCenter[index];}
  else {return mWestTileCenter[index];}
}

void StBbcGeom::GetCorners(short tileId, double* x, double* y){
  double xcent = this->TileCenter(tileId).X();
  double ycent = this->TileCenter(tileId).Y();
  for (int i=0; i<6; i++){
    x[i] = mX[i]+xcent;
    y[i] = mY[i]+ycent;
  }

  // cout << "Returning these corners for tile " << tileId << endl;
  // for (int i=0; i<6; i++){
  //   cout << x[i] << ", " << y[i] << endl;
  // }

}

bool StBbcGeom::IsInTile(short tileId, double x, double y){
  double PolygonX[7];
  double PolygonY[7];
  GetCorners(tileId,PolygonX,PolygonY);
  PolygonX[6]=PolygonX[0];   // must close the polygon
  PolygonY[6]=PolygonY[0];   // must close the polygon
  return TMath::IsInside(x,y,7,PolygonX,PolygonY);
}

