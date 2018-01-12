/*************************************
 * \author Mike Lisa
 * \date 4 Jan 2018
 *
 * \description:
 *  class defining geometrical aspects of an EPD Tile
 *  (position of center, RandomPointOnTile(), etc.)
 *
 * The user may pass the PP/TT/EW _or_ the uniqueID to
 *   most functions.  No option to pass StEpdHit object,
 *   because we want to avoid StObject-dependence.
 *************************************/

#include "StEpdGeom.h"
#include "TRandom.h"
#include "TRandom3.h"
#include "TMath.h"

StEpdGeom::StEpdGeom() : mPP(0), mTT(0), mEW(0){
  if ( gRandom ) {
    pRan = (TRandom3*)gRandom;
  }
  else {
    pRan = new TRandom3();
    pRan -> SetSeed();
  }
}

StEpdGeom::~StEpdGeom(){
  if ( pRan && pRan != gRandom ) delete pRan;
}

void StEpdGeom::SetPpTtEw(short uniqueID){
  mPP = std::abs(uniqueID/100);
  mTT = std::abs(uniqueID % 100);
  mEW = (uniqueID>0)?+1:-1;
}

double StEpdGeom::GetZwheel(){
  const double z_EPD = 375.0;  // EPD is 375 cm away from center of TPC in z-direction
  return z_EPD*mEW;
}

double StEpdGeom::GetPhiCenter(){
  double phiCenter;
  double DeltaPhiSS = 30.0*TMath::Pi()/180.0;  // 30 degree supersectors
  if (mEW<0){    // east
    double phiSS = TMath::Pi()/2.0 - (mPP-0.5)*DeltaPhiSS;
    if (phiSS<0.0) phiSS += 2.0*TMath::Pi();
    if (1 == mTT){phiCenter = phiSS;}  // tile 1
    else{
      if (0 == mTT%2){phiCenter = phiSS - DeltaPhiSS/4.0;}
      else           {phiCenter = phiSS + DeltaPhiSS/4.0;}
    }
  }
  else{  // west
    double phiSS = TMath::Pi()/2.0 + (mPP-0.5)*DeltaPhiSS;
    if (phiSS>2.0*TMath::Pi()) phiSS -= 2.0*TMath::Pi();
    if (1==mTT){phiCenter = phiSS;}  // tile 1
    else{
      if (0 == mTT%2){phiCenter = phiSS + DeltaPhiSS/4.0;}
      else          {phiCenter = phiSS - DeltaPhiSS/4.0;}
    }
  }
  return phiCenter;
}

void StEpdGeom::GetRminRmax(double *Rmin, double *Rmax){
  double RowHeight[16]={4.4, 4.4, 4.4, 5.53, 5.53, 5.53,
			  5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53};
  double Rminimum = 4.6;  // the distance from beamline to the inner edge of tile 1
  double Rlimit[17];
  Rlimit[0] = Rminimum;
  for (int iRow=0; iRow<16; iRow++){
    Rlimit[iRow+1] = Rlimit[iRow] + RowHeight[iRow];
  }
  int Row = this->Row();
  *Rmin = Rlimit[Row-1];
  *Rmax = Rlimit[Row];
}

//------------------------------------------------
short StEpdGeom::Row(short uniqueID){
  SetPpTtEw(uniqueID);
  return this->Row();
}
short StEpdGeom::Row(short PP, short TT, short EW){
  mPP = PP;
  mTT = TT;
  mEW = EW;
  return this->Row();
}
short StEpdGeom::Row(){
  return mTT/2 + 1;
}

//------------------------------------------------
TVector3 StEpdGeom::TileCenter(short uniqueID){
  SetPpTtEw(uniqueID);
  return this->TileCenter();
}
TVector3 StEpdGeom::TileCenter(short PP, short TT, short EW){
  mPP = PP;  
  mTT = TT;  
  mEW = EW;  
  return this->TileCenter();
}
TVector3 StEpdGeom::TileCenter(){
  double Rmin,Rmax;
  GetRminRmax(&Rmin,&Rmax);
  double ZZ = this->GetZwheel();
  TVector3 cent;
  cent.SetXYZ(0.5*(Rmin+Rmax),0.0,ZZ);
  cent.RotateZ(this->GetPhiCenter());
  return cent;
}

//-----------------------------------------------------
TVector3 StEpdGeom::RandomPointOnTile(short uniqueID){
  SetPpTtEw(uniqueID);
  return this->RandomPointOnTile();
}
TVector3 StEpdGeom::RandomPointOnTile(short PP, short TT, short EW){
  mPP = PP;  
  mTT = TT;  
  mEW = EW;  
  return this->RandomPointOnTile();
}
TVector3 StEpdGeom::RandomPointOnTile(){

  double GapWidth = 0.08;  // one half of the glue gap width
  double Aparam = 2.0*tan(7.5*TMath::Pi()/180.0);
  double Bparam = -2.0*GapWidth;

  double ZZ = this->GetZwheel();
  double Rmin,Rmax;
  GetRminRmax(&Rmin,&Rmax);
  short RR=this->Row();
  double Xmin = Rmin + GapWidth;
  double Xmax = Rmax - GapWidth;
  if (1==RR) Xmin -= 2.0*GapWidth;  // no glue on the "inside" of tile 1
  if (16==RR) Xmax += GapWidth; // no glue on "outside" of TT30,31

  // the reason for this next command is that Tile 01 is a pain in the neck.
  // I didn't figure out an easy way to get a random point inside the pentagon,
  // so I make the outer radius a little too big.  Then I get a point, and if
  // it doesn't fit in the tile, I try again until it's finally there.
  if (1==RR) Xmax += GapWidth;

  double A = Aparam;
  if (1==RR) A*= 2.0;

  double gamma = 0.5*A*pow(Xmin,2)+Bparam*Xmin;
  double alpha = 0.5*A*pow(Xmax,2)+Bparam*Xmax-gamma;

  double q = pRan->Rndm();
  double XX = (sqrt(pow(Bparam,2)+2.0*A*(alpha*q+gamma)) - Bparam)/A;
  q = pRan->Rndm();
  double DeltaY = A*XX+Bparam;
  double YY = (q-0.5)*DeltaY;

  TVector3 Point(XX,YY,ZZ);
  Point.RotateZ(this->GetPhiCenter());

  // if this is Tile 01, there's the possibility that the point does
  // not fit into the tile after all, so check and if it doesn't
  // then try again.
  // Recursion ==  Awesomeness.
  if (1==RR){
    if (!(this->IsInTile(Point.X(),Point.Y()))) return this->RandomPointOnTile();}

  return Point;
}

//----------------------------------------------------
void StEpdGeom::GetCorners(short uniqueID,
			   int* nCorners, double* x, double* y){
  SetPpTtEw(uniqueID);
  GetCorners(nCorners,x,y);
}
void StEpdGeom::GetCorners(short position, short tilenumber, short eastwest,
		int* nCorners, double* x, double* y){
  mPP = position;
  mTT = tilenumber;
  mEW = eastwest;
  GetCorners(nCorners,x,y);
}
void StEpdGeom::GetCorners(int* nCorners, double* xc, double* yc){
  double x[5];
  double y[5];
  // we provde the user five corners.  For tiles 2-31, the fifth "corner" is junk.
  // only tile 1 is a pentagon
  double OpeningAngle = 7.5*TMath::Pi()/180.0;
  double GapWidth = 0.08;  // gap between tiles / 2
  short RR = this->Row();
  double Rmin,Rmax;
  GetRminRmax(&Rmin,&Rmax);
  if (1==RR){
    *nCorners=5;
    double xtmp[3], ytmp[3];
    xtmp[0] = Rmin;  ytmp[0] = +Rmin*tan(OpeningAngle);
    xtmp[1] = Rmax;  ytmp[1] = +Rmax*tan(OpeningAngle);
    xtmp[2] = Rmax;  ytmp[2] = -Rmax*tan(OpeningAngle);
    for (int ic=0; ic<3; ic++){
      x[ic] =  xtmp[ic]*cos(OpeningAngle) - ytmp[ic]*sin(OpeningAngle);
      y[ic] = +xtmp[ic]*sin(OpeningAngle) + ytmp[ic]*cos(OpeningAngle);
    }
    y[0] -= GapWidth;
    y[1] -= GapWidth;
    x[1] -= GapWidth;
    x[2] -= GapWidth;
    x[3] = x[1];  y[3] = -y[1];
    x[4] = x[0];  y[4] = -y[0];
  }
  else{
    *nCorners=4;
    x[0] = Rmin + GapWidth;  y[0] = +Rmin*tan(OpeningAngle) - GapWidth;
    x[1] = Rmax - GapWidth;  y[1] = +Rmax*tan(OpeningAngle) - GapWidth;
    x[2] = Rmax - GapWidth;  y[2] = -Rmax*tan(OpeningAngle) + GapWidth;
    x[3] = Rmin + GapWidth;  y[3] = -Rmin*tan(OpeningAngle) + GapWidth;
    x[4] = -999;            y[4] = -999;    // unused for TT!=1

    if (16==RR){       // there is no glue "outside" TT30,31
      x[1] += GapWidth;
      x[2] += GapWidth;
    }
  }
  double phi = this->GetPhiCenter();
  for (int icorn=0; icorn<(*nCorners); icorn++){
    xc[icorn] = +x[icorn]*cos(phi) - y[icorn]*sin(phi);
    yc[icorn] = +x[icorn]*sin(phi) + y[icorn]*cos(phi);
  }
}

//---------------------------------------------------------------------  
bool StEpdGeom::IsInTile(short uniqueID,
			   double x, double y){
  SetPpTtEw(uniqueID);
  return this->IsInTile(x,y);
}
bool StEpdGeom::IsInTile(short position, short tilenumber, short eastwest,
			   double x, double y){
  mPP = position;
  mTT = tilenumber;
  mEW = eastwest;
  return this->IsInTile(x,y);
}
bool StEpdGeom::IsInTile(double x, double y){
  double PolygonX[6];
  double PolygonY[6];
  int numberOfCorners;
  this->GetCorners(&numberOfCorners,PolygonX,PolygonY);
  PolygonX[numberOfCorners] = PolygonX[0];     PolygonY[numberOfCorners] = PolygonY[0];
  return TMath::IsInside(x,y,numberOfCorners+1,PolygonX,PolygonY);  
}
