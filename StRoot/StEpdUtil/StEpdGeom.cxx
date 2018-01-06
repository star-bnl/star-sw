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
// #include "StEvent/StEpdHit.h"  no StObject dependence
#include "TRandom3.h"
#include "TMath.h"

ClassImp(StEpdGeom);

StEpdGeom::StEpdGeom() : mPP(0), mTT(0), mEW(0){
  pRan = new TRandom3;
  pRan->SetSeed();
}

StEpdGeom::~StEpdGeom(){
  delete pRan;
}

void StEpdGeom::SetPpTtEw(Short_t uniqueID){
  mPP = std::abs(uniqueID/100);
  mTT = std::abs(uniqueID % 100);
  mEW = (uniqueID>0)?+1:-1;
}

Double_t StEpdGeom::GetZwheel(){
  return 375.0*mEW;
}

Double_t StEpdGeom::GetPhiCenter(){
  Double_t phiCenter;
  Double_t DeltaPhiSS = 30.0*TMath::Pi()/180.0;  // 30 degree supersectors
  if (mEW<0){    // east
    Double_t phiSS = TMath::Pi()/2.0 - (mPP-0.5)*DeltaPhiSS;
    if (phiSS<0.0) phiSS += 2.0*TMath::Pi();
    if (mTT==1){phiCenter = phiSS;}  // tile 1
    else{
      if (mTT%2 == 0){phiCenter = phiSS - DeltaPhiSS/4.0;}
      else           {phiCenter = phiSS + DeltaPhiSS/4.0;}
    }
  }
  else{  // west
    Double_t phiSS = TMath::Pi()/2.0 + (mPP-0.5)*DeltaPhiSS;
    if (phiSS>2.0*TMath::Pi()) phiSS -= 2.0*TMath::Pi();
    if (mTT==1){phiCenter = phiSS;}  // tile 1
    else{
      if (mTT%2 == 0){phiCenter = phiSS + DeltaPhiSS/4.0;}
      else          {phiCenter = phiSS - DeltaPhiSS/4.0;}
    }
  }
  return phiCenter;
}

void StEpdGeom::GetRminRmax(Double_t *Rmin, Double_t *Rmax){
  Double_t RowHeight[16]={4.4, 4.4, 4.4, 5.53, 5.53, 5.53,
			  5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53};
  Double_t Rminimum = 4.6;  // the distance from beamline to the inner edge of tile 1
  Double_t Rlimit[17];
  Rlimit[0] = Rminimum;
  for (Int_t iRow=0; iRow<16; iRow++){
    Rlimit[iRow+1] = Rlimit[iRow] + RowHeight[iRow];
  }
  Int_t Row = this->Row();
  *Rmin = Rlimit[Row-1];
  *Rmax = Rlimit[Row];
}

//------------------------------------------------
Short_t StEpdGeom::Row(Short_t uniqueID){
  SetPpTtEw(uniqueID);
  return this->Row();
}
Short_t StEpdGeom::Row(Short_t PP, Short_t TT, Short_t EW){
  mPP = PP;
  mTT = TT;
  mEW = EW;
  return this->Row();
}
Short_t StEpdGeom::Row(){
  return mTT/2 + 1;
}

//------------------------------------------------
TVector3 StEpdGeom::TileCenter(Short_t uniqueID){
  SetPpTtEw(uniqueID);
  return this->TileCenter();
}
TVector3 StEpdGeom::TileCenter(Short_t PP, Short_t TT, Short_t EW){
  mPP = PP;  
  mTT = TT;  
  mEW = EW;  
  return this->TileCenter();
}
TVector3 StEpdGeom::TileCenter(){
  Double_t Rmin,Rmax;
  GetRminRmax(&Rmin,&Rmax);
  Double_t ZZ = this->GetZwheel();
  TVector3 cent;
  cent.SetXYZ(0.5*(Rmin+Rmax),0.0,ZZ);
  cent.RotateZ(this->GetPhiCenter());
  return cent;
}

//-----------------------------------------------------
TVector3 StEpdGeom::RandomPointOnTile(Short_t uniqueID){
  SetPpTtEw(uniqueID);
  return this->RandomPointOnTile();
}
TVector3 StEpdGeom::RandomPointOnTile(Short_t PP, Short_t TT, Short_t EW){
  mPP = PP;  
  mTT = TT;  
  mEW = EW;  
  return this->RandomPointOnTile();
}
TVector3 StEpdGeom::RandomPointOnTile(){

  Double_t GapWidth = 0.08;  // one half of the glue gap width
  Double_t Aparam = 2.0*tan(7.5*TMath::Pi()/180.0);
  Double_t Bparam = -2.0*GapWidth;

  Double_t ZZ = this->GetZwheel();
  Double_t Rmin,Rmax;
  GetRminRmax(&Rmin,&Rmax);
  Short_t RR=this->Row();
  Double_t Xmin = Rmin + GapWidth;
  Double_t Xmax = Rmax - GapWidth;
  if (RR==1) Xmin -= 2.0*GapWidth;  // no glue on the "inside" of tile 1
  if (RR==16) Xmax += GapWidth; // no glue on "outside" of TT30,31

  // the reason for this next command is that Tile 01 is a pain in the neck.
  // I didn't figure out an easy way to get a random point inside the pentagon,
  // so I make the outer radius a little too big.  Then I get a point, and if
  // it doesn't fit in the tile, I try again until it's finally there.
  if (RR==1) Xmax += GapWidth;

  Double_t A = Aparam;
  if (1==RR) A*= 2.0;

  Double_t gamma = 0.5*A*pow(Xmin,2)+Bparam*Xmin;
  Double_t alpha = 0.5*A*pow(Xmax,2)+Bparam*Xmax-gamma;

  Double_t q = pRan->Rndm();
  Double_t XX = (sqrt(pow(Bparam,2)+2.0*A*(alpha*q+gamma)) - Bparam)/A;
  q = pRan->Rndm();
  Double_t DeltaY = A*XX+Bparam;
  Double_t YY = (q-0.5)*DeltaY;

  TVector3 Point(XX,YY,ZZ);
  Point.RotateZ(this->GetPhiCenter());

  // if this is Tile 01, there's the possibility that the point does
  // not fit into the tile after all, so check and if it doesn't
  // then try again.
  // Recursion ==  Awesomeness.
  if (RR==1){
    if (!(this->IsInTile(Point.X(),Point.Y()))) return this->RandomPointOnTile();}

  return Point;
}

//----------------------------------------------------
void StEpdGeom::GetCorners(Short_t uniqueID,
			   Int_t* nCorners, Double_t* x, Double_t* y){
  SetPpTtEw(uniqueID);
  GetCorners(nCorners,x,y);
}
void StEpdGeom::GetCorners(Short_t position, Short_t tilenumber, Short_t eastwest,
		Int_t* nCorners, Double_t* x, Double_t* y){
  mPP = position;
  mTT = tilenumber;
  mEW = eastwest;
  GetCorners(nCorners,x,y);
}
void StEpdGeom::GetCorners(Int_t* nCorners, Double_t* xc, Double_t* yc){
  Double_t x[5];
  Double_t y[5];
  // we provde the user five corners.  For tiles 2-31, the fifth "corner" is junk.
  // only tile 1 is a pentagon
  Double_t OpeningAngle = 7.5*TMath::Pi()/180.0;
  Double_t GapWidth = 0.08;  // gap between tiles / 2
  Short_t RR = this->Row();
  Double_t Rmin,Rmax;
  GetRminRmax(&Rmin,&Rmax);
  if (1==RR){
    *nCorners=5;
    Double_t xtmp[3], ytmp[3];
    xtmp[0] = Rmin;  ytmp[0] = +Rmin*tan(OpeningAngle);
    xtmp[1] = Rmax;  ytmp[1] = +Rmax*tan(OpeningAngle);
    xtmp[2] = Rmax;  ytmp[2] = -Rmax*tan(OpeningAngle);
    for (Int_t ic=0; ic<3; ic++){
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

    if (RR==16){       // there is no glue "outside" TT30,31
      x[1] += GapWidth;
      x[2] += GapWidth;
    }
  }
  Double_t phi = this->GetPhiCenter();
  for (Int_t icorn=0; icorn<(*nCorners); icorn++){
    xc[icorn] = +x[icorn]*cos(phi) - y[icorn]*sin(phi);
    yc[icorn] = +x[icorn]*sin(phi) + y[icorn]*cos(phi);
  }
}

//---------------------------------------------------------------------  
Bool_t StEpdGeom::IsInTile(Short_t uniqueID,
			   Double_t x, Double_t y){
  SetPpTtEw(uniqueID);
  return this->IsInTile(x,y);
}
Bool_t StEpdGeom::IsInTile(Short_t position, Short_t tilenumber, Short_t eastwest,
			   Double_t x, Double_t y){
  mPP = position;
  mTT = tilenumber;
  mEW = eastwest;
  return this->IsInTile(x,y);
}
Bool_t StEpdGeom::IsInTile(Double_t x, Double_t y){
  Double_t PolygonX[6];
  Double_t PolygonY[6];
  Int_t numberOfCorners;
  this->GetCorners(&numberOfCorners,PolygonX,PolygonY);
  PolygonX[numberOfCorners] = PolygonX[0];     PolygonY[numberOfCorners] = PolygonY[0];
  return TMath::IsInside(x,y,numberOfCorners+1,PolygonX,PolygonY);  
}
