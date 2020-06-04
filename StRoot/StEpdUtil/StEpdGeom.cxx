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

ClassImp(StEpdGeom)


StEpdGeom::StEpdGeom() : mPP(0), mTT(0), mEW(0){
  if ( gRandom ) {
    pRan = (TRandom3*)gRandom;
  }
  else {
    pRan = new TRandom3();
    pRan -> SetSeed();
  }
  InitializeGeometry();
}

StEpdGeom::~StEpdGeom(){
  if ( pRan && pRan != gRandom ) delete pRan;
}

void StEpdGeom::InitializeGeometry(){

  //  First, save the phi values of the centers of all tiles.
  //  I am aware that this is a bit wasteful, since all the even (or odd) numbered
  //  tiles of a given position all have the same phi.  But you'll go nuts otherwise.
  //  Better to waste a little memory than to get confused.
  double DeltaPhiSS = 30.0*TMath::Pi()/180.0;  // 30 degree supersectors
  int EW=0;   // East
  for (int PP=1; PP<13; PP++){
    double phiSS = TMath::Pi()/2.0 - (PP-0.5)*DeltaPhiSS;
    if (phiSS<0.0) phiSS += 2.0*TMath::Pi();
    mPhiCenter[PP-1][0][EW] = phiSS;
    for (int TT=2; TT<32; TT+=2){      // EVENS
      mPhiCenter[PP-1][TT-1][EW] = phiSS - DeltaPhiSS/4.0;
    }
    for (int TT=3; TT<32; TT+=2){      // ODDS
      mPhiCenter[PP-1][TT-1][EW] = phiSS + DeltaPhiSS/4.0;
    }
  }
  EW=1;   // West
  for (int PP=1; PP<13; PP++){
    double phiSS = TMath::Pi()/2.0 + (PP-0.5)*DeltaPhiSS;
    if (phiSS>2.0*TMath::Pi()) phiSS -= 2.0*TMath::Pi();
    mPhiCenter[PP-1][0][EW] = phiSS;
    for (int TT=2; TT<32; TT+=2){      // EVENS
      mPhiCenter[PP-1][TT-1][EW] = phiSS + DeltaPhiSS/4.0;
    }
    for (int TT=3; TT<32; TT+=2){      // ODDS
      mPhiCenter[PP-1][TT-1][EW] = phiSS - DeltaPhiSS/4.0;
    }
  }

  //  Now the inner, outer, and average radius of a _ROW_
  double RowHeight[16]={4.4, 4.4, 4.4, 5.53, 5.53, 5.53,
			  5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53};
  double Rminimum = 4.6;  // the distance from beamline to the inner edge of tile 1
  mRmin[0] = Rminimum;  // row 1 (tiles 1)
  for (int irow=1; irow<16; irow++){
    mRmin[irow]=mRmin[irow-1]+RowHeight[irow-1];
    mRmax[irow-1]=mRmin[irow];
  }
  mRmax[15]=mRmin[15]+RowHeight[15];
  for (int irow=0; irow<16; irow++){
    mRave[irow] = 0.5*(mRmin[irow]+mRmax[irow]);
  }
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

// double StEpdGeom::GetPhiCenter(){
//   double phiCenter;
//   double DeltaPhiSS = 30.0*TMath::Pi()/180.0;  // 30 degree supersectors
//   if (mEW<0){    // east
//     double phiSS = TMath::Pi()/2.0 - (mPP-0.5)*DeltaPhiSS;
//     if (phiSS<0.0) phiSS += 2.0*TMath::Pi();
//     if (1 == mTT){phiCenter = phiSS;}  // tile 1
//     else{
//       if (0 == mTT%2){phiCenter = phiSS - DeltaPhiSS/4.0;}
//       else           {phiCenter = phiSS + DeltaPhiSS/4.0;}
//     }
//   }
//   else{  // west
//     double phiSS = TMath::Pi()/2.0 + (mPP-0.5)*DeltaPhiSS;
//     if (phiSS>2.0*TMath::Pi()) phiSS -= 2.0*TMath::Pi();
//     if (1==mTT){phiCenter = phiSS;}  // tile 1
//     else{
//       if (0 == mTT%2){phiCenter = phiSS + DeltaPhiSS/4.0;}
//       else          {phiCenter = phiSS - DeltaPhiSS/4.0;}
//     }
//   }
//   return phiCenter;
// }

// void StEpdGeom::GetRminRmax(double *Rmin, double *Rmax){
//   double RowHeight[16]={4.4, 4.4, 4.4, 5.53, 5.53, 5.53,
// 			  5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53, 5.53};
//   double Rminimum = 4.6;  // the distance from beamline to the inner edge of tile 1
//   double Rlimit[17];
//   Rlimit[0] = Rminimum;
//   for (int iRow=0; iRow<16; iRow++){
//     Rlimit[iRow+1] = Rlimit[iRow] + RowHeight[iRow];
//   }
//   int Row = this->Row();
//   *Rmin = Rlimit[Row-1];
//   *Rmax = Rlimit[Row];
// }

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
  //  double Rmin,Rmax;
  //  GetRminRmax(&Rmin,&Rmax);
  double ZZ = this->GetZwheel();
  TVector3 cent(mRave[this->Row()-1],0.0,ZZ);
  //  cent.SetXYZ(0.5*(Rmin+Rmax),0.0,ZZ);
  //  cent.RotateZ(this->GetPhiCenter());
  int ew = (mEW>0)?1:0;
  cent.RotateZ(mPhiCenter[mPP-1][mTT-1][ew]);
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
  short RR=this->Row();
  double Rmin = mRmin[RR-1];
  double Rmax = mRmax[RR-1];
  //  GetRminRmax(&Rmin,&Rmax);
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
  //  Point.RotateZ(this->GetPhiCenter());
  int ew = (mEW>0)?1:0;
  Point.RotateZ(mPhiCenter[mPP-1][mTT-1][ew]);


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
  //  double Rmin,Rmax;
  double Rmin=mRmin[RR-1];
  double Rmax=mRmax[RR-1];
  //  GetRminRmax(&Rmin,&Rmax);
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
  //  double phi = this->GetPhiCenter();
  int ew=(mEW>0)?1:0;
  double phi = mPhiCenter[mPP-1][mTT-1][ew];
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


//-------------------------------------------------------------------

void StEpdGeom::GetOverlappingBbcTiles(short uniqueID,
				       int* nOverlappingBbcTiles, short* BbcTileIDs){
  SetPpTtEw(uniqueID);
  GetOverlappingBbcTiles(nOverlappingBbcTiles,BbcTileIDs);
}

void StEpdGeom::GetOverlappingBbcTiles(short position, short tilenumber, short eastwest,
				       int* nOverlappingBbcTiles, short* BbcTileIDs){
  mPP = position;
  mTT = tilenumber;
  mEW = eastwest;
  GetOverlappingBbcTiles(nOverlappingBbcTiles,BbcTileIDs);
}
void StEpdGeom::GetOverlappingBbcTiles(int* nOverlappingBbcTiles, short* BbcTileIDs){
  if (mTT>9){                // only tiles 1-9 overlap with inner BBC
    nOverlappingBbcTiles=0;
    return;
  }
  if (mEW<=0){   // East
    *nOverlappingBbcTiles = mEastNumberOfOverlappingBbcTiles[mPP-1][mTT-1];
    for (int i=0; i<*nOverlappingBbcTiles; i++){
      BbcTileIDs[i] = mEastBbcTilesWhichOverlap[mPP-1][mTT-1][i];
    }
    for (int i=*nOverlappingBbcTiles; i<10; i++){
      BbcTileIDs[i] = 0;
    }
  }
  else{   // West
    *nOverlappingBbcTiles = mWestNumberOfOverlappingBbcTiles[mPP-1][mTT-1];
    for (int i=0; i<*nOverlappingBbcTiles; i++){
      BbcTileIDs[i] = mWestBbcTilesWhichOverlap[mPP-1][mTT-1][i];
    }
    for (int i=*nOverlappingBbcTiles; i<10; i++){
      BbcTileIDs[i] = 0;
    }
  }

}



// EastNumberOfOverlappingBbcTiles[PP-1][TT-1]; gives the number of BBC tiles that overlap
short StEpdGeom::mEastNumberOfOverlappingBbcTiles[12][9] = {
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 1
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 2
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 3
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 4
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 5
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 6
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 7
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 8
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 9
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 10
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 11
  { 1, 1, 2, 3, 1, 2, 2, 1, 0}    // PP= 12
};

// EastBbcTilesWhichOverlap[PP-1][TT-1][j] gives the BBC tile ID of the jth overlapping BBC tile
short StEpdGeom::mEastBbcTilesWhichOverlap[12][9][3] = {
  { { -1, 0, 0}, // PP = 1 TT = 1
    { -1, -7, 0}, // PP = 1 TT = 2
    { -1, 0, 0}, // PP = 1 TT = 3
    { -7, 0, 0}, // PP = 1 TT = 4
    { -1, -7, -8}, // PP = 1 TT = 5
    { -7, -8, 0}, // PP = 1 TT = 6
    { -7, -8, 0}, // PP = 1 TT = 7
    { 0, 0, 0}, // PP = 1 TT = 8
    { -8, 0, 0} // PP = 1 TT = 9
  },
  { { -6, 0, 0}, // PP = 2 TT = 1
    { -6, 0, 0}, // PP = 2 TT = 2
    { -6, -7, 0}, // PP = 2 TT = 3
    { -6, -7, -18}, // PP = 2 TT = 4
    { -7, 0, 0}, // PP = 2 TT = 5
    { -7, -18, 0}, // PP = 2 TT = 6
    { -7, -18, 0}, // PP = 2 TT = 7
    { -18, 0, 0}, // PP = 2 TT = 8
    { 0, 0, 0} // PP = 2 TT = 9
  },
  { { -6, 0, 0}, // PP = 3 TT = 1
    { -6, -17, 0}, // PP = 3 TT = 2
    { -6, 0, 0}, // PP = 3 TT = 3
    { -17, 0, 0}, // PP = 3 TT = 4
    { -6, -17, -18}, // PP = 3 TT = 5
    { -17, -18, 0}, // PP = 3 TT = 6
    { -17, -18, 0}, // PP = 3 TT = 7
    { 0, 0, 0}, // PP = 3 TT = 8
    { -18, 0, 0} // PP = 3 TT = 9
  },
  { { -5, 0, 0}, // PP = 4 TT = 1
    { -5, 0, 0}, // PP = 4 TT = 2
    { -5, -17, 0}, // PP = 4 TT = 3
    { -5, -16, -17}, // PP = 4 TT = 4
    { -17, 0, 0}, // PP = 4 TT = 5
    { -16, -17, 0}, // PP = 4 TT = 6
    { -16, -17, 0}, // PP = 4 TT = 7
    { -16, 0, 0}, // PP = 4 TT = 8
    { 0, 0, 0} // PP = 4 TT = 9
  },
  { { -5, 0, 0}, // PP = 5 TT = 1
    { -5, -15, 0}, // PP = 5 TT = 2
    { -5, 0, 0}, // PP = 5 TT = 3
    { -15, 0, 0}, // PP = 5 TT = 4
    { -5, -15, -16}, // PP = 5 TT = 5
    { -15, -16, 0}, // PP = 5 TT = 6
    { -15, -16, 0}, // PP = 5 TT = 7
    { 0, 0, 0}, // PP = 5 TT = 8
    { -16, 0, 0} // PP = 5 TT = 9
  },
  { { -4, 0, 0}, // PP = 6 TT = 1
    { -4, 0, 0}, // PP = 6 TT = 2
    { -4, -15, 0}, // PP = 6 TT = 3
    { -4, -14, -15}, // PP = 6 TT = 4
    { -15, 0, 0}, // PP = 6 TT = 5
    { -14, -15, 0}, // PP = 6 TT = 6
    { -14, -15, 0}, // PP = 6 TT = 7
    { -14, 0, 0}, // PP = 6 TT = 8
    { 0, 0, 0} // PP = 6 TT = 9
  },
  { { -4, 0, 0}, // PP = 7 TT = 1
    { -4, -13, 0}, // PP = 7 TT = 2
    { -4, 0, 0}, // PP = 7 TT = 3
    { -13, 0, 0}, // PP = 7 TT = 4
    { -4, -13, -14}, // PP = 7 TT = 5
    { -13, -14, 0}, // PP = 7 TT = 6
    { -13, -14, 0}, // PP = 7 TT = 7
    { 0, 0, 0}, // PP = 7 TT = 8
    { -14, 0, 0} // PP = 7 TT = 9
  },
  { { -3, 0, 0}, // PP = 8 TT = 1
    { -3, 0, 0}, // PP = 8 TT = 2
    { -3, -13, 0}, // PP = 8 TT = 3
    { -3, -12, -13}, // PP = 8 TT = 4
    { -13, 0, 0}, // PP = 8 TT = 5
    { -12, -13, 0}, // PP = 8 TT = 6
    { -12, -13, 0}, // PP = 8 TT = 7
    { -12, 0, 0}, // PP = 8 TT = 8
    { 0, 0, 0} // PP = 8 TT = 9
  },
  { { -3, 0, 0}, // PP = 9 TT = 1
    { -3, -11, 0}, // PP = 9 TT = 2
    { -3, 0, 0}, // PP = 9 TT = 3
    { -11, 0, 0}, // PP = 9 TT = 4
    { -3, -11, -12}, // PP = 9 TT = 5
    { -11, -12, 0}, // PP = 9 TT = 6
    { -11, -12, 0}, // PP = 9 TT = 7
    { 0, 0, 0}, // PP = 9 TT = 8
    { -12, 0, 0} // PP = 9 TT = 9
  },
  { { -2, 0, 0}, // PP = 10 TT = 1
    { -2, 0, 0}, // PP = 10 TT = 2
    { -2, -11, 0}, // PP = 10 TT = 3
    { -2, -10, -11}, // PP = 10 TT = 4
    { -11, 0, 0}, // PP = 10 TT = 5
    { -10, -11, 0}, // PP = 10 TT = 6
    { -10, -11, 0}, // PP = 10 TT = 7
    { -10, 0, 0}, // PP = 10 TT = 8
    { 0, 0, 0} // PP = 10 TT = 9
  },
  { { -2, 0, 0}, // PP = 11 TT = 1
    { -2, -9, 0}, // PP = 11 TT = 2
    { -2, 0, 0}, // PP = 11 TT = 3
    { -9, 0, 0}, // PP = 11 TT = 4
    { -2, -9, -10}, // PP = 11 TT = 5
    { -9, -10, 0}, // PP = 11 TT = 6
    { -9, -10, 0}, // PP = 11 TT = 7
    { 0, 0, 0}, // PP = 11 TT = 8
    { -10, 0, 0} // PP = 11 TT = 9
  },
  { { -1, 0, 0}, // PP = 12 TT = 1
    { -1, 0, 0}, // PP = 12 TT = 2
    { -1, -9, 0}, // PP = 12 TT = 3
    { -1, -8, -9}, // PP = 12 TT = 4
    { -9, 0, 0}, // PP = 12 TT = 5
    { -8, -9, 0}, // PP = 12 TT = 6
    { -8, -9, 0}, // PP = 12 TT = 7
    { -8, 0, 0}, // PP = 12 TT = 8
    { 0, 0, 0} // PP = 12 TT = 9
  }
};

// WestNumberOfOverlappingBbcTiles[PP-1][TT-1]; gives the number of BBC tiles that overlap
short StEpdGeom::mWestNumberOfOverlappingBbcTiles[12][9] = {
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 1
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 2
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 3
  { 1, 1, 2, 3, 1, 1, 2, 1, 0},   // PP= 4
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 5
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 6
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 7
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 8
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 9
  { 1, 1, 2, 3, 1, 2, 2, 1, 0},   // PP= 10
  { 1, 2, 1, 1, 3, 2, 2, 0, 1},   // PP= 11
  { 1, 1, 2, 3, 1, 2, 2, 1, 0}    // PP= 12 
};

// WestBbcTilesWhichOverlap[PP-1][TT-1][j] gives the BBC tile ID of the jth overlapping BBC tile
short StEpdGeom::mWestBbcTilesWhichOverlap[12][9][3] = {
  { { 1, 0, 0}, // PP = 1 TT = 1
    { 1, 7, 0}, // PP = 1 TT = 2
    { 1, 0, 0}, // PP = 1 TT = 3
    { 7, 0, 0}, // PP = 1 TT = 4
    { 1, 7, 8}, // PP = 1 TT = 5
    { 7, 8, 0}, // PP = 1 TT = 6
    { 7, 8, 0}, // PP = 1 TT = 7
    { 0, 0, 0}, // PP = 1 TT = 8
    { 8, 0, 0} // PP = 1 TT = 9
  },
  { { 6, 0, 0}, // PP = 2 TT = 1
    { 6, 0, 0}, // PP = 2 TT = 2
    { 6, 7, 0}, // PP = 2 TT = 3
    { 6, 7, 18}, // PP = 2 TT = 4
    { 7, 0, 0}, // PP = 2 TT = 5
    { 7, 18, 0}, // PP = 2 TT = 6
    { 7, 18, 0}, // PP = 2 TT = 7
    { 18, 0, 0}, // PP = 2 TT = 8
    { 0, 0, 0} // PP = 2 TT = 9
  },
  { { 6, 0, 0}, // PP = 3 TT = 1
    { 6, 17, 0}, // PP = 3 TT = 2
    { 6, 0, 0}, // PP = 3 TT = 3
    { 17, 0, 0}, // PP = 3 TT = 4
    { 6, 17, 18}, // PP = 3 TT = 5
    { 17, 18, 0}, // PP = 3 TT = 6
    { 17, 18, 0}, // PP = 3 TT = 7
    { 0, 0, 0}, // PP = 3 TT = 8
    { 18, 0, 0} // PP = 3 TT = 9
  },
  { { 5, 0, 0}, // PP = 4 TT = 1
    { 5, 0, 0}, // PP = 4 TT = 2
    { 5, 17, 0}, // PP = 4 TT = 3
    { 5, 16, 17}, // PP = 4 TT = 4
    { 17, 0, 0}, // PP = 4 TT = 5
    { 16, 0, 0}, // PP = 4 TT = 6
    { 16, 17, 0}, // PP = 4 TT = 7
    { 16, 0, 0}, // PP = 4 TT = 8
    { 0, 0, 0} // PP = 4 TT = 9
  },
  { { 5, 0, 0}, // PP = 5 TT = 1
    { 5, 15, 0}, // PP = 5 TT = 2
    { 5, 0, 0}, // PP = 5 TT = 3
    { 15, 0, 0}, // PP = 5 TT = 4
    { 5, 15, 16}, // PP = 5 TT = 5
    { 15, 16, 0}, // PP = 5 TT = 6
    { 15, 16, 0}, // PP = 5 TT = 7
    { 0, 0, 0}, // PP = 5 TT = 8
    { 16, 0, 0} // PP = 5 TT = 9
  },
  { { 4, 0, 0}, // PP = 6 TT = 1
    { 4, 0, 0}, // PP = 6 TT = 2
    { 4, 15, 0}, // PP = 6 TT = 3
    { 4, 14, 15}, // PP = 6 TT = 4
    { 15, 0, 0}, // PP = 6 TT = 5
    { 14, 15, 0}, // PP = 6 TT = 6
    { 14, 15, 0}, // PP = 6 TT = 7
    { 14, 0, 0}, // PP = 6 TT = 8
    { 0, 0, 0} // PP = 6 TT = 9
  },
  { { 4, 0, 0}, // PP = 7 TT = 1
    { 4, 13, 0}, // PP = 7 TT = 2
    { 4, 0, 0}, // PP = 7 TT = 3
    { 13, 0, 0}, // PP = 7 TT = 4
    { 4, 13, 14}, // PP = 7 TT = 5
    { 13, 14, 0}, // PP = 7 TT = 6
    { 13, 14, 0}, // PP = 7 TT = 7
    { 0, 0, 0}, // PP = 7 TT = 8
    { 14, 0, 0} // PP = 7 TT = 9
  },
  { { 3, 0, 0}, // PP = 8 TT = 1
    { 3, 0, 0}, // PP = 8 TT = 2
    { 3, 13, 0}, // PP = 8 TT = 3
    { 3, 12, 13}, // PP = 8 TT = 4
    { 13, 0, 0}, // PP = 8 TT = 5
    { 12, 13, 0}, // PP = 8 TT = 6
    { 12, 13, 0}, // PP = 8 TT = 7
    { 12, 0, 0}, // PP = 8 TT = 8
    { 0, 0, 0} // PP = 8 TT = 9
  },
  { { 3, 0, 0}, // PP = 9 TT = 1
    { 3, 11, 0}, // PP = 9 TT = 2
    { 3, 0, 0}, // PP = 9 TT = 3
    { 11, 0, 0}, // PP = 9 TT = 4
    { 3, 11, 12}, // PP = 9 TT = 5
    { 11, 12, 0}, // PP = 9 TT = 6
    { 11, 12, 0}, // PP = 9 TT = 7
    { 0, 0, 0}, // PP = 9 TT = 8
    { 12, 0, 0} // PP = 9 TT = 9
  },
  { { 2, 0, 0}, // PP = 10 TT = 1
    { 2, 0, 0}, // PP = 10 TT = 2
    { 2, 11, 0}, // PP = 10 TT = 3
    { 2, 10, 11}, // PP = 10 TT = 4
    { 11, 0, 0}, // PP = 10 TT = 5
    { 10, 11, 0}, // PP = 10 TT = 6
    { 10, 11, 0}, // PP = 10 TT = 7
    { 10, 0, 0}, // PP = 10 TT = 8
    { 0, 0, 0} // PP = 10 TT = 9
  },
  { { 2, 0, 0}, // PP = 11 TT = 1
    { 2, 9, 0}, // PP = 11 TT = 2
    { 2, 0, 0}, // PP = 11 TT = 3
    { 9, 0, 0}, // PP = 11 TT = 4
    { 2, 9, 10}, // PP = 11 TT = 5
    { 9, 10, 0}, // PP = 11 TT = 6
    { 9, 10, 0}, // PP = 11 TT = 7
    { 0, 0, 0}, // PP = 11 TT = 8
    { 10, 0, 0} // PP = 11 TT = 9
  },
  { { 1, 0, 0}, // PP = 12 TT = 1
    { 1, 0, 0}, // PP = 12 TT = 2
    { 1, 9, 0}, // PP = 12 TT = 3
    { 1, 8, 9}, // PP = 12 TT = 4
    { 9, 0, 0}, // PP = 12 TT = 5
    { 8, 9, 0}, // PP = 12 TT = 6
    { 8, 9, 0}, // PP = 12 TT = 7
    { 8, 0, 0}, // PP = 12 TT = 8
    { 0, 0, 0} // PP = 12 TT = 9
  }
};




