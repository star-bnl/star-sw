#include "StBbcGeom.h"
#include "TMath.h"
#include <iostream>

using namespace std;

ClassImp(StBbcGeom)

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

  //------------- fill member array of phototube numbers.
  //------------- I am too stupid to figure out a better way of doing this:

  for (int pmt=0; pmt<16; pmt++){
    mNtilesOfPmt[pmt] = 0;
    mTileNumbersOfPmt[pmt][0] = 0;
    mTileNumbersOfPmt[pmt][1] = 0;
  }

  // phototube number corresponding to a given inner BBC tile.  Same for east and west BBCs
  //    Tile#               1  2  3  4  5  6  7  8  9 10  11  12  12  14  15  16  17  18
  unsigned short pmt[18] = {1, 2, 3, 4, 5, 6, 7, 8, 7, 9, 10, 11, 12, 13, 12, 14, 15, 16};
  for (int itile=0; itile<18; itile++){
    mPMT[itile] = pmt[itile];
    // note ordering of the following two statements
    mTileNumbersOfPmt[pmt[itile]-1][mNtilesOfPmt[pmt[itile]-1]] = itile+1;
    mNtilesOfPmt[pmt[itile]-1]++;
  }
}
 
//-------------------------------------------------------
void StBbcGeom::GetTilesOfPmt(unsigned short pmtNumber, unsigned short *nTiles, unsigned short* tileNumbers){
  *nTiles = mNtilesOfPmt[pmtNumber-1];                   // will usually be 1.  Will be 2 for pmt #7 and #12
  tileNumbers[0] = mTileNumbersOfPmt[pmtNumber-1][0];
  tileNumbers[1] = mTileNumbersOfPmt[pmtNumber-1][1];    // this will usually be zero except for pmt #7 and #12
}
			      



//-------------------------------------------------------
TVector3 StBbcGeom::TileCenter(short tileId){
  //  short index = abs(tileId) - 1;
  //  if (tileId<0){return mEastTileCenter[index];}
  //  else {return mWestTileCenter[index];}
  unsigned short absValTile = abs(tileId);
  return (tileId<0)?TileCenter(absValTile,0):TileCenter(absValTile,1);
}
TVector3 StBbcGeom::TileCenter(unsigned short absValueTileNumber, short eastwest){
  return (eastwest>0)?mWestTileCenter[absValueTileNumber-1]:mEastTileCenter[absValueTileNumber-1];
}



//-------------------------------------------------------
void StBbcGeom::GetCorners(short tileId, double* x, double* y){
  unsigned short absValTile = abs(tileId);
  return (tileId<0)?GetCorners(absValTile,0,x,y):GetCorners(absValTile,1,x,y);
}
/*

  TVector3 cent = this->TileCenter(tileId);
  double xcent = cent.X();
  double ycent = cent.Y();
  for (int i=0; i<6; i++){
    x[i] = mX[i]+xcent;
    y[i] = mY[i]+ycent;
  }
}
*/
void StBbcGeom::GetCorners(unsigned short absValueTileNumber, short eastwest, double* x, double* y){
  TVector3 cent = this->TileCenter(absValueTileNumber,eastwest);
  double xcent = cent.X();
  double ycent = cent.Y();
  for (int i=0; i<6; i++){
    x[i] = mX[i]+xcent;
    y[i] = mY[i]+ycent;
  }
}

//-------------------------------------------------------
bool StBbcGeom::IsInTile(short tileId, double x, double y){
  unsigned short absValTile = abs(tileId);
  return (tileId<0)?IsInTile(absValTile,0,x,y):IsInTile(absValTile,1,x,y);
}
bool StBbcGeom::IsInTile(unsigned short absValueTileNumber, short eastwest, double x, double y){
  double PolygonX[7];
  double PolygonY[7];
  GetCorners(absValueTileNumber,eastwest,PolygonX,PolygonY);
  PolygonX[6]=PolygonX[0];   // must close the polygon
  PolygonY[6]=PolygonY[0];   // must close the polygon
  return TMath::IsInside(x,y,7,PolygonX,PolygonY);
}

//-------------------------------------------------------
void StBbcGeom::GetOverlappingEpdTiles(short tileId,
				       int* nOverlappingEpdTiles, short* EpdTileIDs){
  unsigned short absValTile = abs(tileId);
  if (tileId<0) {GetOverlappingEpdTiles(absValTile,-1,nOverlappingEpdTiles,EpdTileIDs);}
  else {GetOverlappingEpdTiles(absValTile,1,nOverlappingEpdTiles,EpdTileIDs);}
}
void StBbcGeom::GetOverlappingEpdTiles(unsigned short BB, short eastwest,
				       int* nOverlappingEpdTiles, short* EpdTileIDs){
  if ((BB==0)||(BB>18)){
    *nOverlappingEpdTiles=0;
    return;
  }
  if (eastwest<0){  // East
    *nOverlappingEpdTiles = mEastNumberOfOverlappingEpdTiles[BB-1];
    for (int i=0; i<*nOverlappingEpdTiles; i++){
      EpdTileIDs[i] = mEastEpdTilesWhichOverlap[BB-1][i];
    }
  }
  else{            // west
    *nOverlappingEpdTiles = mWestNumberOfOverlappingEpdTiles[BB-1];
    for (int i=0; i<*nOverlappingEpdTiles; i++){
      EpdTileIDs[i] = mWestEpdTilesWhichOverlap[BB-1][i];
    }
  }
  for (int i=*nOverlappingEpdTiles; i<10; i++){
    EpdTileIDs[i] = 0;
  }
}



//-------------------------------------------------------
// EastNumberOfOverlappingEpdTiles[BB-1]  gives the number of EPD tiles that have any overlap with BBC tile BB
short StBbcGeom::mEastNumberOfOverlappingEpdTiles[18] = { 8, 8, 8, 8, 8, 8, 10, 8, 10, 8, 10, 8, 9, 8, 10, 8, 10, 8};

// EastEpdTilesWhichOverlap[BB-1][i]  gives the EPD tile id of the jth EPD tile that overlaps BBC inner tile BB
short StBbcGeom::mEastEpdTilesWhichOverlap[18][10] = {
  { -101, -102, -103, -105, -1201, -1202, -1203, -1204, 0, 0},         // BBC tile #1
  { -1001, -1002, -1003, -1004, -1101, -1102, -1103, -1105, 0, 0},         // BBC tile #2
  { -801, -802, -803, -804, -901, -902, -903, -905, 0, 0},         // BBC tile #3
  { -601, -602, -603, -604, -701, -702, -703, -705, 0, 0},         // BBC tile #4
  { -401, -402, -403, -404, -501, -502, -503, -505, 0, 0},         // BBC tile #5
  { -201, -202, -203, -204, -301, -302, -303, -305, 0, 0},         // BBC tile #6
  { -102, -104, -105, -106, -107, -203, -204, -205, -206, -207},         // BBC tile #7
  { -105, -106, -107, -109, -1204, -1206, -1207, -1208, 0, 0},         // BBC tile #8
  { -1102, -1104, -1105, -1106, -1107, -1203, -1204, -1205, -1206, -1207},         // BBC tile #9
  { -1004, -1006, -1007, -1008, -1105, -1106, -1107, -1109, 0, 0},         // BBC tile #10
  { -902, -904, -905, -906, -907, -1003, -1004, -1005, -1006, -1007},         // BBC tile #11
  { -804, -806, -807, -808, -905, -906, -907, -909, 0, 0},         // BBC tile #12
  { -702, -704, -705, -706, -707, -803, -804, -805, -807, 0},         // BBC tile #13
  { -604, -606, -607, -608, -705, -706, -707, -709, 0, 0},         // BBC tile #14
  { -502, -504, -505, -506, -507, -603, -604, -605, -606, -607},         // BBC tile #15
  { -404, -406, -407, -408, -505, -506, -507, -509, 0, 0},         // BBC tile #16
  { -302, -304, -305, -306, -307, -403, -404, -405, -406, -407},         // BBC tile #17
  { -204, -206, -207, -208, -305, -306, -307, -309, 0, 0}         // BBC tile #18
};

// WestNumberOfOverlappingEpdTiles[BB-1]  gives the number of EPD tiles that have any overlap with BBC tile BB
short StBbcGeom::mWestNumberOfOverlappingEpdTiles[18] = { 8, 8, 8, 8, 8, 8, 10, 8, 10, 8, 10, 8, 10, 8, 10, 8, 10, 8};

// WestEpdTilesWhichOverlap[BB-1][i]  gives the EPD tile id of the jth EPD tile that overlaps BBC inner tile BB
short StBbcGeom::mWestEpdTilesWhichOverlap[18][10] = {
  { 101, 102, 103, 105, 1201, 1202, 1203, 1204, 0, 0},         // BBC tile #1
  { 1001, 1002, 1003, 1004, 1101, 1102, 1103, 1105, 0, 0},         // BBC tile #2
  { 801, 802, 803, 804, 901, 902, 903, 905, 0, 0},         // BBC tile #3
  { 601, 602, 603, 604, 701, 702, 703, 705, 0, 0},         // BBC tile #4
  { 401, 402, 403, 404, 501, 502, 503, 505, 0, 0},         // BBC tile #5
  { 201, 202, 203, 204, 301, 302, 303, 305, 0, 0},         // BBC tile #6
  { 102, 104, 105, 106, 107, 203, 204, 205, 206, 207},         // BBC tile #7
  { 105, 106, 107, 109, 1204, 1206, 1207, 1208, 0, 0},         // BBC tile #8
  { 1102, 1104, 1105, 1106, 1107, 1203, 1204, 1205, 1206, 1207},         // BBC tile #9
  { 1004, 1006, 1007, 1008, 1105, 1106, 1107, 1109, 0, 0},         // BBC tile #10
  { 902, 904, 905, 906, 907, 1003, 1004, 1005, 1006, 1007},         // BBC tile #11
  { 804, 806, 807, 808, 905, 906, 907, 909, 0, 0},         // BBC tile #12
  { 702, 704, 705, 706, 707, 803, 804, 805, 806, 807},         // BBC tile #13
  { 604, 606, 607, 608, 705, 706, 707, 709, 0, 0},         // BBC tile #14
  { 502, 504, 505, 506, 507, 603, 604, 605, 606, 607},         // BBC tile #15
  { 404, 406, 407, 408, 505, 506, 507, 509, 0, 0},         // BBC tile #16
  { 302, 304, 305, 306, 307, 403, 404, 405, 406, 407},         // BBC tile #17
  { 204, 206, 207, 208, 305, 306, 307, 309, 0, 0}         // BBC tile #18
};


