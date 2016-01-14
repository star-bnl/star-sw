#include "AgPosition.h"

#include "TString.h"
#include "TGeoMatrix.h"

//________________________________________________________________________________________________
AgPosition::AgPosition() :
  AgTransform(),
  AgParameterList(),
  mOrderOps(kRotTran),
  mX(0),
  mY(0),
  mZ(0)
{

};

//________________________________________________________________________________________________
bool AgPosition::Alternate( const double tr[4][4], const char* opts )
{
  // TODO: implement inverse transformation
  Reset();
  Matrix( tr );
  return true;
};
//________________________________________________________________________________________________
bool AgPosition::Misalign( const double tr[4][4], const char* opts )
{
  // TODO: implement inverse transformation
  Matrix( tr );
  return true;
}
//________________________________________________________________________________________________
TGeoMatrix *AgPosition::rotation()
{
  TGeoRotation *rot = new TGeoRotation();
  TString name  = Form("rot_%s_in_%s",_Block.c_str(),_Mother.c_str());
  TString title = _Table;
  rot->SetName(name);
  rot->SetTitle(title);
  double rotm[3][3];
  for ( int row=0;row<3;row++ )
  for ( int col=0;col<3;col++ )
    rotm[row][col] = mMatrix[row][col];
  rot->SetMatrix((double*)rotm);


  return rot;
};
//________________________________________________________________________________________________
TGeoMatrix *AgPosition::translation()
{

  TGeoTranslation *tran = new TGeoTranslation();
  TString name  = Form("pos_%s_in_%s",_Block.c_str(),_Mother.c_str());
  TString title = _Table;
  tran->SetName(name);
  tran->SetTitle(title);
  if ( kGeneral == mOrderOps ) {
    double x[3];
    for ( int row=0;row<3;row++ )
      x[row] = mMatrix[row][3];
    tran->SetTranslation(x[0],x[1],x[2]);
  }
  else {
    tran->SetTranslation(mX,mY,mZ);
  }
  return tran;
};
//________________________________________________________________________________________________
TGeoMatrix *AgPosition::matrix()
{
  TGeoRotation    *R = (TGeoRotation *)rotation();
  TGeoTranslation *T = (TGeoTranslation *)translation();
  TGeoCombiTrans *com = new TGeoCombiTrans( *T, *R );
  return com;
};
//________________________________________________________________________________________________
void AgPosition::Translate( double x, double y, double z ) {
  // Perform general transformation
  if ( kGeneral == mOrderOps ) {
    AgTransform::Translate( x, y, z );
    return;
  }
  // Seperate translation from rotation
  if ( kRotTran == mOrderOps ||
       kTranRot == mOrderOps ) {
    mX+=x;
    mY+=y;
    mZ+=z;
  }
    

};
