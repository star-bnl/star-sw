#include "AgPosition.h"

#include <TString.h>
#include <TGeoMatrix.h>
#include <TROOT.h>

#include <StChain.h>
#include <TTable.h>
#include <StDetectorDbMaker/St_SurveyC.h>
#include <StMessMgr.h>
#include <algorithm>
#include <iostream>

#include "StarVMC/StarAgmlLib/StarAgmlStacker.h"
#include "TGeoManager.h"

const char* orderOpsName[] = 
  { "unknown",
    "general",
    "rot+tran",
    "tran+rot" };


static int debuglvl = 0;
void AgPosition::SetDebug( int d ){ debuglvl = d; }

/// Helper function to retrive a DB table and extract the full translation, rotation matrix
//void GetMatrixFromDb( const char* table, const int row, double matrix[4][4] )
#define chain StMaker::GetChain()

void AgMLDbMatrix::operator()( const char* table, const int row, double matrix[4][4] )
{

  // FORtran land can insert spaces unexpectedly?  
  TString tablename = table;
  tablename.ReplaceAll(" ","");

  TTable *ttable = (TTable *)chain-> GetDataBase( tablename );
  if ( ttable )
    {

      if (1 <= debuglvl) LOG_INFO << "DB Matrix: " << tablename.Data() << " [" << row << "]" << endm;
      //  if (2 <= debug ) ttable->Print(row,row+1);

      Survey_st* t = (Survey_st*)ttable->At(row);
      matrix[0][0] = t->r00;
      matrix[0][1] = t->r01;
      matrix[0][2] = t->r02;
      matrix[0][3] = t->t0;
      matrix[1][0] = t->r10;
      matrix[1][1] = t->r11;
      matrix[1][2] = t->r12;
      matrix[1][3] = t->t1;
      matrix[2][0] = t->r20;
      matrix[2][1] = t->r21;
      matrix[2][2] = t->r22;
      matrix[2][3] = t->t2;
      matrix[3][0] = 0;
      matrix[3][1] = 0;
      matrix[3][2] = 0;
      matrix[3][3] = 1;
      
      if (2<=debuglvl) {

	LOG_INFO << Form( "[%9.5f %9.5f %9.5f | %9.5f]", matrix[0][0], matrix[0][1], matrix[0][2], matrix[0][3] ) << endm;
	LOG_INFO << Form( "[%9.5f %9.5f %9.5f | %9.5f]", matrix[1][0], matrix[1][1], matrix[1][2], matrix[1][3] ) << endm;
	LOG_INFO << Form( "[%9.5f %9.5f %9.5f | %9.5f]", matrix[2][0], matrix[2][1], matrix[2][2], matrix[2][3] ) << endm;
	LOG_INFO << Form( "[%9.5f %9.5f %9.5f | %9.5f]", matrix[3][0], matrix[3][1], matrix[3][2], matrix[3][3] ) << endm;

      }


    }
  else  // no DB table.  Warn and return 1.
    {
      LOG_WARN << "DB Table not found: " << table << " [" << row << "]" << endm;
      matrix[0][0] = 1;
      matrix[0][1] = 0;
      matrix[0][2] = 0;
      matrix[0][3] = 0;
      matrix[1][0] = 0;
      matrix[1][1] = 1;
      matrix[1][2] = 0;
      matrix[1][3] = 0;
      matrix[2][0] = 0;
      matrix[2][1] = 0;
      matrix[2][2] = 1;
      matrix[2][3] = 0;
      matrix[3][0] = 0;
      matrix[3][1] = 0;
      matrix[3][2] = 0;
      matrix[3][3] = 1;        
    }
  
};


void AgPosition::SetReal()
{
  LOG_INFO << "Setting misaligned geometry" << endm;
  AgMLDbFunctor *f =  new AgMLDbMatrix();
  AgMLDbFunctor::Register(f);
}
void AgPosition::SetIdeal()
{
  LOG_INFO << "Setting ideal geometry" << endm;
  AgMLDbFunctor *f = new AgMLDbFunctor();
  AgMLDbFunctor::Register(f);
}
  
//________________________________________________________________________________________________
AgPosition::AgPosition() :
  AgTransform(),
  AgParameterList(),
  mOrderOps(kRotTran),
  mX(0),
  mY(0),
  mZ(0),
  mRotationMatrix{1,0,0, 0,1,0, 0,0,1},
  mHasRotm(false)
{

};

//________________________________________________________________________________________________
// bool AgPosition::Alternate( const double tr[4][4], const char* opts )
// {
//   // TODO: implement inverse transformation
//   Reset();
//   Matrix( tr );
//   return true;
// };
// //________________________________________________________________________________________________
// bool AgPosition::Misalign( const double tr[4][4], const char* opts )
// {
//   // TODO: implement inverse transformation
//   Matrix( tr );
//   return true;
// }
//________________________________________________________________________________________________
TGeoMatrix *AgPosition::rotation()
{
  TGeoRotation *rot = new TGeoRotation();
  TString name  = Form("rot_%s_in_%s",_Block.c_str(),_Mother.c_str());
  TString title = _Table;
  rot->SetName(name);
  //rot->SetTitle(title);
  if ( mHasRotm )  {
    rot->SetMatrix( mRotationMatrix );      // use this instead
    return rot;
  }
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
  //  tran->SetTitle(title);
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


  // Initialize rotation and translation to the values specified by the geometry file
  TGeoRotation    *R = (TGeoRotation *)rotation();
  TGeoTranslation *T = (TGeoTranslation *)translation();
  //  R->SetTitle( "geometry file" );
  //  T->SetTitle( "geometry file" );

  TGeoCombiTrans  *C = new TGeoCombiTrans( *T, *R );
  C->SetTitle( R->GetTitle() );

  return C;
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
//________________________________________________________________________________________________
void AgPosition::SetOrder( OrderOps_t order )
{

  // Save the current state
  OrderOps_t older = OrderOps_t(mOrderOps);

  // No change in order of operations
  if ( older == order ) { mOrderOps = order; return; }

  // Switch from generalized 4x4 transformation to plain rotation
  if ( older == kGeneral )
    {
      mX = mMatrix[0][3];
      mY = mMatrix[1][3];
      mZ = mMatrix[2][3];      

      mMatrix[0][3] = 0;
      mMatrix[1][3] = 0;
      mMatrix[2][3] = 0;
    }
  else 
    {

      mMatrix[0][3] = mX;
      mMatrix[1][3] = mY; 
      mMatrix[2][3] = mZ;

      mX = 0;
      mY = 0;
      mZ = 0;

    }

 mOrderOps=order;
  // // let's see... as implemented, this negates translations... need to rethink
  // // the use case here.

  // if ( order==kGeneral ) // switch to generalized transformation
  //   {
  //     mMatrix[0][3]=mX;
  //     mMatrix[1][3]=mY;
  //     mMatrix[2][3]=mZ;
  //   }
  // else                  // switch to ordered transformation
  //   {
  //     mX = mMatrix[0][3];
  //     mY = mMatrix[1][3];
  //     mZ = mMatrix[2][3];
  //   }



}
//________________________________________________________________________________________________
bool hasOpt( const char* opt, TString optlist ){  return optlist.Contains(opt); }

bool AgPosition::Misalign( const char* tablename, const int rownumber, const char* options )
{

  TString opts = options;
  int lr = kLeftMultiply;
  if ( opts.Contains("right") )
    {
      lr = kRightMultiply;
    }
  if ( opts.Contains("left") )
    {
      lr = kLeftMultiply;
    }
  bool verbose = false; // opts.Contains("print");

  AgMLDbFunctor &GetMatrixFromDb = *AgMLDbFunctor::instance();

  mIsMisaligned = true;

  // Switch more to general transformation
  //  if ( mOrderOps != kGeneral)
  SetOrder( kGeneral );

  std::string mytable = table();
  mytable += ":";
  mytable += tablename;
  SetTable( mytable.c_str() ); // store name of table

  // Obtain transformation matrix from the database
  double M[4][4];
  GetMatrixFromDb( tablename, rownumber, M);

  if ( verbose ) { 
    LOG_INFO << "Matrix before misalignment" << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[0][0],mMatrix[0][1],mMatrix[0][2],mMatrix[0][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[1][0],mMatrix[1][1],mMatrix[1][2],mMatrix[1][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[2][0],mMatrix[2][1],mMatrix[2][2],mMatrix[2][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[3][0],mMatrix[3][1],mMatrix[3][2],mMatrix[3][3]) << endm;
    std::cout << std::endl;

    LOG_INFO << "Misalignment matrix (" << options << ")" << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", M[0][0],M[0][1],M[0][2],M[0][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", M[1][0],M[1][1],M[1][2],M[1][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", M[2][0],M[2][1],M[2][2],M[2][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", M[3][0],M[3][1],M[3][2],M[3][3]) << endm;
    std::cout << std::endl;
 
  }

  // And apply it
  Matrix( M, lr );

  if ( verbose ) { 
    LOG_INFO << "Matrix after misalignment" << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[0][0],mMatrix[0][1],mMatrix[0][2],mMatrix[0][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[1][0],mMatrix[1][1],mMatrix[1][2],mMatrix[1][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[2][0],mMatrix[2][1],mMatrix[2][2],mMatrix[2][3]) << endm;
    LOG_INFO << Form( "%8.5f   %8.5f   %8.5f | %8.5f", mMatrix[3][0],mMatrix[3][1],mMatrix[3][2],mMatrix[3][3]) << endm;
    std::cout << std::endl;
  }


  return true;
  
};
//________________________________________________________________________________________________
int AgPosition::NextCopyNumber()
{
  TGeoVolume *mothervolume = gGeoManager->FindVolumeFast( mother() );
  if ( 0==mothervolume ) {
    return -1;
  }

  if ( isSet("ncopy") ) {
    return par("ncopy");
  }

  int copy = 1;
  for ( int i=0;i<mothervolume->GetNdaughters();i++ )
    {
      TGeoNode *node   = mothervolume->GetNode(i);
      TString   name   = node->GetVolume()->GetName();
      TString   myname = StarAgmlStacker::realname(name);
      if ( myname.Contains( block() ) )
	{
	  copy++; // ok... this is simplified. 
        	  // But we take care of not placing same block elsewhere...
	}
    }
  return copy;

};
