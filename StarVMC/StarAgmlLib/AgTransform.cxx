#include "AgTransform.h"
#include "StMessMgr.h"
#include "TString.h"

// Matrices of the form A[ROW][COL]


AgTransform::AgTransform( const char* block, const char* mother, const char* group, const char* table ) 
  : TNamed(block,Form("Place %s into %s",block,mother)),
    mMatrix{ {1,0,0,0}, {0,1,0,0}, {0,0,1,0}, {0,0,0,1} }
{

  // If a DB table has been specified, look it up and overwrite
  if ( table ) {
    LOG_INFO << "Use DB table " << table << endm;
  }
  
};

void AgTransform::Rotation( const double matrix[3][3] )
{
  double R[4][4] = {  {1,0,0,0}, {0,1,0,0}, {0,0,1,0}, {0,0,0,1}  };
  for ( int row=0; row<3; row++ )
  for ( int col=0; col<3; col++ )
    {
      R[row][col] = matrix[row][col];
    }
  Matrix( R );
};

void AgTransform::Print( Option_t *opts ) const
{
  if ( 0==opts )  {
    LOG_INFO << GetTitle() << endm;
  }
  else {
    LOG_INFO << opts << endm;
  }

  for ( int row=0; row<4; row++ ) {
    TString column = "";
    for ( int col=0; col<4; col++ ) {
      column += Form("%8.3G ",mMatrix[row][col]);
    };
    LOG_INFO << column.Data() << endm;
  };
};


void AgTransform::Matrix( const double matrix[4][4] )
{
  //
  // Multiply matrix into mMatrix from the LEFT
  //
  // R = matrix * mMatrix 
  //
  // We want to be able to go from local to global coordinates:
  //
  //  G = T1 * T2 * ... * Tn * L
  //
  // where L is the local transformation relative to its mother, and T1 ... Tn
  // are the transformations all the way up to the master volume (T1=1).
  //
  static double R[4][4]; 
  for ( int row=0; row<4; row++ ) {
  for ( int col=0; col<4; col++ ) {
      double sum = 0.0;
      for ( int inner = 0; inner < 4; inner ++ ) {
	sum += matrix[row][inner] * mMatrix[inner][col];
      }
      if (abs(sum)<1.0E-16) sum=0;
      R[row][col] = sum;

  }
  }
  //
  // Store result in mMatrix
  //
  for ( int row=0; row<4; row++ ) {
    for ( int col=0; col<4; col++ ) {
      mMatrix[row][col] = R[row][col];
    }
  }
};

void AgTransform::Invert()
{
  // Transpose the roation part of the matrix
  double R[4][4] = {  {1,0,0,0}, {0,1,0,0}, {0,0,1,0}, {0,0,0,1}  };
  
  for ( int row=0; row<3; row++ )
  for ( int col=0; col<3; col++ )
    R[row][col] = mMatrix[col][row];

  // Switch sign on translation part of the matrix
  for ( int row=0; row<3; row++ )
    R[row][3] = -mMatrix[row][3];

  //
  // Store result in mMatrix
  //
  for ( int row=0; row<4; row++ ) {
    for ( int col=0; col<4; col++ ) {
      mMatrix[row][col] = R[row][col];
    }
  }  
}

void AgTransform::Translate(const double x, const double y, const double z )
{
  const double v[] = { x, y, z };
  for ( int i=0;i<3;i++ )
    {
      mMatrix[i][3] += v[i]; // Set x,y,z in last column of matrix
    }
  return;

}
