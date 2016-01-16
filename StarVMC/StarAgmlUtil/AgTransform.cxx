#include "AgTransform.h"
// #include "StMessMgr.h"
// #include "TString.h"
// #include "TMath.h"

// Matrices of the form A[ROW][COL]

#include <cmath>
//#include <string>
#include <iostream>
#include <iomanip>

#define Option_t const char

const double pi = std::acos(-1.0);
const double deg2rad = pi / 180.0;
const double twopi   = pi * 2;

const double identity[4][4] =       
  {{1, 0, 0, 0}, 
   {0, 1, 0, 0}, 
   {0, 0, 1, 0}, 
   {0, 0, 0, 1} 
  };

AgTransform::AgTransform( const char* block, const char* mother, const char* group, const char* table ) 
  : mMatrix{ {1,0,0,0}, {0,1,0,0}, {0,0,1,0}, {0,0,0,1} }
{

  // If a DB table has been specified, look it up and overwrite
  if ( table ) {
    std::cout << "Use DB table " << table << std::endl;
  }
  
};

void AgTransform::Rotation( const double matrix[3][3] )
{
  double R[4][4] = {  
    {1,0,0,0}, 
    {0,1,0,0}, 
    {0,0,1,0}, 
    {0,0,0,1}  
  };

  for ( int row=0; row<3; row++ )
  for ( int col=0; col<3; col++ )
    {
      R[row][col] = matrix[row][col];
    }
  Matrix( R );

};

void AgTransform::Print( Option_t *opts ) const
{
  const int prec  = 5;

  if ( 0==opts )  {
    //    std::cout << GetTitle() << std::endl;
  }
  else {
    std::cout << opts << std::endl;
  }

  for ( int row=0; row<4; row++ ) {
    //  TString column = "";
    //  for ( int col=0; col<4; col++ ) {
    //    column += Form("%8.3G ",mMatrix[row][col]);
    //  };
    //  std::cout << column.Data() << std::endl;

    for ( int col=0;col<4;col++ ) {
      std::cout << std::fixed 
		<< std::setw(5+prec)
		<< std::right
		<< std::setprecision(prec)
		<< mMatrix[row][col] << "   ";
    }
    std::cout << std::endl;
  };
};


void AgTransform::Matrix( const double matrix[4][4], int lr )
{
  if ( kRightMultiply == lr ) MatrixRight( matrix );
  else                        MatrixLeft ( matrix );
  return;

}

void AgTransform::MatrixLeft( const double matrix[4][4] )
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

  // temp result
  double R[4][4];
  for ( int row=0; row<4; row++ ) {
  for ( int col=0; col<4; col++ ) {

      R[row][col] = 0;
      for ( int inner = 0; inner < 4; inner ++ ) {

	R[row][col] += matrix[row][inner] * mMatrix[inner][col];
      }
      if (fabs(R[row][col])<3.0E-16) R[row][col]=0;
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

  //  Print();

};



void AgTransform::MatrixRight( const double matrix[4][4] )
{
  //
  // Multiply matrix into mMatrix from the RIGHT
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
  // Right multiplication does not do this...
  //
  static double R[4][4]; 
  for ( int row=0; row<4; row++ ) {
  for ( int col=0; col<4; col++ ) {
    R[row][col]=0;
      for ( int inner = 0; inner < 4; inner ++ ) {
	R[row][col] += mMatrix[row][inner] * matrix[inner][col];
      }
      if (fabs(R[row][col])<3.0E-16) R[row][col]=0;
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
  for ( int row=0; row<4; row++ ) 
  for ( int col=0; col<4; col++ ) 
    mMatrix[row][col] = R[row][col];

  

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

void AgTransform::RotateX( double alpha )
{
  double cosA = std::cos(alpha*deg2rad);
  double sinA = std::sin(alpha*deg2rad);
  double trm[4][4] = 
    {
      1.00, 0.00, 0.00, 0.00,
      0.00, cosA,-sinA, 0.00,
      0.00, sinA, cosA, 0.00,
      0.00, 0.00, 0.00, 1.00
    };
  Matrix(trm, kLeftMultiply);
};
void AgTransform::RotateY( double alpha )
{
  double cosA = std::cos(alpha*deg2rad);
  double sinA = std::sin(alpha*deg2rad);  
  double trm[4][4] = 
    {
      cosA, 0.00, sinA, 0.00,
      0.00, 1.00, 0.00, 0.00,
     -sinA, 0.00, cosA, 0.00,
      0.00, 0.00, 0.00, 1.00
    };
  Matrix(trm,kLeftMultiply);
};
void AgTransform::RotateZ( double alpha )
{
  double cosA = std::cos(alpha*deg2rad);
  double sinA = std::sin(alpha*deg2rad);  
  double trm[4][4] = 
    {
      cosA,-sinA, 0.00, 0.00,
      sinA, cosA, 0.00, 0.00,
      0.00, 0.00, 1.00, 0.00,
      0.00, 0.00, 0.00, 1.00
    };
  Matrix(trm,kLeftMultiply);
};
void AgTransform::Reset()
{
  for ( int row=0;row<4;row++ )
  for ( int col=0;col<4;col++ )
    mMatrix[row][col] = identity[row][col];
};

bool AgTransform::IsIdentity()
{
  const double tol = 1.0E-5;
  for ( int row=0;row<3;row++ )
  for ( int col=0;col<3;col++ )
    if ( std::fabs( mMatrix[row][col] - identity[row][col] ) > tol ) return false;

  return true;
}

double roundOffPhi( const double _phi ) // phi [0,360]
{
  double phi = _phi;
  while ( phi >=  360.0 ) phi -= 360.0;
  while ( phi <= -360.0 ) phi += 360.0;
  if ( abs(phi) < 2.0E-16 ) phi = 0.;
  return phi;
};

void AgTransform::Angles( const double _thetax, const double _phix,
			  const double _thetay, const double _phiy,
			  const double _thetaz, const double _phiz )
{

  double thetax = _thetax * deg2rad,  phix = roundOffPhi( _phix ) * deg2rad ;
  double thetay = _thetay * deg2rad,  phiy = roundOffPhi( _phiy ) * deg2rad ;
  double thetaz = _thetaz * deg2rad,  phiz = roundOffPhi( _phiz ) * deg2rad ;

  double Cos_Phix_Sin_Thetax = std::cos(phix) * std::sin(thetax);
  double Sin_Phix_Sin_Thetax = std::sin(phix) * std::sin(thetax);
  double Cos_Thetax          = std::cos( thetax );

  double Cos_Phiy_Sin_Thetay = std::cos(phiy) * std::sin(thetay);
  double Sin_Phiy_Sin_Thetay = std::sin(phiy) * std::sin(thetay);
  double Cos_Thetay          = std::cos( thetay );

  double Cos_Phiz_Sin_Thetaz = std::cos(phiz) * std::sin(thetaz);
  double Sin_Phiz_Sin_Thetaz = std::sin(phiz) * std::sin(thetaz);
  double Cos_Thetaz          = std::cos( thetaz );

  double trm[4][4] = {
    Cos_Phix_Sin_Thetax,     Cos_Phiy_Sin_Thetay,     Cos_Phiz_Sin_Thetaz,                 0,
    Sin_Phix_Sin_Thetax,     Sin_Phiy_Sin_Thetay,     Sin_Phiz_Sin_Thetaz,                 0,
    Cos_Thetax,              Cos_Thetay,              Cos_Thetaz,                          0,
    0,                       0,                       0,                                   1
  };

  Matrix( trm, kLeftMultiply );

}


void AgTransform::Ortho( const char* opts )
{
  //  TString myort=opts;
  //  myort.ToUpper();

  std::string myort = opts;

  short  dir  = +1;
  int    axis =  0;

  int     ort[3] = { 0, 0, 0 };
  

//	for ( int i=0;i<myort.Length();i++ )
	for ( unsigned int i=0;i<myort.size();i++ )
	{
		if ( myort[i] == '+' ) { dir =  1; continue; }
		if ( myort[i] == '-' ) { dir = -1; continue; }
		if ( myort[i] == 'X' ) { dir *= 1; }
		if ( myort[i] == 'Y' ) { dir *= 2; }
		if ( myort[i] == 'Z' ) { dir *= 3; }
		if ( myort[i] == 'x' ) { dir *= 1; }
		if ( myort[i] == 'y' ) { dir *= 2; }
		if ( myort[i] == 'z' ) { dir *= 3; }
		ort[ axis++ ] = dir;
		dir=+1; // and reset to + direction
	}
  //                         -z   -y   -x        +x   +y   +z
  const double thetas[] = { 180,  90,  90, 000,  90,  90,   0 };
  const double phis[]   = {   0, 270, 180, 000,   0,  90,   0 };
  int ix = ort[0];
  int iy = ort[1];
  int iz = ort[2];


#if 0

  // OLD CODE REQUIRING RIGHT MULTIPLY

  Angles( thetas[ix+3], phis[ix+3], thetas[iy+3], phis[iy+3], thetas[iz+3], phis[iz+3] );

#else

  // New code breaks dependence on Reference / Angles

  double thetax = thetas[ix+3]* deg2rad;
  double thetay = thetas[iy+3]* deg2rad;
  double thetaz = thetas[iz+3]* deg2rad;

  double phix = phis[ix+3]* deg2rad;
  double phiy = phis[iy+3]* deg2rad;
  double phiz = phis[iz+3]* deg2rad;

  double Cos_Phix_Sin_Thetax = std::cos(phix) * std::sin(thetax);
  double Sin_Phix_Sin_Thetax = std::sin(phix) * std::sin(thetax);
  double Cos_Thetax          = std::cos( thetax );

  double Cos_Phiy_Sin_Thetay = std::cos(phiy) * std::sin(thetay);
  double Sin_Phiy_Sin_Thetay = std::sin(phiy) * std::sin(thetay);
  double Cos_Thetay          = std::cos( thetay );

  double Cos_Phiz_Sin_Thetaz = std::cos(phiz) * std::sin(thetaz);
  double Sin_Phiz_Sin_Thetaz = std::sin(phiz) * std::sin(thetaz);
  double Cos_Thetaz          = std::cos( thetaz );

  double trm[4][4] = {
    Cos_Phix_Sin_Thetax,     Cos_Phiy_Sin_Thetay,     Cos_Phiz_Sin_Thetaz,                 0,
    Sin_Phix_Sin_Thetax,     Sin_Phiy_Sin_Thetay,     Sin_Phiz_Sin_Thetaz,                 0,
    Cos_Thetax,              Cos_Thetay,              Cos_Thetaz,                          0,
    0,                       0,                       0,                                   1
  };

  Matrix( trm, kRightMultiply );
#endif
  
  return;
}
