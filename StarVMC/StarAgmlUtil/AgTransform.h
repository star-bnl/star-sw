#ifndef __AgTransform_h__
#define __AgTransform_h__

//#include "TNamed.h"
//#include "TString.h"
//#include "AgParameterList.h"
#include <map>

//class TGeoCombiTrans;

class AgTransform  
{
public:
  AgTransform( const char* block=0, const char* mother=0, const char* group=0, const char* table=0 );
  virtual ~AgTransform(){ /* nada */ };

  /// Apply rotation matrix
  void Rotation( const double rotm[3][3] );

  /// Apply translation to current matrix
  virtual void Translate ( const double x, const double y, const double z );
  void TranslateX( const double x ) { Translate(x,0,0); }
  void TranslateY( const double x ) { Translate(0,x,0); }
  void TranslateZ( const double x ) { Translate(0,0,x); }

  /// Returns true if this is the identity rotation matrix (ignoring rotations < 1E-5)
  /// Does not check that translation = 0
  bool IsIdentity();

  /// Multiply by transformation matrix
  void Matrix( const double trm[4][4], int lr=kRightMultiply );

  /// Multiply current state by transformation matrix on the right.  i.e. current = current * trm.
  void MatrixRight( const double trm[4][4] );

  /// Multiply current state by transformation matrix on the left.  i.e. current = trm * current.
  void MatrixLeft( const double trm[4][4] );  

  /// Inverse transformation
  void Invert();

  /// Print the current rotation matrix
  void Print( const char* opts=0 ) const;

  /// Rotate about X axis
  /// @param alpha rotation angle expressed in degrees
  void RotateX( const double alpha );

  /// Rotate about Y axis
  /// @param alpha rotation angle expressed in degrees
  void RotateY( const double alpha );

  /// Rotate about Z axis
  /// @param alpha rotation angle expressed in degrees
  void RotateZ( const double alpha );

  /// Perform ORT rotation
  void Ortho( const char* ort );

  /// Set angles using G3 notation
  /// @param thetax, phix polar and azimuthal angles, expressed in degrees, of the x' axis
  /// @param thetay, phiy polar and azimuthal angles, expressed in degrees, of the y' axis
  /// @param thetaz, phiz polar and azimuthal angles, expressed in degrees, of the z' axis
  void Angles( const double thetax, const double phix,
	       const double thetay, const double phiy,
	       const double thetaz, const double phiz );

  enum { kLeftMultiply, kRightMultiply };
	       

  /// Reset the matrix to unit matrix 
  void Reset();

  // /// Get the six G3 angles defined by the rotation part of the matrix
  // void getAngles( double& thetax,  double& phix,
  // 		  double& thetay,  double& phiy,
  // 		  double& thetaz,  double& phiz );


private:
protected:

  double mMatrix[4][4];

};


#endif
