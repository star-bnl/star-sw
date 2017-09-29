#ifndef __AgPosition_h__
#define __AgPosition_h__

#include <StarVMC/StarAgmlUtil/AgTransform.h>
//#include "AgParameterList.h"
#include <StarVMC/StarAgmlUtil/AgParameterList.h>

#include <TNamed.h>

class TGeoMatrix;

class AgPosition : public AgTransform, public AgParameterList<double>, public TNamed
{
public:

  AgPosition();
 ~AgPosition(){ /* nada */ };

  /// Apply misalignment parameters from DB table.  The current matrix will
  /// be multiplied by the matrix accessed from the DB.
  /// @param tablename Name of the table in the DB
  /// @param options Delimited list of options.  "invert" will perform the inverse transformation.
  bool Misalign( const char* tablename=0, const int rownumber=0, const char* options="" ) { return false; }

  /// Reset state of transofmation matrix and apply alternate positioning based on DB table.
  bool Alternate( const char* tablename=0, const int rownumber=0, const char* options="" ){ return false; }

  /// Apply specified matrix as a misalignment.  
  bool Misalign( const double trn[4][4], const char *options="" );

  /// Reset state of transofrmation matrix and apply the specified matrix instead.
  bool Alternate( const double trn[4][4], const char* options="" );
   
  /// Returns a pointer to a new instance of a matrix.  
  TGeoMatrix* matrix(); 

  /// Returns a pointer to a new instance of the rotation matrix
  TGeoMatrix *rotation();

  /// Returns a pointer to a new instance of the translation
  TGeoMatrix *translation();

  /// Perform translation
  void Translate( const double x, const double y, const double z );

  /// Perform Ortho transformation.  (Must respect order of operations, so overrides base class).
  
  /// Rotation matrix (nullifies all prior rotations)
  void RotationMatrix( Double_t rotm[9] ){ for (int i=0;i<9;i++ ) mRotationMatrix[i]=rotm[i]; mHasRotm=true; }



  /// Referrence sets the six G3 angles.  It is proceeded by a reset of the
  /// state of the matrix back to 1.
  void Reference( const double thetax, const double phix,
		  const double thetay, const double phiy,
		  const double thetaz, const double phiz ) 
  { 
    Angles( thetax, phix, thetay, phiy, thetaz, phiz ); 
  }

  void AlphaX( const double alpha ) { RotateX(alpha); }
  void AlphaY( const double alpha ) { RotateY(alpha); }
  void AlphaZ( const double alpha ) { RotateZ(alpha); }

  /// Specifiy order of operations
  enum OrderOps_t { 
    kUnknown,  /// Just don't
    kGeneral,  /// General transformation.  Rotations and translations evaluated in order
    kRotTran,  /// Rotations executed first, then translations
    kTranRot   /// Translations executed first, then rotations (TODO)
  };
  
  /// Order of operations
  void SetOrder( OrderOps_t order ){ mOrderOps = order; }

private:
protected:

  int         mOrderOps;
  double      mX, mY, mZ;
  double      mRotationMatrix[9];
  bool        mHasRotm;


  ClassDef(AgPosition,1);
};

#endif
