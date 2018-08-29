#ifndef __AgMLPosition_h__
#define __AgMLPosition_h__

#include <StarVMC/StarAgmlUtil/AgTransform.h>
#include <StarVMC/StarAgmlUtil/AgParameterList.h>

class AgMLPosition : public AgTransform, public AgParameterList<double>
{
public:

  AgMLPosition();
 ~AgMLPosition(){ /* nada */ };

  void Print( const char* opts="" ) const;

  /// Apply misalignment parameters from DB table.  The current matrix will
  /// be multiplied by the matrix accessed from the DB.
  /// @param tablename Name of the table in the DB
  /// @param options Delimited list of options.  "invert" will perform the inverse transformation.
  bool Misalign( const char* tablename=0, const int rownumber=0, const char* options="" );

  /// Reset state of transofmation matrix and apply alternate positioning based on DB table.
  bool Alternate( const char* tablename=0, const int rownumber=0, const char* options="" ){ return false; }

  // /// Apply specified matrix as a misalignment.  
  // bool Misalign( const double trn[4][4], const char *options="" );

  // /// Reset state of transofrmation matrix and apply the specified matrix instead.
  // bool Alternate( const double trn[4][4], const char* options="" );
   
  /// Perform translation
  void Translate( const double x, const double y, const double z );


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
  void SetOrder( OrderOps_t order );//{ mOrderOps = order; }
  int  GetOrder(){ return mOrderOps; }

  // big question w/ code below... getting X for general transform?

  /// Get X position
  double GetX();
  /// Get Y position
  double GetY();
  /// Get Z position
  double GetZ();

  /// Return the six G3 angles which correspond to the current state of the rotation matrix.
  void GetAngles( float& thetax, float& phix, 
		  float& thetay, float& phiy,
		  float& thetaz, float& phiz );
private:
protected:

  int         mOrderOps;
  double      mX, mY, mZ;


};

#endif
