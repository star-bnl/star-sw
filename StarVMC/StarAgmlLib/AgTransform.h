#ifndef __AgTransform_h__
#define __AgTransform_h__

#include "TNamed.h"
#include "TString.h"
#include "AgParameterList.h"
#include <map>

class TGeoCombiTrans;

class AgTransform : public TNamed, public AgParameterList<double>
{
public:
  AgTransform( const char* block, const char* mother, const char* group=0, const char* table=0 );
 ~AgTransform(){ /* nada */ };

  /// Apply rotation matrix
  void Rotation( const double rotm[3][3] );

  /// Apply translation to current matrix
  void Translate( const double x, const double y, const double z );

  /// Multiply by transformation matrix
  void Matrix( const double trm[4][4] );

  /// Inverse transformation
  void Invert();

  /// Print the current rotation matrix
  void Print( Option_t *opts=0 ) const;

private:
protected:

  double mMatrix[4][4];

  ClassDef(AgTransform,1);

};

#endif
