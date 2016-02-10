#include "AgMLPosition.h"
#include <iostream>
#include <assert.h>
#include <string>

AgMLPosition *_position = 0;

//@define __UNIT_TEST__

const double pi = std::acos(-1.0);
const double deg2rad = pi / 180.0;
const double rad2deg = 180.0 / pi;
const double twopi   = pi * 2;

const bool verbose = false;

extern "C" 
{
  
  void agml_position_begin_( const char* _volume, int l )
  {    
    if ( _position ) {
      delete _position;
    }
    //    volume[l--] = '\0'; // NULL terminate the string
    std::string volume = _volume;
    volume = volume.substr(0,l);

    _position = new AgMLPosition();
    _position->Reset();
    _position->SetBlock(volume.c_str());

    if(verbose) _position -> Print(volume.c_str());

  };
  void agml_set_mother_( const char* mother, int l ){ _position->SetMother(mother); }
  void agml_set_group_ ( const char* group,  int l ){ _position->SetGroup(group); }
  void agml_set_table_ ( const char* table,  int l ){ _position->SetTable(table); }


  void agml_position_end_()
  {
    _position->Print();
  };
  void agml_rotate_x_( float& angle ){ 
    //  std::cout << "RotateX = " << angle << std::endl;
    _position->RotateX(angle); 
    if(verbose) _position -> Print("RotateX");
    //  _position->Print("rotateX");
  }
  void agml_rotate_y_( float& angle ){ 
    //  std::cout << "RotateY = " << angle << std::endl; 
    _position->RotateY(angle); 
    if(verbose) _position -> Print("RotateY");
    //  _position->Print("rotateY");
  }
  void agml_rotate_z_( float& angle ){ 
    //  std::cout << "RotateZ = " << angle << std::endl;
    _position->RotateZ(angle);
    if(verbose) _position -> Print("RotateZ"); 
    //  _position->Print("rotateZ");
  }
  void agml_translate_x_( float& x ) { 
    _position->TranslateX(x); 
    if(verbose) _position -> Print("TranslateX");
  } 
  void agml_translate_y_( float& y ) { 
    _position->TranslateY(y); 
    if(verbose) _position -> Print("TranslateY");
  }
  void agml_translate_z_( float& z ) { 
    _position->TranslateZ(z); 
    if(verbose) _position -> Print("TranslateZ");
  }
  void agml_set_angles_( float& thetax, float& phix, float& thetay, float& phiy, float& thetaz, float& phiz )
  {
    _position->Angles( thetax, phix, thetay, phiy, thetaz, phiz );
    if(verbose) _position -> Print("G3 Angles");
  };
  void agml_ortho_( const char* ort, int l  ){ 
    _position->Ortho(ort); 
    if(verbose) _position -> Print(ort);
  }
  void agml_invert(){ 
    _position->Invert(); 
    if(verbose) _position -> Print("Invert");
  }
  void agml_rotation( const double rotm[3][3] ){ 
    _position->Rotation(rotm); 
    if(verbose) _position -> Print("Rot matrix");
  }

  void agml_get_angles_( float& tx, float& px, float& ty, float& py, float& tz, float& pz ) {
    _position->GetAngles(tx,px,ty,py,tz,pz);
  };
    
  void agml_get_translation_( float &x, float& y, float& z ) {
    x = _position->GetX();
    y = _position->GetY();
    z = _position->GetZ();
  };
  
};
//________________________________________________________________________________________________
AgMLPosition::AgMLPosition() :
  AgTransform(),
  AgParameterList(),
  mOrderOps(kRotTran),
  mX(0),
  mY(0),
  mZ(0)
{

};

//________________________________________________________________________________________________
void AgMLPosition::Print( const char* opts ) const
{
  if ( opts ) AgTransform::Print(opts);
  else {
    std::string myopts = _Block;
    myopts += " in ";
    myopts += _Mother;
    myopts += " group=";
    myopts += _Group;
    myopts += " table=";
    myopts += _Table;
    AgTransform::Print(myopts.c_str());
  }

  return;

}
//________________________________________________________________________________________________
bool AgMLPosition::Alternate( const double tr[4][4], const char* opts )
{
  // TODO: implement inverse transformation
  Reset();
  Matrix( tr );
  return true;
};
//________________________________________________________________________________________________
bool AgMLPosition::Misalign( const double tr[4][4], const char* opts )
{
  // TODO: implement inverse transformation
  Matrix( tr );
  return true;
}
//________________________________________________________________________________________________
void AgMLPosition::Translate( double x, double y, double z ) {
  // Perform general transformation
  std::cout << "Peforming agml translation" << std::endl;
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
    
  return;

};
//________________________________________________________________________________________________
void AgMLPosition::GetAngles( float& thetax, float& phix, float& thetay, float& phiy, float& thetaz, float& phiz )
{

  //
  // Calculate direction of each axis
  //
  thetax = std::acos( mMatrix[2][0] ) * rad2deg;
  thetay = std::acos( mMatrix[2][1] ) * rad2deg;
  thetaz = std::acos( mMatrix[2][2] ) * rad2deg;

  if ( std::abs( mMatrix[0][0] ) < 1.0e-6 && std::abs( mMatrix[1][0] ) < 1.0e-6 ) phix = 0;
  else { phix = std::atan2( mMatrix[1][0], mMatrix[0][0] ) * rad2deg; }

  if ( std::abs( mMatrix[0][1] ) < 1.0e-6 && std::abs( mMatrix[1][1] ) < 1.0e-6 ) phiy = 0;
  else { phiy = std::atan2( mMatrix[1][1], mMatrix[0][1] ) * rad2deg; }

  if ( std::abs( mMatrix[0][2] ) < 1.0e-6 && std::abs( mMatrix[1][2] ) < 1.0e-6 ) phiz = 0;
  else { phiz = std::atan2( mMatrix[1][2], mMatrix[0][2] ) * rad2deg; }

  //
  // Now normalize
  //
  while ( thetax < 0 ) thetax += 360;
  while ( thetay < 0 ) thetay += 360;
  while ( thetaz < 0 ) thetaz += 360;
  while ( phix < 0 ) phix += 360;
  while ( phiy < 0 ) phiy += 360;
  while ( phiz < 0 ) phiz += 360;

  // while ( thetax > 360 ) thetax -= 360;
  // while ( thetay > 360 ) thetay -= 360;
  // while ( thetaz > 360 ) thetaz -= 360;
  // while ( phix > 360 ) phix -= 360;
  // while ( phiy > 360 ) phiy -= 360;
  // while ( phiz > 360 ) phiz -= 360;



  return;

};
//________________________________________________________________________________________________
#ifdef __UNIT_TEST__

struct unit_test1
{
  unit_test1() 
    {
      _position = new AgMLPosition();
      float thetax, thetay, thetaz, phix, phiy, phiz;
      _position->GetAngles( thetax, phix, thetay, phiy, thetaz, phiz );
      _position->Print("Initialized matrix");
      std::cout << "thetax = " << thetax << " phix = " << phix << std::endl;
      std::cout << "thetay = " << thetay << " phiy = " << phiy << std::endl;
      std::cout << "thetaz = " << thetaz << " phiz = " << phiz << std::endl;
      assert(0);
    };
};
struct unit_test2
{
  unit_test2() 
    {
      _position = new AgMLPosition();
      float thetax, thetay, thetaz, phix, phiy, phiz;
      _position->RotateX(90.0);
      _position->GetAngles( thetax, phix, thetay, phiy, thetaz, phiz );
      _position->Print("Rotate X 90 degrees matrix");
      std::cout << "thetax = " << thetax << " phix = " << phix << std::endl;
      std::cout << "thetay = " << thetay << " phiy = " << phiy << std::endl;
      std::cout << "thetaz = " << thetaz << " phiz = " << phiz << std::endl;
      assert(0);
    };
};

unit_test2 U;

#endif
