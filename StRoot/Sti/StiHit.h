//StiHit.h
//M.L. Miller (Yale Software)
//04/01

//Hit class to be used for the ITTF tracker

#ifndef StiHit_HH
#define StiHit_HH

#include "TObject.h"

class StHit;
class ostream;

class StiHit : public TObject
{
 public:
  StiHit() {};
  
  StiHit(const StiHit& rhs) {
    if (*this==rhs) return;
    copyToThis(rhs);
    return;
  }
  
    ~StiHit() {};
  
  inline double x() const {return mx;}
  inline double y() const {return my;}
  inline double z() const {return mz;}
  inline double sxx() const {return msxx;}
  inline double syy() const {return msyy;}
  inline double szz() const {return mszz;}
  inline double sxy() const {return msxy;}
  inline double sxz() const {return msxz;}
  inline double syz() const {return msyz;}
  inline double refangle() const {return mrefangle;}
  inline double position() const {return mposition;}
  inline StHit* stHit() const {return msthit;}
  
  inline void set(double refAngle, double position,
		  double x, double y, double z, 
		  double sxx, double sxy, double sxz, 
		  double syy, double syz, double szz)
    {
      mrefangle = refAngle;
      mposition = position;
      mx = x;
      my = y;
      mz = z;
      msxx = sxx;
      msyy = syy;
      mszz = szz;
      msxy = sxy;
      msxz = sxz;
      msyz = syz;  
    }
  inline void setX(double val) {mx=val;}
  inline void setY(double val) {my=val;}
  inline void setZ(double val) {mz=val;}
  inline void setRefangle(double val) {mrefangle=val;}
  inline void setPosition(double val) {mposition=val;}
  inline void setStHit(StHit* val) {msthit=val;}
  
  inline bool operator==(const StiHit& rhs) {
    return ( mrefangle==rhs.mrefangle && mposition==rhs.mposition &&
	     mx==rhs.mx && my==rhs.my && mz==rhs.mz );
  }

  inline StiHit& operator=(const StiHit& rhs) {
    if (*this==rhs) return *this;
    copyToThis(rhs);
    return *this;
  }
  
 private:
  
  void copyToThis(const StiHit& rhs) {
    mrefangle = rhs.mrefangle;
    mposition = rhs.mposition;
    mx = rhs.mx;
    my = rhs.my;
    mz = rhs.mz;
    msxx = rhs.msxx;
    msyy = rhs.msyy;
    mszz = rhs.mszz;
    msxy = rhs.msxy;
    msxz = rhs.msxz;
    msyz = rhs.msyz;
  }
  
  double mrefangle;
  double mposition;
  double mx;
  double my;
  double mz; 
  double msxx;
  double msyy;
  double mszz;
  double msxy;
  double msxz;
  double msyz;
  StHit* msthit;

};

//Non-member functions

ostream& operator<<(ostream&, const StiHit&);

#endif
