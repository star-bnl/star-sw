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
  inline double refangle() const {return mrefangle;}
  inline double position() const {return mposition;}
  inline StHit* stHit() const {return msthit;}
  
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
  }
  
  double mrefangle;
  double mposition;
  double mx;
  double my;
  double mz;
  StHit* msthit;

};

//Non-member functions

ostream& operator<<(ostream&, const StiHit&);

#endif
