//StiHit.h
//M.L. Miller (Yale Software)
//04/01

//Hit class to be used for the ITTF tracker

#ifndef StiHit_HH
#define StiHit_HH

#include "StiObjectFactory.h"

class StHit;
class StThreeVectorD;
class StMatrixF;
class StiDetector;

class StiHit 
{
public:
    StiHit();
    StiHit(const StiHit&);   
    ~StiHit();

    //Gets
    double x() const;
    double y() const;
    double z() const;
    double sxx() const;
    double syy() const;
    double szz() const;
    double sxy() const;
    double sxz() const;
    double syz() const;
    double refangle() const;
    double position() const;
    
    const StiDetector* detector() const;
    StiDetector* detector();
    
    const StHit* stHit() const;
    
    bool   isUsed() const;
    StThreeVectorD globalPosition() const;

    //Sets
    void set(double refAngle, double position,double x, double y, double z, 
	     double sxx, double sxy, double sxz, double syy, double syz, double szz);
    
    void setX(double);
    void setY(double);
    void setZ(double);
    void setRefangle(double);
    void setPosition(double);
    void setError(const StMatrixF&);
    void setDetector(StiDetector*);
    void setStHit(StHit*);
    void setUsed(bool);

    //Operators
    bool operator==(const StiHit&);
    StiHit& operator=(const StiHit&);

    //Action
    void reset();
    
private:
  
    void copyToThis(const StiHit&);
    
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
    bool   mused;
    StiDetector* mdetector;
    StHit* msthit;
};

//Inlines-----------------------------------------------------------

inline void StiHit::reset()
{
    mrefangle = mposition = mx = my = mz = msxx = msyy = mszz = msxy = msxz = msyz = 0.;
    mused = false;
    mdetector = 0;
    msthit = 0;
}

inline double StiHit::x() const {return mx;}

inline double StiHit::y() const {return my;}

inline double StiHit::z() const {return mz;}

inline double StiHit::sxx() const {return msxx;}

inline double StiHit::syy() const {return msyy;}

inline double StiHit::szz() const {return mszz;}

inline double StiHit::sxy() const {return msxy;}

inline double StiHit::sxz() const {return msxz;}

inline double StiHit::syz() const {return msyz;}

inline double StiHit::refangle() const {return mrefangle;}

inline double StiHit::position() const {return mposition;}

inline const StHit* StiHit::stHit() const {return msthit;}

inline const StiDetector* StiHit::detector() const {return mdetector;}

inline StiDetector* StiHit::detector() {return mdetector;}

inline bool   StiHit::isUsed() const { return mused;}

inline void StiHit::set(double refAngle, double position, double x, double y, double z, 
			double sxx, double sxy, double sxz, double syy, double syz, double szz) 
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
    mused = false;
}

inline void StiHit::setX(double val) {mx=val;}

inline void StiHit::setY(double val) {my=val;}

inline void StiHit::setZ(double val) {mz=val;}

inline void StiHit::setRefangle(double val) {mrefangle=val;}

inline void StiHit::setPosition(double val) {mposition=val;}

inline void StiHit::setDetector(StiDetector* det) {mdetector=det;}

inline void StiHit::setStHit(StHit* val) {msthit=val;}

inline void StiHit::setUsed(bool val) { mused = val;}

inline bool StiHit::operator==(const StiHit& rhs)
{
    return ( mrefangle==rhs.mrefangle && mposition==rhs.mposition &&
	     mx==rhs.mx && my==rhs.my && mz==rhs.mz );
}

inline StiHit& StiHit::operator=(const StiHit& rhs)
{
    if (*this==rhs) return *this;
    copyToThis(rhs);
    return *this;
}

inline void StiHit::copyToThis(const StiHit& rhs)
{
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
    mused = rhs.mused;
}

typedef StiObjectFactory<StiHit> StiHitFactory;

#endif
