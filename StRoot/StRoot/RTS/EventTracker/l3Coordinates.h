#ifndef L3_COORDINATES_H
#define L3_COORDINATES_H

#include <iostream>
#include <math.h>

//#include "FtfBaseHit.h"

class l3ThreeVector
{
 public:
    l3ThreeVector ( float _x=0, float _y=0, float _z=0 )
	{ x = _x ; y = _y ; z = _z ; } ; 
    void set ( float _x, float _y, float _z )
	{ x = _x ; y = _y ; z = _z ; } ; 

    float x ;
    float y ;
    float z ;
};



class l3xyzCoordinate  {

 private: 
    double xyz[3] ;
  
 public:
    l3xyzCoordinate() { xyz[0] = xyz[1] = xyz[2] = 0 ; } ;  
    l3xyzCoordinate(double X, double Y, double Z )   
    {
     	    xyz[0] = X ;  
    	    xyz[1] = Y ;  
     	    xyz[2] = Z ;  
    } 
    //virtual ~St_l3_xyz_Coordinate() { ; }; // empty destructor

    // Copy assignment/constructor for Ftf3DHit
    l3xyzCoordinate(l3ThreeVector hit) {
	Setxyz(hit.x, hit.y, hit.z);
    }

    l3xyzCoordinate operator=(l3ThreeVector hit) {
	Setxyz(hit.x, hit.y, hit.z);

	return *this;
    }

    // Setters and Getters
    double Getx() const { return xyz[0] ; } ;
    double Gety() const { return xyz[1] ; } ;
    double Getz() const { return xyz[2] ; } ;
    double* Getxyz() { return xyz ; } ;
    
    inline double Getr() { 
	return sqrt ( xyz[0]*xyz[0] +
		      xyz[1]*xyz[1] +
		      xyz[2]*xyz[2] );
    }

    inline double Gettheta() { 
	return atan2(hypot(xyz[0], xyz[1]), xyz[2]);
    }
    
    inline double Geteta() { 
	return - log(tan(Gettheta()/2));
    }
    
    inline double Getphi() { 
	return atan2(xyz[1], xyz[0]);
    }
    
    
    
    void Setx(double X) { xyz[0] = X ; } ; 
    void Sety(double Y) { xyz[1] = Y ; } ; 
    void Setz(double Z) { xyz[2] = Z ; } ; 
    void Setxyz(double X, double Y, double Z ) 
	{ 
	    xyz[0] = X ;  
	    xyz[1] = Y ;  
	    xyz[2] = Z ;  
	}

} ;

class l3ptrsCoordinate {
 private:
     double ptrs[4] ; // pad, time, row, sec as double !
  
 public:
     //St_l3_ptrs_Coordinate() { ptrs[0] = ptrs[1] = ptrs[2] = ptrs[3] = 0 ; }  ;  
     // empty constructor

     l3ptrsCoordinate(double P=0,double  T=0, double R=0, double S=0 ) 
     {
       ptrs[0] = P ;
       ptrs[1] = T ;
       ptrs[2] = R ;
       ptrs[3] = S ;
     } ;
     //virtual ~St_l3_ptrs_Coordinate() { ;}; // empty destructor

     // Setters and Getters
     inline double Getp() const { return ptrs[0] ; } ;
     inline double Gett() const { return ptrs[1] ; } ;
     inline double Getr() const { return ptrs[2] ; } ;
     inline double Gets() const { return ptrs[3] ; } ;
     inline double* Getptrs() { return ptrs ; } ;

     inline void Setp(double P) { ptrs[0] = P ; } ; 
     inline void Sett(double T) { ptrs[1] = T ; } ; 
     inline void Setr(double R) { ptrs[2] = R ; } ; 
     inline void Sets(double S) { ptrs[3] = S ; } ; 
     inline void Setptrs(double P,double  T, double R, double S ) {
	 ptrs[0] = P ;
	 ptrs[1] = T ;
	 ptrs[2] = R ;
	 ptrs[3] = S ; 
     } ;
} ;
   
#endif
