#ifndef St_l3_Coordinates_hh
#define St_l3_Coordinates_hh

#include "Stiostream.h"

class St_l3_xyz_Coordinate  {

 private: 
    double xyz[3] ;
  
 public:
    St_l3_xyz_Coordinate() { xyz[0] = xyz[1] = xyz[2] = 0 ; } ;  
    St_l3_xyz_Coordinate(double X, double Y, double Z )   
    {
     	    xyz[0] = X ;  
    	    xyz[1] = Y ;  
     	    xyz[2] = Z ;  
    } 
    //virtual ~St_l3_xyz_Coordinate() { ; }; // empty destructor

    // Setters and Getters
    double Getx() const { return xyz[0] ; } ;
    double Gety() const { return xyz[1] ; } ;
    double Getz() const { return xyz[2] ; } ;
    double* Getxyz() { return xyz ; } ;
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

class St_l3_ptrs_Coordinate {
 private:
     double ptrs[4] ; // pad, time, row, sec as double !
  
 public:
     St_l3_ptrs_Coordinate() { ptrs[0] = ptrs[1] = ptrs[2] = ptrs[3] = 0 ; }  ;  // empty constructor
     St_l3_ptrs_Coordinate(double P,double  T, double R, double S ) 
     {
       ptrs[0] = P ;
       ptrs[1] = T ;
       ptrs[2] = R ;
       ptrs[3] = S ;
     } ;
     //virtual ~St_l3_ptrs_Coordinate() { ;}; // empty destructor

     // Setters and Getters
     double Getp() const { return ptrs[0] ; } ;
     double Gett() const { return ptrs[1] ; } ;
     double Getr() const { return ptrs[2] ; } ;
     double Gets() const { return ptrs[3] ; } ;
     double* Getptrs() { return ptrs ; } ;

     void Setp(double P) { ptrs[0] = P ; } ; 
     void Sett(double T) { ptrs[1] = T ; } ; 
     void Setr(double R) { ptrs[2] = R ; } ; 
     void Sets(double S) { ptrs[3] = S ; } ; 
     void Setptrs(double P,double  T, double R, double S ) 
	 {
	     ptrs[0] = P ;
	     ptrs[1] = T ;
	     ptrs[2] = R ;
	     ptrs[3] = S ; 
	 } ;
} ;
   
#endif //St_l3_Coordinates_hh
