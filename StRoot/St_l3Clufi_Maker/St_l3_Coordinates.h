#ifndef St_l3_Coordinates_hh
#define St_l3_Coordinates_hh
#include <Rtypes.h>
#include <StObject.h>



class St_l3_xyz_Coordinate: public StObject {

 private:
    Double_t xyz[3] ;
  
 public:
    St_l3_xyz_Coordinate() { xyz[0] = xyz[1] = xyz[2] = 0 ; } ;  
    St_l3_xyz_Coordinate(Double_t X, Double_t Y, Double_t Z )   
    {
     	    xyz[0] = X ;  
    	    xyz[1] = Y ;  
     	    xyz[2] = Z ;  
    } 
    ~St_l3_xyz_Coordinate() { ; }; // empty destructor

    // Setters and Getters
    Double_t Getx() const { return xyz[0] ; } ;
    Double_t Gety() const { return xyz[1] ; } ;
    Double_t Getz() const { return xyz[2] ; } ;
    Double_t* Getxyz() { return xyz ; } ;
    void Setx(Double_t X) { xyz[0] = X ; } ; 
    void Sety(Double_t Y) { xyz[1] = Y ; } ; 
    void Setz(Double_t Z) { xyz[2] = Z ; } ; 
    void Setxyz(Double_t X, Double_t Y, Double_t Z ) 
	{ 
	    xyz[0] = X ;  
	    xyz[1] = Y ;  
	    xyz[2] = Z ;  
	}
 
    // Root connection
    ClassDef(St_l3_xyz_Coordinate, 1)
} ;



class St_l3_ptrs_Coordinate: public StObject {

 private:
    Double_t ptrs[4] ; // pad, time, row, sec as double !
  
 public:
     St_l3_ptrs_Coordinate() { ptrs[0] = ptrs[1] = ptrs[2] = ptrs[3] = 0 ; }  ;  // empty constructor
     St_l3_ptrs_Coordinate(Double_t P,Double_t  T, Double_t R, Double_t S ) 
     {
       ptrs[0] = P ;
       ptrs[1] = T ;
       ptrs[2] = R ;
       ptrs[3] = S ;
     } ;
    ~St_l3_ptrs_Coordinate() { ;}; // empty destructor

     // Setters and Getters
     Double_t Getp() const { return ptrs[0] ; } ;
     Double_t Gett() const { return ptrs[1] ; } ;
     Double_t Getr() const { return ptrs[2] ; } ;
     Double_t Gets() const { return ptrs[3] ; } ;
     Double_t* Getptrs() { return ptrs ; } ;

     void Setp(Double_t P) { ptrs[0] = P ; } ; 
     void Sett(Double_t T) { ptrs[1] = T ; } ; 
     void Setr(Double_t R) { ptrs[2] = R ; } ; 
     void Sets(Double_t S) { ptrs[2] = S ; } ; 
     void Setptrs(Double_t P,Double_t  T, Double_t R, Double_t S ) 
	 {
	     ptrs[0] = P ;
	     ptrs[1] = T ;
	     ptrs[2] = R ;
	     ptrs[3] = S ; 
	 } ;
 
     // Root connection
     ClassDef(St_l3_ptrs_Coordinate, 1)
} ;

#endif //St_l3_Coordinates_hh
