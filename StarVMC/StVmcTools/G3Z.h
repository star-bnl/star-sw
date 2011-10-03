#ifndef G3Z_H
#define G3Z_H



/*
 Box  ?  dx,dy,dz;
 Trd1 ?  dx1,dx2, dy,dz;
 Trd2 ?  dx1,dx2, dy1,dy2, dz;
 Trap ?  dz,thet,phi, h1,bl1,tl1,alp1, h2,bl2,tl2,alp2;
 Tube ?  Rmin,Rmax,Dz;
 Tubs ?  Rmin,Rmax,Dz, Phi1,Phi2;
 Cone ?  Dz, Rmn1,Rmx1, Rmn2,Rmx2;
 Cons ?  Dz, Rmn1,Rmx1, Rmn2,Rmx2, phi1,phi2;
 Sphe ?  Rmin,Rmax, The1,The2, Phi1,Phi2;
 Para ?  Dx,Dy,Dz,  Alph,Thet,Phi;
 Pgon ?  Phi1,Dphi,Npdiv,Nz;
 Pcon ?  Phi1,Dphi,      Nz;
 Eltu ?  P1, P2, Dz;
 Hype ?  Rmin,Rmax,Dz,Thet;
 Gtra ?  Dz,Thet,Phi,Twis, h1,bl1,tl1,alp1, h2,bl2,tl2,alp2;
 Ctub ?  Rmin,Rmax,Dz, Phi1,Phi2, Lx,Ly,Lz,Hx,Hy,Hz;

 If %Shape=='PGON' | %Shape=='PCON'
 { k=%Npar; %Npar+=3*%Nz;  do i=1,%Nz
   { %Par(3*i+k-2)=%zi(i); %Par(3*i+k-1)=%rmn(i); %Par(3*i+k)=%rmx(i);}
 }
*/

#include "Z.h"
#include "TString.h"

//  	Codes of G3 shapes
typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
		PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes_e;
//  	Names of G3 shapes
static const char *NAPES[]= {0,
"BOX ","TRD1","TRD2","TRAP","TUBE","TUBS","CONE","CONS","SPHE","PARA",
"PGON","PCON","ELTU","HYPE","____","____","____","____","____","____"
"____","____","____","____","____","____","____","GTRA","CTUB","____"};

//________________________________________________________________________________
class JMate_t : public Z_t {
public:
  char  Name[20];
  float A; //
  float Z;
  float DENS;
  float RADL;
  float ABSL;
  float NLMAT;
  float User[20];

TString GetTitle(){return TString(Name,20);}

};

//________________________________________________________________________________
class JTmed_t : public Z_t {
public:
  char  Name[20];
  float IMAT;
  float ISVOL;
  float IFIELD;
  float FIELDM;
  float TMAXFD;
  float STEMAX;
  float DEEMAX;
  float EPSIL;
  float STMIN;

TString GetTitle(){return TString(Name,20);}
};  
  
//________________________________________________________________________________
class JDiv_t;
class JPos_t;
class JVolu_t : public Z_t {
public:
  float ISEARC;
  float ISHAPE;
  float NIN;
  float NUMED;
  float NPAR;
  float NATT;
  float PARS[99];
float  *GetPars()	{return (NPAR)? PARS:0;}  
float  *GetAtts()	{return (NATT)? PARS+int(NPAR):0;}  
JDiv_t *GetJDiv()	{return (NIN<0)? (JDiv_t*)Link(-1):0;} 
JPos_t *GetJPos(int in) {return (NIN<0 || in>NIN)? 0:(JPos_t*)Link(in);}
TString GetName(){return TString((char*)&GetID(),4);}
};

//________________________________________________________________________________
class JDiv_t : public Z_t {
public:
  float IAXIS;
  float IVO;
  float NDIV;
  float C0;
  float STEP;
};
//________________________________________________________________________________
class JPos_t : public Z_t {
public:
  float DUMM;
  float IVO;
  float NR;
  float IROT;
  float X[3];
  float KONLY;
  float NPAR;
  float PARS[99];
};
#endif //G3Z_H
