/* StMagF - WALove 12 January 1999  A package for
 the STAR magnet field.  Methods included to read in 
 a file of STAR field values Bz, Br, Bphi, on an evenly
 spaced grid in r, z, phi and to return Bx, By, Bz at
 a given point x,y,z.  Map uses meters and gauss.  The
 Field method takes centimeter input and returns kilogauss.
 A factor input to the ctor is provided to reverse the
 field sign when necessary. A map arg of the ctor chooses
 constant field (map=1) or the interpolation grid (map=2).
*/  
#include "StMagF.h"
#include <stdlib.h>
#include <stdio.h>



static StMagF *gMagfield = 0;
//________________________________________
void type_of_call agufld_(Float_t *x, Float_t *b)
{
  if (!gMagfield) 
    gMagfield = new StMagFCM("Star Full Field",
			     "/afs/rhic/star/packages/dev/StDb/StMagF/bfp112.map",
			     kConMesh,1.0);
  gMagfield->Field(x,b);
}

ClassImp(StMagF)

//________________________________________
StMagF::StMagF(const char *name, const char *title, const EField map, 
		 const Float_t factor)
  : TNamed(name,title)
{
  fMap = map;
  fFactor = factor;
}
//________________________________________
void StMagF::Agufld(Float_t *x, Float_t *b)
{
  b[0]=b[1]=b[2]=0;
  if(!gMagfield){
    if(!fMap){
      printf("Undefined MagF Field called, returning 0\n");
    }
    else if(fMap==kConst){
       gMagfield = new StMagFC("Uniform Field","Fixed", 1.0);
       gMagfield->Field(x,b);
    }
    else if(fMap==kConMesh){
       gMagfield = new StMagFCM(" Star Full Field","/afs/rhic/star/packages/dev/StDb/StMagF/bfp112.map",kConMesh,1.0);
       gMagfield->Field(x,b);
    }
  }
  else gMagfield->Field(x,b);
}
//________________________________________
void StMagF::Field(Float_t *x, Float_t *b)
{
  printf("Undefined MagF Field called, returning 0\n");
  b[0]=b[1]=b[2]=0;
}
      
ClassImp(StMagFC)

//________________________________________
StMagFC::StMagFC(const char *name, const char *title,const Float_t factor)
  : StMagF(name,title,kConst,factor)
{
  printf("Constant Field %s created: map= %d, factor= %f\n",fName.Data(),fMap,fFactor);

}

//________________________________________
void StMagFC::Field(Float_t *x, Float_t *b)
{
  b[0]=b[1]=b[2]=0;
  if(fMap==kConst) {
    if(TMath::Abs(x[2])<280 && x[0]*x[0]+x[1]*x[1] < 240*240) 
      b[2]=5.0;
  } else {
    printf("Invalid field map for constant field %d\n",fMap);
    exit(1);
  }
}
    
ClassImp(StMagFCM)

//________________________________________
StMagFCM::StMagFCM(const char *name, const char *title, const EField map, 
		 const Float_t factor)
  : StMagF(name,title,map,factor)
{
  printf("Constant Mesh Field %s created: map= %d, factor= %f, file= %s\n",
	 fName.Data(),fMap,fFactor,fTitle.Data());
  ReadField();
}

//________________________________________
void StMagFCM::Field(Float_t *x, Float_t *b)
{
  Double_t ratp, ratr, ratz, hip, hir, hiz, ratp1, ratr1, ratz1, 
    bhyhz, bhylz, blyhz, blylz, bhz, blz, xl[3];
  Float_t r, z, phi, phideg, br, bphi, bz, bnorm;
  const Double_t one=1;
  Int_t ip, ir, iz;

  b[0]=b[1]=b[2]=0;
  bnorm=.001*fFactor;  //convert to kilogauss and change sign if necessary.    
  // --- find the position in the grid --- convert input cm to m.
  z = 0.01*x[2];
  r = 0.01*TMath::Sqrt(x[0]*x[0] + x[1]*x[1]);
  phi = TMath::ATan2(x[1],x[0]);
  phideg=phi*180/TMath::Pi();
  if(phideg<0) phideg=phideg+360.0;

  if(fZbeg<=z && z<fZbeg+fZdel*(fZn-1) && r<fRbeg+fRdel*(fRn-1) ) {

    xl[0]=phideg-fPbeg;
    xl[1]=r-fRbeg;
    if (xl[1]<0) xl[1] = 0;
    xl[2]=z-fZbeg;
    
    // --- start with x
    
    hip=xl[0]*fPdeli;
    ratp=hip-int(hip);
    ip=int(hip);
    
    hir=xl[1]*fRdeli;
    ratr=hir-int(hir);
    ir=int(hir);
    
    hiz=xl[2]*fZdeli;
    ratz=hiz-int(hiz);
    iz=int(hiz);
    
    if(fMap==kConMesh) {
      // ... simple interpolation
      ratp1=one-ratp;
      ratr1=one-ratr;
      ratz1=one-ratz;
      bhyhz = Bp(ip  ,ir+1,iz+1)*ratp1+Bp(ip+1,ir+1,iz+1)*ratp;
      bhylz = Bp(ip  ,ir+1,iz  )*ratp1+Bp(ip+1,ir+1,iz  )*ratp;
      blyhz = Bp(ip  ,ir  ,iz+1)*ratp1+Bp(ip+1,ir  ,iz+1)*ratp;
      blylz = Bp(ip  ,ir  ,iz  )*ratp1+Bp(ip+1,ir  ,iz  )*ratp;
      bhz   = blyhz             *ratr1+bhyhz             *ratr;
      blz   = blylz             *ratr1+bhylz             *ratr;
      bphi  = blz               *ratz1+bhz               *ratz;
      //
      bhyhz = Br(ip  ,ir+1,iz+1)*ratp1+Br(ip+1,ir+1,iz+1)*ratp;
      bhylz = Br(ip  ,ir+1,iz  )*ratp1+Br(ip+1,ir+1,iz  )*ratp;
      blyhz = Br(ip  ,ir  ,iz+1)*ratp1+Br(ip+1,ir  ,iz+1)*ratp;
      blylz = Br(ip  ,ir  ,iz  )*ratp1+Br(ip+1,ir  ,iz  )*ratp;
      bhz   = blyhz             *ratr1+bhyhz             *ratr;
      blz   = blylz             *ratr1+bhylz             *ratr;
      br  = blz               *ratz1+bhz               *ratz;
      //
      bhyhz = Bz(ip  ,ir+1,iz+1)*ratp1+Bz(ip+1,ir+1,iz+1)*ratp;
      bhylz = Bz(ip  ,ir+1,iz  )*ratp1+Bz(ip+1,ir+1,iz  )*ratp;
      blyhz = Bz(ip  ,ir  ,iz+1)*ratp1+Bz(ip+1,ir  ,iz+1)*ratp;
      blylz = Bz(ip  ,ir  ,iz  )*ratp1+Bz(ip+1,ir  ,iz  )*ratp;
      bhz   = blyhz             *ratr1+bhyhz             *ratr;
      blz   = blylz             *ratr1+bhylz             *ratr;
      bz  = blz               *ratz1+bhz               *ratz;
      // Convert to Cartesian - Brian Lasiuk says there will be
      // inexpensive methods for this.  Also convert to kilogauss.
      b[0]= bnorm*(br*TMath::Cos(phi) - bphi*TMath::Sin(phi));
      b[1]= bnorm*(br*TMath::Sin(phi) + bphi*TMath::Cos(phi));
      b[2]= bnorm*bz;
      //printf("ratp,ratr,ratz,b[0],b[1],b[2] %f %f %f %f %f %f\n",
      //ratp,ratr,ratz,b[0],b[1],b[2]);
      //
    }
    else {
      printf("Invalid field map id for constant mesh %d\n",fMap);
    }
  }
  else {
      printf("Coordinates out of range\n");
  }
}

//________________________________________
void StMagFCM::ReadField()
{
  FILE *magfile;
  Int_t iphi, ir, iz, ipphi, ipr, ipz;
  Float_t bphi, br, bz;
  printf("Reading Magnetic Field %s from file %s \n",fName.Data(),fTitle.Data());
  magfile=fopen(fTitle.Data(),"r");
  if (magfile) {
    fscanf(magfile," %d %f %f %d %f %f  %d %f %f",&fZn,&fZbeg,&fZdel,&fRn,&fRbeg,&fRdel,&fPn,&fPbeg,&fPdel);
    printf("fPn %d, fRn %d, fZn %d, fPdel %f, fRdel %f, fZdel %f, fPbeg %f, fRbeg %f, fZbeg %f\n",
    	   fPn, fRn, fZn, fPdel, fRdel, fZdel, fPbeg, fRbeg, fZbeg);
    fPdeli=1./fPdel;
    fRdeli=1./fRdel;
    fZdeli=1./fZdel;
    fB = new TVector(3*fPn*fRn*fZn);
    for (iz=0; iz<fZn; iz++) {
      ipz=iz*3*(fPn*fRn);
      for (ir=0; ir<fRn; ir++) {
	ipr=ipz+ir*3*fPn;
	for (iphi=0; iphi<fPn; iphi++) {
	  ipphi=ipr+iphi*3;
	  fscanf(magfile,"%f %f %f",&bz,&br,&bphi);
	  (*fB)(ipphi+2)=bz;
	  (*fB)(ipphi+1)=br;
	  (*fB)(ipphi  )=bphi;
	}
      }
    }
  } else { 
    fprintf(stderr,"File %s not found !\n",fTitle.Data());
    exit(1);
  }
}

  

