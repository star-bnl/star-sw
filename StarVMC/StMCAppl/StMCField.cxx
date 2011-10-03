// $Id: StMCField.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCField
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "StMCField.h"

ClassImp(StMCField)

//_____________________________________________________________________________
StMCField::StMCField(const char *name,const char *tit,Double_t *fld)
  : GCall(name,tit)
{
  fField[0] = 0.;
  fField[1] = 0.;
  fField[2] = 5.;
  if (fld) memcpy(fField,fld,sizeof(fField));
}   
//_____________________________________________________________________________
void StMCField::Field(const Double_t*, Double_t* b)
{ b[0]=fField[0];b[1]=fField[1];b[2]=fField[2];}
//_____________________________________________________________________________
void StMCField::Print(const Option_t*) const
{
  double x[3] = {0.,0.,0.};
  double b[3];
  StMCField *This = (StMCField *)this;
  This->Field(x,b);
  printf("%s(%s)::Field at zero = %g %g %g\n",ClassName(),GetName(),b[0],b[1],b[2]);		
}		
		
		
		
