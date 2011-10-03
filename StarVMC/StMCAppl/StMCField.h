// $Id: StMCField.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
//
// Class StMCField
// ------------------


#ifndef STMC_FIELD_H
#define STMC_FIELD_H

#include "TNamed.h"
#include "GCall.h"

class StMCField : public GCall
{
  public:
    StMCField(const char *name="DefaultField",const char *tit="",Double_t *fld=0);
    virtual ~StMCField(){}    
    // methods
    virtual void Field(const Double_t*, Double_t* b);
    virtual void Print(const Option_t* opt=0) const;
  private:
    // data members
    Double_t fField[3];
    
    ClassDef(StMCField,0) // Extended TParticle
};

#endif //STMC_FIELD_H   
   

