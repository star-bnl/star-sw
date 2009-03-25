#include "TProfile2D.h"

class StTProfile2D: public TProfile2D
{
public:
StTProfile2D():TProfile2D(){;}
StTProfile2D(const char* name, const char* title, Int_t nbinsx, Double_t xlow, Double_t xup, Int_t nbinsy, Double_t ylow, Double_t yup, Option_t* option = ""):
  TProfile2D(name,title,nbinsx,xlow,xup,nbinsy,ylow,yup,option){;}

TProfile* ProfileX(const char* name = "_pfx", Int_t firstybin = 1, Int_t lastybin = -1, Option_t* option = "") const;
TProfile* ProfileY(const char* name = "_pfy", Int_t firstxbin = 1, Int_t lastxbin = -1, Option_t* option = "") const;
private:
TProfile *DoProfile(bool onX, const char *name, Int_t firstbin, Int_t lastbin, Option_t *option) const;

ClassDef(StTProfile2D,0) 
};
