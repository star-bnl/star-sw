#ifndef ROOT_TColoredAxis
#define ROOT_TColoredAxis

#include "TGaxis.h"
#include "TAttFill.h"

class TColoredAxis : public TGaxis, public TAttFill {
 private:
   Int_t     fNColors;    // the number of colors
   Double_t *fLookup;     // Pointer to lookup table must be [fNColros+1] at least
   Int_t     fNlookup;    // size of the fLookup table
 public:
        TColoredAxis();
        TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               Float_t wmin,Float_t wmax,Int_t ndiv=510, Option_t *chopt="",
               Float_t gridlength = 0, Int_t nColors=0);
        TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               Float_t wmin,Float_t wmax, Double_t *wval, Int_t lookupsize
              , Int_t ndiv=510, Option_t *chopt="", Float_t gridlength = 0, Int_t nColors =50);
        TColoredAxis(Float_t xmin,Float_t ymin,Float_t xmax,Float_t ymax,
               const char *funcname, Int_t ndiv=510, Option_t *chopt="",
               Float_t gridlength = 0,Int_t nColors=0);
        virtual ~TColoredAxis(){;}
        void    Paint(Option_t *);
        void    PaintPalette();


 ClassDef(TColoredAxis,1) // Class to draw the colored axis
};

#endif
