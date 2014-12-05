#ifndef VertexT_h
#define VertexT_h

#include <string.h>
#include "TObject.h"
#include "TMath.h"

class VertexT : public TObject
{
private:
   Char_t          beg;
   Double32_t      fVx;
   Double32_t      fVy;
   Double32_t      fVz;
   Int_t           fNtracks;
   Char_t          end;

public:
   VertexT() { Clear(); }
   VertexT(Double32_t *xyz, Int_t n) { SetVertex(xyz); SetNtracks(n); }
   virtual ~VertexT() {Clear();}
   void          Clear(Option_t *option = "") {if (option); memset(&beg, 0, &end - &beg);}
   Double32_t    GetVx() { return fVx; }
   Double32_t    GetVy() { return fVy; }
   Double32_t    GetVz() { return fVz; }
   Int_t         GetNtracks() { return fNtracks; }

   virtual void SetVertex(Double32_t *xyz) { fVx = xyz[0]; fVy = xyz[1]; fVz = xyz[2]; }
   virtual void SetNtracks(Int_t n) { fNtracks = n; }

   ClassDef(VertexT, 1)
};

#endif
