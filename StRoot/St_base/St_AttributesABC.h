#ifndef ROOT_St_AttributesABC
#define ROOT_St_AttributesABC

//+SEQ,CopyRight,T=NOINCLUDE.

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_AttributesABC                                                     //
//                                                                      //
// Abstract class to define the Attributes of 3D points                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Rtypes.h"

#ifndef ROOT_Gtypes
#include "Gtypes.h"
#endif

class St_AttributesABC {

public:
        St_AttributesABC(){;}
        virtual ~St_AttributesABC(){;}

        virtual Int_t     GetAttributeI(const Char_t *attribName) const =0;
        virtual Float_t   GetAttributeF(const Char_t *attribName) const =0;
        virtual Double_t  GetAttributeD(const Char_t *attribName) const =0;
        virtual Color_t   GetColorAttribute() const=0;
        virtual Width_t   GetSizeAttribute()  const=0;
        virtual Style_t   GetStyleAttribute() const=0;
        virtual Int_t     SetAttribute(const Char_t *attribName,Int_t    attrib)=0;
        virtual Float_t   SetAttribute(const Char_t *attribName,Float_t  attrib)=0;
        virtual Double_t  SetAttribute(const Char_t *attribName,Double_t attrib)=0;
        virtual Color_t   SetColorAttribute(Color_t )=0;
        virtual Width_t   SetSizeAttribute(Width_t ) =0;
        virtual Style_t   SetStyleAttribute(Style_t) =0;
        virtual Int_t     GetNumberOfAttributes() const  =0;
        virtual Int_t     SetNumberOfAttributes(Int_t n) =0;

        ClassDef(St_AttributesABC,0)  //A 3-D Points

};

#endif
