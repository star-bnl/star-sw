//Z,+PATCH,//ROOT/HISTPAINTER.
//Z,+KEEP,TAxis3D,T=C++..
//*CMZ :          28/11/99  00.18.25  by  Valery Fine(fine@mail.cern.ch)
//*-- Author :    Valery Fine(fine@bnl.gov)   27/11/99
#ifndef ROOT_TAxis3D
#define ROOT_TAxis3D

// $Id: TAxis3D.h,v 1.1 1999/11/29 19:49:57 fine Exp $ 
//+SEQ,CopyRight,T=NOINCLUDE.
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TAxis3D                                                              //
//                                                                      //
// 3D axice                                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
 
 
#ifndef ROOT_TAxis
//Z,+SEQ,TAxis.
//*KEEP,TAxis.
#include "TAxis.h"
//*KEND.
#endif

#ifndef ROOT_TAttLine
//Z,+SEQ,TAttLine.
//*KEEP,TAttLine.
#include "TAttLine.h"
//*KEND.
#endif
 
#ifndef ROOT_TAttFill
//Z,+SEQ,TAttFill.
//*KEEP,TAttFill.
#include "TAttFill.h"
//*KEND.
#endif
 
#ifndef ROOT_TAttMarker
//Z,+SEQ,TAttMarker.
//*KEEP,TAttMarker.
#include "TAttMarker.h"
//*KEND.
#endif
 
 
class TF1;
class TBrowser;
class TGaxis;
 
class TAxis3D : public TNamed  {
 
protected:
    TAxis       fAxis[3];         //X axis descriptor
    TString     fOption;

 
private:
    Int_t   AxisChoice(Option_t *axis);
    void    Build();
 
protected:
    virtual void    Copy(TObject &hnew);
 
public:
    TAxis3D(){;}
    TAxis3D(Option_t *option);
//    TAxis3D(const Text_t *name,const Text_t *title,Int_t nbinsx,Axis_t xlow,Axis_t xup);
//    TAxis3D(const Text_t *name,const Text_t *title,Int_t nbinsx,Axis_t *xbins);
    virtual ~ TAxis3D(){;}

//    virtual void     Browse(TBrowser *b);

    virtual Int_t    DistancetoPrimitive(Int_t px, Int_t py);
//    virtual void     Draw(Option_t *option="");
    virtual void     ExecuteEvent(Int_t event, Int_t px, Int_t py);
 
    virtual Int_t    GetNdivisions(Option_t *axis="X");
    virtual Color_t  GetAxisColor(Option_t *axis="X");
    virtual Color_t  GetLabelColor(Option_t *axis="X");
    virtual Style_t  GetLabelFont(Option_t *axis="X");
    virtual Float_t  GetLabelOffset(Option_t *axis="X");
    virtual Float_t  GetLabelSize(Option_t *axis="X");
    virtual Float_t  GetTitleOffset(Option_t *axis="X");
    virtual Float_t  GetTickLength(Option_t *axis="X");
   
    virtual void     GetCenter(Axis_t *center){fAxis[0].GetCenter(center);}
 
    virtual void     GetLowEdge(Axis_t *edge){fAxis[0].GetLowEdge(edge);}
 
    virtual Text_t  *GetObjectInfo(Int_t px, Int_t py);
 
    Option_t        *GetOption() const {return fOption.Data();}
 
    virtual TAxis   *GetXaxis() {return &fAxis[0];}
    virtual TAxis   *GetYaxis() {return &fAxis[1];}
    virtual TAxis   *GetZaxis() {return &fAxis[2];}
 
    virtual void     Paint(Option_t *option="");
            void     PaintLegoAxis(TGaxis *axis, Float_t ang);
//    virtual void     Print(Option_t *option="");
    virtual void     SavePrimitive(ofstream &out, Option_t *option);
 
    virtual void     SetAxisColor(Color_t color=1, Option_t *axis="X");
    virtual void     SetAxisRange(Float_t xmin, Float_t xmax, Option_t *axis="X");

 
    virtual void     SetLabelColor(Color_t color=1, Option_t *axis="X");
    virtual void     SetLabelFont(Style_t font=62, Option_t *axis="X");
    virtual void     SetLabelOffset(Float_t offset=0.005, Option_t *axis="X");
    virtual void     SetLabelSize(Float_t size=0.02, Option_t *axis="X");
 
//    virtual void     SetName(const Text_t *name); // *MENU*
    virtual void     SetNdivisions(Int_t n=510, Option_t *axis="X");
    virtual void     SetOption(Option_t *option=" ") {fOption = option;}
    virtual void     SetTickLength(Float_t length=0.02, Option_t *axis="X");
    virtual void     SetTitleOffset(Float_t offset=1, Option_t *axis="X");
    virtual void     SetXTitle(Text_t *title) {fAxis[0].SetTitle(title);}
    virtual void     SetYTitle(Text_t *title) {fAxis[1].SetTitle(title);}
    virtual void     SetZTitle(Text_t *title) {fAxis[2].SetTitle(title);}
    void             UseCurrentStyle();
 
    ClassDef(TAxis3D,1)  //1-Dim histogram base class
};
 
// $Log: TAxis3D.h,v $
// Revision 1.1  1999/11/29 19:49:57  fine
// ROOT class: TAxis3D. To be moved to ROOT later
//
#endif
