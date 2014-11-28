//*-- Author : Valeri Fine (Faine); E-mail: fine@bnl.gov, fine@mail.cern.ch
//*CMZ : 23/03/98
// Copyright (C) FineSoft, Valery Fine at Brookhaven National Laboratory (fine@bnl.gov)
// $Id: St_Module.h,v 1.11 2003/09/07 03:49:07 perev Exp $
// $Log: St_Module.h,v $
// Revision 1.11  2003/09/07 03:49:07  perev
// gcc 3.2 + WarnOff
//
// Revision 1.10  2001/07/16 23:58:35  fine
// suppressing the compilation warning
//
// Revision 1.9  2000/03/26 01:59:23  fine
// new version of St_Module. Works for STAF module only
//
// Revision 1.8  2000/03/24 20:35:22  fine
// adjusted to ROOT 2.24. Doesn't work yet. Under development
//
// Revision 1.7  1999/12/07 22:26:27  fine
// Clean up to remove the compilation warnings
//
// Revision 1.6  1999/07/28 15:36:33  fisyak
// Resolve clash with TSystem for ELogLevel
//
// Revision 1.5  1999/02/24 17:10:57  fine
//  St_Table  New and Purge method have been introdiced, some clean up for St_module as well
//
// Revision 1.4  1998/11/25 21:58:34  fisyak
// Cleanup
//
// Revision 1.3  1998/08/25 23:07:24  didenko
// New base with Tree
//

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  St_Module                                                            //
//                                                                      //
//  St_Module is a virtual base class to call the begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/ssd_l/staf_l/STAF-current/pams/pams.html">PAMS - Physics Analysis Modules</A> end_html
//  modules from C++ programs and begin_html <A HREF="http://root.cern.ch">ROOT</A> end_html batch and interactive environments.
//                                                                      //
//////////////////////////////////////////////////////////////////////////



#ifndef STAFF_St_Module
#define STAFF_St_Module


#include "Rtypes.h"
#include "St_table_header_Table.h" 
#include "TObjArray.h"

// Copyright (C) FineSoft 1998 Valeri Fine (Faine) E-mail: fine@bnl.gov

/////////////////////////////////////////////////////////////////////
//                                                                 //
//  St_Module                                                      //
//                                                                 //
/////////////////////////////////////////////////////////////////////

//______________________________________________________________________________

//*-- Author :    Valery Fine   24/03/98

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  St_Module                                                            //
//                                                                      //
//  St_Module is a virtual base class to call the begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/ssd_l/staf_l/STAF-current/pams/pams.html">PAMS - Physics Analysis Modules</A> end_html
//  modules from C++ programs and begin_html <A HREF="http://root.cern.ch">ROOT</A> end_html batch and interactive environments.
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifdef __CINT__
#pragma Ccomment on
#endif

class  St_Module {
private:
        TObjArray  *fParams;      // Array of the input tables
        TObjArray  *fHeaders;     // Array of the headers of that tables
        char       *fName;        // Name of the module for the dynamic loading
        Int_t       fIndex;       // The index pof the current element
protected:
                 void ClearParams();
        virtual  void SetParameter(TTable *f);
        virtual  void SetAllParameters(
                             TTable *f1,TTable *f2,TTable *f3,TTable *f4
                            ,TTable *f5,TTable *f6,TTable *f7,TTable *f8
                            ,TTable *f9,TTable *f10,TTable *f11,TTable *f12
                            ,TTable *f13,TTable *f14,TTable *f15,TTable *f16 
                            ,TTable *f17,TTable *f18,TTable *f19,TTable *f20
                            ,TTable *f21,TTable *f22,TTable *f23,TTable *f24
                            ,TTable *f25,TTable *f26,TTable *f27,TTable *f28
                            ,TTable *f29,TTable *f30,TTable *f31,TTable *f32
                            ,TTable *f33,TTable *f34,TTable *f35,TTable *f36
                            ,TTable *f37,TTable *f38
                       );

public:

        St_Module();  // Default ctor;
        St_Module(           TTable *f1,  TTable *f2=0, TTable *f3=0, TTable *f4=0
                            ,TTable *f5=0, TTable *f6=0, TTable *f7=0, TTable *f8=0
                            ,TTable *f9=0, TTable *f10=0, TTable *f11=0, TTable *f12=0
                            ,TTable *f13=0, TTable *f14=0, TTable *f15=0, TTable *f16=0
                            ,TTable *f17=0, TTable *f18=0, TTable *f19=0, TTable *f20=0
                            ,TTable *f21=0, TTable *f22=0, TTable *f23=0, TTable *f24=0
                            ,TTable *f25=0, TTable *f26=0, TTable *f27=0, TTable *f28=0
                            ,TTable *f29=0, TTable *f30=0, TTable *f31=0, TTable *f32=0
                            ,TTable *f33=0, TTable *f34=0, TTable *f35=0, TTable *f36=0
                            ,TTable *f37=0, TTable *f38=0
                ); 

      St_Module(Char_t *name, TTable *f1=0,  TTable *f2=0,  TTable *f3=0,  TTable *f4=0
                             ,TTable *f5=0,  TTable *f6=0,  TTable *f7=0,  TTable *f8=0
                             ,TTable *f9=0,  TTable *f10=0, TTable *f11=0, TTable *f12=0
                             ,TTable *f13=0, TTable *f14=0, TTable *f15=0, TTable *f16=0
                             ,TTable *f17=0, TTable *f18=0, TTable *f19=0, TTable *f20=0
                             ,TTable *f21=0, TTable *f22=0, TTable *f23=0, TTable *f24=0
                             ,TTable *f25=0, TTable *f26=0, TTable *f27=0, TTable *f28=0
                             ,TTable *f29=0, TTable *f30=0, TTable *f31=0, TTable *f32=0
                             ,TTable *f33=0, TTable *f34=0, TTable *f35=0, TTable *f36=0
                             ,TTable *f37=0, TTable *f38=0
               );


  virtual      ~St_Module(); 

  //  virtual void  Call(){Int_t i=ExecuteModule();}
  virtual void  Call(){ExecuteModule();}
  virtual Int_t CheckParameters(const Char_t *names[]=0);
  virtual Int_t CheckResults(Int_t res, const Char_t *names[]=0);
  virtual Int_t ExecuteModule();

  table_head_st *GetHeader(Int_t i) const {return ((St_table_header *)fHeaders->At(i))->GetTable();}
  TTable *GetTable(Int_t i) const {return (TTable *)fParams->At(i);}
  void *GetStruct(Int_t i) const {return ((TTable *)fParams->At(i))->GetArray();}

//VP  virtual Int_t  operator()() { return ExecuteModule(); }
          Int_t  InvokeModule(TTable *f1,    TTable *f2=0,  TTable *f3=0,  TTable *f4=0
                              ,TTable *f5=0,  TTable *f6=0,  TTable *f7=0,  TTable *f8=0
                              ,TTable *f9=0,  TTable *f10=0, TTable *f11=0, TTable *f12=0
                              ,TTable *f13=0, TTable *f14=0, TTable *f15=0, TTable *f16=0
                              ,TTable *f17=0, TTable *f18=0, TTable *f19=0, TTable *f20=0
                              ,TTable *f21=0, TTable *f22=0, TTable *f23=0, TTable *f24=0
                              ,TTable *f25=0, TTable *f26=0, TTable *f27=0, TTable *f28=0
                              ,TTable *f29=0, TTable *f30=0, TTable *f31=0, TTable *f32=0
                              ,TTable *f33=0, TTable *f34=0, TTable *f35=0, TTable *f36=0
                              ,TTable *f37=0, TTable *f38=0
                              );

         virtual const Char_t *GetTitle(){ return "title";}
         virtual const Char_t *GetName() { return "virtual St_Module";}

  ClassDef(St_Module,0)
};
#endif
