//*-- Author : Valeri Fine (Faine); E-mail: fine@bnl.gov, fine@mail.cern.ch
//*CMZ : 23/03/98
// Copyright (C) FineSoft, Valery Fine at Brookhaven National Laboratory (fine@bnl.gov)

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

// Copyright (C) FineSoft 1998 Valeri Fine (Faine) E-mail: fine@bnl.gov

/////////////////////////////////////////////////////////////////////
//                                                                 //
//  St_Module                                                      //
//                                                                 //
//  A special class to call any Fortran subroutine from C/C++      //
//  env. across platforms                                          //
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


class  St_Module {

public:
typedef enum {kUnknown=-1,   // The type of the staf module "unknown"
              kCtype,        // Define the C module
              kCPlusplustype,// Define C++ module
              kFortran       // Define Fortran module
             } EModuleTypes; // Define the original language the present module was written with

typedef enum {kNoWarning     // No message
             ,kError         // print out the severe errors (like zero pointers)
             ,kWarning       // print out the warning too
             ,kIgnore
             } ELogLevel;
private:
	EModuleTypes   st_Type;          // kCtype, kCPlusplustype, kFortran
        char          *st_Name;          // Name of the module for the dynamic loading
        Int_t          fN;               // The length of the array
        Int_t          fIndex;           // The index pof the current element
        ELogLevel      fTraceLogLevel;   // the level of the diagnostic messages (0-3)
protected:

	void        **st_Params;        // The total 
        virtual  void SetParameter(void *f);
        virtual  void SetAllParameters(
                             void  *f1,void  *f2,void  *f3,void  *f4
                            ,void  *f5,void  *f6,void  *f7,void  *f8
                            ,void  *f9,void *f10,void *f11,void *f12
                            ,void *f13,void *f14,void *f15,void *f16 
                            ,void *f17,void *f18,void *f19,void *f20
                            ,void *f21,void *f22,void *f23,void *f24
                            ,void *f25,void *f26,void *f27,void *f28
                            ,void *f29,void *f30,void *f31,void *f32
                            ,void *f33,void *f34,void *f35,void *f36
                            ,void *f37,void *f38
#if 0
                                                ,void *f39,void *f40
                            ,void *f41,void *f42,void *f43,void *f44
                            ,void *f45,void *f46,void *f47,void *f48
                            ,void *f49,void *f50,void *f51,void *f52
                            ,void *f53,void *f54,void *f55,void *f56
                            ,void *f57,void *f58,void *f59,void *f60
                            ,void *f61,void *f62,void *f63,void *f64
                            ,void *f65,void *f66,void *f67,void *f68
                            ,void *f69,void *f70,void *f71,void *f72
                            ,void *f73,void *f74,void *f75,void *f76
                            ,void *f77,void *f78,void *f79,void *f80
#endif
                       );

public:

        St_Module();  // Default ctor;
        St_Module(           void  *f1,  void  *f2=0, void  *f3=0, void  *f4=0
                            ,void  *f5=0, void  *f6=0, void  *f7=0, void  *f8=0
                            ,void  *f9=0, void *f10=0, void *f11=0, void *f12=0
                            ,void *f13=0, void *f14=0, void *f15=0, void *f16=0
                            ,void *f17=0, void *f18=0, void *f19=0, void *f20=0
                            ,void *f21=0, void *f22=0, void *f23=0, void *f24=0
                            ,void *f25=0, void *f26=0, void *f27=0, void *f28=0
                            ,void *f29=0, void *f30=0, void *f31=0, void *f32=0
                            ,void *f33=0, void *f34=0, void *f35=0, void *f36=0
                            ,void *f37=0, void *f38=0
#if 0
                                                , void *f39=0, void *f40=0
                            ,void *f41=0, void *f42=0, void *f43=0, void *f44=0
                            ,void *f45=0, void *f46=0, void *f47=0, void *f48=0
                            ,void *f49=0, void *f50=0, void *f51=0, void *f52=0
                            ,void *f53=0, void *f54=0, void *f55=0, void *f56=0
                            ,void *f57=0, void *f58=0, void *f59=0, void *f60=0
                            ,void *f61=0, void *f62=0, void *f63=0, void *f64=0
                            ,void *f65=0, void *f66=0, void *f67=0, void *f68=0
                            ,void *f69=0, void *f70=0, void *f71=0, void *f72=0
                            ,void *f73=0, void *f74=0, void *f75=0, void *f76=0
                            ,void *f77=0, void *f78=0, void *f79=0, void *f80=0
#endif
                ); 

      St_Module(Char_t *name,void  *f1=0, void  *f2=0, void  *f3=0, void  *f4=0
                             ,void  *f5=0, void  *f6=0, void  *f7=0, void  *f8=0
                             ,void  *f9=0, void *f10=0, void *f11=0, void *f12=0
                             ,void *f13=0, void *f14=0, void *f15=0, void *f16=0
                             ,void *f17=0, void *f18=0, void *f19=0, void *f20=0
                             ,void *f21=0, void *f22=0, void *f23=0, void *f24=0
                             ,void *f25=0, void *f26=0, void *f27=0, void *f28=0
                             ,void *f29=0, void *f30=0, void *f31=0, void *f32=0
                             ,void *f33=0, void *f34=0, void *f35=0, void *f36=0
                             ,void *f37=0, void *f38=0
#if 0
                                                , void *f39=0, void *f40=0
                             ,void *f41=0, void *f42=0, void *f43=0, void *f44=0
                             ,void *f45=0, void *f46=0, void *f47=0, void *f48=0
                             ,void *f49=0, void *f50=0, void *f51=0, void *f52=0
                             ,void *f53=0, void *f54=0, void *f55=0, void *f56=0
                             ,void *f57=0, void *f58=0, void *f59=0, void *f60=0
                             ,void *f61=0, void *f62=0, void *f63=0, void *f64=0
                             ,void *f65=0, void *f66=0, void *f67=0, void *f68=0
                             ,void *f69=0, void *f70=0, void *f71=0, void *f72=0
                             ,void *f73=0, void *f74=0, void *f75=0, void *f76=0
                             ,void *f77=0, void *f78=0, void *f79=0, void *f80=0 
#endif
               );


  virtual      ~St_Module(); 

  virtual void  Call(){Int_t i=ExecuteModule();}
  virtual Int_t CheckParameters(const Char_t *names[]=0);
  virtual Int_t ExecuteModule();
  virtual const Char_t *GetEntryName(){return GetTitle();}  // Return the mangled name of the Fotran subroutines to be loaded dymanically
  virtual void *GetParams(Int_t idx=0){ return (-1 < idx && idx<fN ) ? (void *)st_Params[idx] : (void *)-1;}   // The total 

  virtual void  SetType(EModuleTypes type=kFortran){st_Type = type;}
//  virtual void &operator[](Int_t i){return st_Params[i];}

  virtual Int_t  operator()() { return ExecuteModule(); }
  virtual Int_t  ExecuteModule(void  *f1,   void  *f2=0, void  *f3=0, void  *f4=0
                              ,void  *f5=0, void  *f6=0, void  *f7=0, void  *f8=0
                              ,void  *f9=0, void *f10=0, void *f11=0, void *f12=0
                              ,void *f13=0, void *f14=0, void *f15=0, void *f16=0
                              ,void *f17=0, void *f18=0, void *f19=0, void *f20=0
                              ,void *f21=0, void *f22=0, void *f23=0, void *f24=0
                              ,void *f25=0, void *f26=0, void *f27=0, void *f28=0
                              ,void *f29=0, void *f30=0, void *f31=0, void *f32=0
                              ,void *f33=0, void *f34=0, void *f35=0, void *f36=0
                              ,void *f37=0, void *f38=0
#if 0
                                                , void *f39=0, void *f40=0
                              ,void *f41=0, void *f42=0, void *f43=0, void *f44=0
                              ,void *f45=0, void *f46=0, void *f47=0, void *f48=0
                              ,void *f49=0, void *f50=0, void *f51=0, void *f52=0
                              ,void *f53=0, void *f54=0, void *f55=0, void *f56=0
                              ,void *f57=0, void *f58=0, void *f59=0, void *f60=0
                              ,void *f61=0, void *f62=0, void *f63=0, void *f64=0
                              ,void *f65=0, void *f66=0, void *f67=0, void *f68=0
                              ,void *f69=0, void *f70=0, void *f71=0, void *f72=0
                              ,void *f73=0, void *f74=0, void *f75=0, void *f76=0
                              ,void *f77=0, void *f78=0, void *f79=0, void *f80=0
#endif
                              );

  virtual Int_t  operator()   (void  *f1,   void  *f2=0, void  *f3=0, void  *f4=0
                              ,void  *f5=0, void  *f6=0, void  *f7=0, void  *f8=0
                              ,void  *f9=0, void *f10=0, void *f11=0, void *f12=0
                              ,void *f13=0, void *f14=0, void *f15=0, void *f16=0
                              ,void *f17=0, void *f18=0, void *f19=0, void *f20=0
                              ,void *f21=0, void *f22=0, void *f23=0, void *f24=0
                              ,void *f25=0, void *f26=0, void *f27=0, void *f28=0
                              ,void *f29=0, void *f30=0, void *f31=0, void *f32=0
                              ,void *f33=0, void *f34=0, void *f35=0, void *f36=0
                              ,void *f37=0, void *f38=0
#if 0
                                                , void *f39=0, void *f40=0
                              ,void *f41=0, void *f42=0, void *f43=0, void *f44=0
                              ,void *f45=0, void *f46=0, void *f47=0, void *f48=0
                              ,void *f49=0, void *f50=0, void *f51=0, void *f52=0
                              ,void *f53=0, void *f54=0, void *f55=0, void *f56=0
                              ,void *f57=0, void *f58=0, void *f59=0, void *f60=0
                              ,void *f61=0, void *f62=0, void *f63=0, void *f64=0
                              ,void *f65=0, void *f66=0, void *f67=0, void *f68=0
                              ,void *f69=0, void *f70=0, void *f71=0, void *f72=0
                              ,void *f73=0, void *f74=0, void *f75=0, void *f76=0
                              ,void *f77=0, void *f78=0, void *f79=0, void *f80=0
#endif
                              )
                           { return ExecuteModule(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f13,f14
                                                 ,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24
                                                 ,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34
                                                 ,f35,f36,f37,f38
#if 0
                                                ,f39,f40,f41,f42,f43,f44
                                                 ,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54
                                                 ,f55,f56,f57,f58,f59,f60,f61,f62,f63,f64
                                                 ,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74
                                                 ,f75,f76,f77,f78,f79,f80
#endif
                                                 );
                           }

         virtual const Char_t *GetTitle(){ return "title";}
         virtual const Char_t *GetName() { return "virtual St_Module";}

  ClassDef(St_Module,0)
};


#endif
