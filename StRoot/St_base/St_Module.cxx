//*-- Author :    Valery Fine   24/03/98

#include <string.h>
#include "St_Module.h"

ClassImp(St_Module)


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  St_Module                                                                  //
//                                                                            //
//  St_Module is a virtual base class to call the begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/ssd_l/staf_l/STAF-current/pams/pams.html">PAMS - Physics Analysis Modules</A> end_html
//  modules from C++ programs and begin_html <A HREF="http://root.cern.ch">ROOT</A> end_html batch and interactive environments.
//                                                                            //
//  Note: This class uses the "default" parameter term.                       //
//        The value of the default parameter is defined as follows:           //
//                                                                            //
//        "it is either "zero" or                                             //
//         the last value of the "missed" parameters                          //
//                                                                            //
//         Calling St_Module::Call, St_Module::ExecuteModule methods with       //
//         any parameter = 0 means the wrapped STAR module will be called     //
//         with "default" parameter instead.                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

//______________________________________________________________________________
St_Module::St_Module() 
: fN(0), st_Params(0),st_Name(0),st_Type(kUnknown), fIndex(0)
{  // Default ctor;
}
//______________________________________________________________________________
St_Module::St_Module(char *name,void  *f1,void  *f2,void  *f3,void  *f4
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
                            ) 
: fN(0), st_Params(0), st_Name(0), st_Type(kFortran), fIndex(38)
// : TNamed(name)
{}
//______________________________________________________________________________
St_Module::St_Module(        void  *f1,void  *f2,void  *f3,void  *f4
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
                            ) 
: fN(0), st_Params(0), st_Name(0), st_Type(kFortran), fIndex(38)
{
  SetAllParameters( f1, f2, f3, f4, f5, f6, f7, f8, f9,f10,f11,f12
                  ,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24
                  ,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36
                  ,f37,f38
#if 0
                          ,f39,f40,f41,f42,f43,f44,f45,f46,f47,f48
                  ,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60
                  ,f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72
                  ,f73,f74,f75,f76,f77,f78,f79,f80
#endif
                  );
}
//______________________________________________________________________________
St_Module::~St_Module()
{
  if (st_Params) 
  {
    for (Int_t i=0;i<fN;i++) 
    {
      if (st_Params[i]) {
//          delete st_Params[i];
          st_Params[i]=0;
      }
    }
    delete [] st_Params;
    st_Params = 0; 
  }
//*-*
//*-*   Free memory with the name of the entry point
//
  if (st_Name)
  {
    delete st_Name;
    st_Name = 0;
  }
}
//______________________________________________________________________________
void St_Module::SetAllParameters(void  *f1,void  *f2,void  *f3,void  *f4
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
                               ) 
{  
#if 0
  SetParameter(f80);
  SetParameter(f79);
  SetParameter(f78);
  SetParameter(f77);
  SetParameter(f76);
  SetParameter(f75);
  SetParameter(f74);
  SetParameter(f73);
  SetParameter(f72);
  SetParameter(f71);
  SetParameter(f70);
  SetParameter(f69);
  SetParameter(f68);
  SetParameter(f67);
  SetParameter(f66);
  SetParameter(f65);
  SetParameter(f64);
  SetParameter(f63);
  SetParameter(f62);
  SetParameter(f61);
  SetParameter(f60);
  SetParameter(f59);
  SetParameter(f58);
  SetParameter(f57);
  SetParameter(f56);
  SetParameter(f55);
  SetParameter(f54);
  SetParameter(f53);
  SetParameter(f52);
  SetParameter(f51);
  SetParameter(f50);
  SetParameter(f49);
  SetParameter(f48);
  SetParameter(f47);
  SetParameter(f46);
  SetParameter(f45);
  SetParameter(f44);
  SetParameter(f43);
  SetParameter(f42);
  SetParameter(f41);
  SetParameter(f40);
  SetParameter(f39);
#endif
  SetParameter(f38);
  SetParameter(f37);
  SetParameter(f36);
  SetParameter(f35);
  SetParameter(f34);
  SetParameter(f33);
  SetParameter(f32);
  SetParameter(f31);
  SetParameter(f30);
  SetParameter(f29);
  SetParameter(f28);
  SetParameter(f27);
  SetParameter(f26);
  SetParameter(f25);
  SetParameter(f24);
  SetParameter(f23);
  SetParameter(f22);
  SetParameter(f21);
  SetParameter(f20);
  SetParameter(f19);
  SetParameter(f18);
  SetParameter(f17);
  SetParameter(f16);
  SetParameter(f15);
  SetParameter(f14);
  SetParameter(f13);
  SetParameter(f12);
  SetParameter(f11);
  SetParameter(f10);
  SetParameter(f9);
  SetParameter(f8);
  SetParameter(f7);
  SetParameter(f6);
  SetParameter(f5);
  SetParameter(f4);
  SetParameter(f3);
  SetParameter(f2);
  SetParameter(f1);
}

//______________________________________________________________________________
void St_Module::SetParameter(void *f)
{
  if (!fIndex) return;
  if (f != 0 || st_Params !=0 )
  {
    if (!st_Params) {
//*-*
//*-*  Allocate the list of the parameteres
//
       st_Params = new void *[fIndex];
       memset(st_Params,0,sizeof(void *)*fIndex);
       fN = fIndex;
    }
    else if (f !=0 && fIndex > fN)
    {
//*-*
//*-*  Re-allocate the list of the parameteres
//
        void **obj = new void *[fIndex];
//*-*
//*-*  Copy old staf into the new one
//
       for (Int_t i=0;i<fN; i++) obj[i]=st_Params[i];
       delete [] st_Params;
//*-*
//*-*  replace the old point with a new one
//
       st_Params = obj;  
       fN = fIndex;
    }
    if (fIndex <= fN)
        st_Params[fIndex-1] = f;
  }
  fIndex--;
}
//______________________________________________________________________________
Int_t St_Module::CheckParameters(const Char_t *names[])
{
 ////////////////////////////////////////////////////////////////////
 //                                                                //
 // CheckParameters()                                              //
 //                                                                //
 //  Check whether all parameters are defined to call ExecutModule //
 //  properly ?                                                    //
 //                                                                //
 //  Return:                                                       //
 //      0  = Ok, All parameters are "well" defined                //
 //     +n  = the n paramneters were not defined yet.              //
 //                                                                //
 //  To print the list of parameters one may call                  //
 //           St_Module::Print() method                            //
 //                                                                //
 ////////////////////////////////////////////////////////////////////
   Int_t errcode = 0;
   if (st_Params) {
     for (Int_t i=0;i<fN;i++)
       if (!st_Params[i]) { 
//           errcode = St_Module::ExecuteModule();
           errcode++;
           if (errcode == 1) 
               fprintf(stderr, "\n \t ***** Error calling module <%s> *****\n"
                             ,GetName()); 
           Char_t *suffix[4]={"st","nd","d","th"};
           Char_t *title[2] = {"header","table"};
           Int_t  odd = i>>1;
           Int_t indx = (odd+1)%10;
           if ( (10 < odd && odd < 20) || indx > 3 || indx == 0) indx = 4;
           indx--;
           const Char_t *name = names ? names[odd] : "unknown" ;
           fprintf(stderr, "\t %i-%s %s of <%s> has not been defined yet\n"
                   ,odd+1,suffix[indx],title[i&1],name);
       }
   }
   return errcode;
}
//______________________________________________________________________________
Int_t St_Module::ExecuteModule()
{
   Int_t errcode = 0;
   printf(" This \"%s\" module has ", GetName());
   if (st_Params) 
   {
     if (fN == 1)
       printf("only one parameter: ");
     else
       printf("%i parameters: ",fN);
     printf("\n");
     for (Int_t i=0;i<fN;i++)
     {
        if (st_Params[i]) printf(" %i ",*((ULong_t *)st_Params[i]));
        else { 
            errcode++; 
            Char_t *suffix[4]={"st","nd","d","th"};
            Char_t *title[2] = {"header","table"};
            Bool_t istable = i & 1;
            printf("%i %s has not been defined yet\n"
                   ,(i>>1)+1, title[i&1]);
        }

        if (i < fN-1) printf(", ");
     }
     printf("; \n");
   }
   else
    printf(" NO parameters \n");
   return errcode;
}
#if 0
//______________________________________________________________________________
void St_Module::SetEntryName()
{
  st_Name = new char[strlen(GetName()+10];
  strcpy(st_Name,GetName());
  
  if (st_Name) 
  {
    if (st_Type==kFortran)
    {
#ifdef U_CASE
      strupr(st_Name);
# ifdef CERNLIB_MSSTDCALL
      char buftmp[10];
      strcat(st_Name,"@");
      atoi(buftmp,fN*4,10);   
      strcat(st_Name,buftmp);
# endif  
#else if defined(uscope)
      strcat(st_Name,"_");
#endif
     }
  }
  SetTitle(st_Name);
}
#endif
//______________________________________________________________________________
Int_t  St_Module::ExecuteModule(void  *f1,void  *f2,void  *f3,void  *f4,
                              void  *f5,void  *f6,void  *f7,void  *f8,
                              void  *f9,void *f10,void *f11,void *f12,
                              void *f13,void *f14,void *f15,void *f16,
                              void *f17,void *f18,void *f19,void *f20
                             ,void *f21,void *f22,void *f23,void *f24,
                              void *f25,void *f26,void *f27,void *f28,
                              void *f29,void *f30,void *f31,void *f32,
                              void *f33,void *f34,void *f35,void *f36,
                              void *f37,void *f38
#if 0
                                                ,void *f39,void *f40,
                              void *f41,void *f42,void *f43,void *f44,
                              void *f45,void *f46,void *f47,void *f48,
                              void *f49,void *f50,void *f51,void *f52,
                              void *f53,void *f54,void *f55,void *f56,
                              void *f57,void *f58,void *f59,void *f60,
                              void *f61,void *f62,void *f63,void *f64,
                              void *f65,void *f66,void *f67,void *f68,
                              void *f69,void *f70,void *f71,void *f72,
                              void *f73,void *f74,void *f75,void *f76,
                              void *f77,void *f78,void *f79,void *f80
#endif
                              )
{
  fIndex = 38;
  SetAllParameters( f1, f2, f3, f4, f5, f6, f7, f8, f9,f10,f11,f12
                  ,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,
                  f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,
                  f37,f38
#if 0
                                                ,f39,f40,f41,f42,f43,f44,f45,f46,f47,f48,
                  f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60,
                  f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,
                  f73,f74,f75,f76,f77,f78,f79,f80
#endif
                  );
  return ExecuteModule();
}
void St_Module::Streamer(TBuffer &b)
{
};
