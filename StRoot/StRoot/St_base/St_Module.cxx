//*-- Author :    Valery Fine   24/03/98
// $Id: St_Module.cxx,v 1.18 2003/04/30 20:39:12 perev Exp $

#include <assert.h>
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
//         Calling St_Module::Call, St_Module::ExecuteModule methods with     //
//         any parameter = 0 means the wrapped STAR module will be called     //
//         with "default" parameter instead.                                  //
//                                                                            //
//                                                                            //
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/Module2Root.gif"> </P> End_Html  
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

//______________________________________________________________________________
St_Module::St_Module() 
: fParams(0), fHeaders(0), fName(0), fIndex(0)
{  // Default ctor;
}
//______________________________________________________________________________
St_Module::St_Module(char *, TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *,TTable *,TTable *
                            ,TTable *,TTable *
                            ) 
: fParams(0), fHeaders(0), fName(0), fIndex(38)
// : TNamed(name)
{}
//______________________________________________________________________________
St_Module::St_Module(        TTable *f1,TTable *f2,TTable *f3,TTable *f4
                            ,TTable *f5,TTable *f6,TTable *f7,TTable *f8
                            ,TTable *f9,TTable *f10,TTable *f11,TTable *f12
                            ,TTable *f13,TTable *f14,TTable *f15,TTable *f16
                            ,TTable *f17,TTable *f18,TTable *f19,TTable *f20
                            ,TTable *f21,TTable *f22,TTable *f23,TTable *f24
                            ,TTable *f25,TTable *f26,TTable *f27,TTable *f28
                            ,TTable *f29,TTable *f30,TTable *f31,TTable *f32
                            ,TTable *f33,TTable *f34,TTable *f35,TTable *f36
                            ,TTable *f37,TTable *f38
                            ) 
: fParams(0), fHeaders(0), fName(0), fIndex(38)
{
  SetAllParameters( f1, f2, f3, f4, f5, f6, f7, f8, f9,f10,f11,f12
                  ,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24
                  ,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36
                  ,f37,f38
                  );
}
//______________________________________________________________________________
St_Module::~St_Module()
{
  if (fHeaders) fHeaders->Delete();
  SafeDelete(fHeaders);
  SafeDelete(fParams);     
}
//______________________________________________________________________________
void St_Module::ClearParams()
{
  if (fHeaders) {
     fHeaders->Delete();
     fParams->Clear();
  }
}
//______________________________________________________________________________
void St_Module::SetAllParameters(TTable *f1,TTable *f2,TTable *f3,TTable *f4
                               ,TTable *f5,TTable *f6,TTable *f7,TTable *f8
                               ,TTable *f9,TTable *f10,TTable *f11,TTable *f12
                               ,TTable *f13,TTable *f14,TTable *f15,TTable *f16
                               ,TTable *f17,TTable *f18,TTable *f19,TTable *f20
                               ,TTable *f21,TTable *f22,TTable *f23,TTable *f24
                               ,TTable *f25,TTable *f26,TTable *f27,TTable *f28
                               ,TTable *f29,TTable *f30,TTable *f31,TTable *f32
                               ,TTable *f33,TTable *f34,TTable *f35,TTable *f36
                               ,TTable *f37,TTable *f38
                               ) 
{  
  ClearParams();
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
void St_Module::SetParameter(TTable *f)
{
  if (!fIndex) return;
  if (f) {
    if (!fParams) {
       fParams  = new TObjArray(fIndex);
       fHeaders = new TObjArray(fIndex);
    }
    fParams->AddAt(f,fIndex-1);
    fHeaders->AddAt(new St_table_header(f),fIndex-1);
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
 //     +n  = the n parameters were not defined yet.              //
 //                                                                //
 //  To print the list of parameters one may call                  //
 //           St_Module::Print() method                            //
 //                                                                //
 ////////////////////////////////////////////////////////////////////
   Int_t errcode = 0;
   if (fParams) {
     TObjArray &thisParams =  *fParams;
     Int_t thisSize = thisParams.GetSize();
     for (Int_t i=0;i<thisSize;i++) 
       if (!thisParams[i]) { 
//           errcode = St_Module::ExecuteModule();
           errcode++;
           if (errcode == 1) 
               fprintf(stderr, "\n \t ***** Error calling module <%s> *****\n"
                             ,GetName()); 
           const Char_t *suffix[4]={"st","nd","d","th"};
           Int_t indx = i%10;
           if ( (10 < i && i < 20) || indx > 3 || indx == 0) indx = 4;
           indx--;
           const Char_t *name = names ? names[i] : "unknown" ;
           fprintf(stderr, "\t %i-%s table of <%s> has not been defined yet\n"
                   ,i,suffix[indx],name);
       }
//=====     assert (!errcode);
   }
   return errcode;
}
//______________________________________________________________________________
Int_t St_Module::CheckResults(Int_t res, const Char_t *names[])
{
   Int_t errcode = 0;
   if (fParams) {
     Int_t thisSize = fParams->GetSize();
     for (Int_t i=0;i<thisSize;i++) {
        Bool_t bug = kFALSE;
        // Get header 
        table_head_st *h = GetHeader(i);
        TTable *table = GetTable(i);
        if (table) {
          assert(table->GetTableSize() == h->maxlen 
                && table->GetRowSize() == h->rbytes
                && table == (TTable *)h->dsl_pointer);
          table->SetNRows(h->nok);
          if (h->nok > h->maxlen){
             res = kFALSE; 
             errcode++;
             bug = kTRUE;
          }
	  table->NaN();
          if (errcode && bug) {
          if (errcode == 1) 
                 fprintf(stderr,
                 "\n \t ***** module  <%s>  returned the corrupted table %s *****\n \t * The number of the used rows more (or equal) of the allocated ones *\n"
                               ,GetName(), table->GetName()); 
          const Char_t *suffix[4]={"st","nd","rd","th"};
          Int_t indx = i%10;
          if ( (10 < i && i < 20) || indx > 3 || indx == 0) indx = 4;
          indx--;
          const Char_t *name = names ? names[i] : "unknown" ;
          fprintf(stderr, "\t %i-%s <%s> %s has used %d with %d allocated\n"
          ,i+1,suffix[indx],name, table->GetName(),(int)h->nok,(int)h->maxlen);
        }
     }
///=====     assert (!errcode);
   }
}
   return res;
}

//______________________________________________________________________________
Int_t St_Module::ExecuteModule()
{

   Int_t errcode = 0;
   printf(" This \"%s\" module has ", GetName());
   if (fParams) 
   {
     Int_t thisSize = fParams->GetSize();
     if (thisSize == 1)
       printf("only one parameter: ");
     else
       printf("%i parameters: ",thisSize);
     for (Int_t i=0;i<thisSize;i++)
     {
        if (fParams->At(i)) printf(" %lx ",*((ULong_t *)fParams->At(i)));
        else { 
            errcode++; 
	    //yf            Char_t *suffix[4]={"st","nd","d","th"};
	    //yf            Bool_t istable = i & 1;
            printf("%i parameter has not been defined yet\n"
                   ,i+1);
        }

        if (i < thisSize-1) printf(", ");
     }
     printf("; \n");
///=====     assert (!errcode);
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
#else //VP if defined(uscope)
      strcat(st_Name,"_");
#endif
     }
  }
  SetTitle(st_Name);
}
#endif
//______________________________________________________________________________
Int_t  St_Module::InvokeModule(TTable *f1,TTable *f2,TTable *f3,TTable *f4,
                              TTable *f5,TTable *f6,TTable *f7,TTable *f8,
                              TTable *f9,TTable *f10,TTable *f11,TTable *f12,
                              TTable *f13,TTable *f14,TTable *f15,TTable *f16,
                              TTable *f17,TTable *f18,TTable *f19,TTable *f20
                             ,TTable *f21,TTable *f22,TTable *f23,TTable *f24,
                              TTable *f25,TTable *f26,TTable *f27,TTable *f28,
                              TTable *f29,TTable *f30,TTable *f31,TTable *f32,
                              TTable *f33,TTable *f34,TTable *f35,TTable *f36,
                              TTable *f37,TTable *f38
                              )
{
  fIndex = 38;
  SetAllParameters( f1, f2, f3, f4, f5, f6, f7, f8, f9,f10,f11,f12
                  ,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,
                  f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,
                  f37,f38
                  );
  return ExecuteModule();
}
void St_Module::Streamer(TBuffer &)
{ assert(0);}

//________________________________________________________________________
// $Log: St_Module.cxx,v $
// Revision 1.18  2003/04/30 20:39:12  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.17  2001/07/16 23:58:35  fine
// suppressing the compilation warning
//
// Revision 1.16  2000/09/15 15:12:37  perev
// NaN check added
//
// Revision 1.15  2000/07/30 01:40:12  perev
// StMem class added
//
// Revision 1.14  2000/03/30 05:20:51  fine
// bug fixed
//
// Revision 1.13  2000/03/27 02:19:26  fine
// warning removed
//
// Revision 1.12  2000/03/26 01:59:23  fine
// new version of St_Module. Works for STAF module only
//
// Revision 1.11  2000/03/24 20:35:22  fine
// adjusted to ROOT 2.24. Doesn't work yet. Under development
//
// Revision 1.10  1999/12/21 18:57:13  fine
// compilation warning plus new type for SizeAttribute
//
// Revision 1.9  1999/12/07 22:26:26  fine
// Clean up to remove the compilation warnings
//
// Revision 1.8  1999/06/14 09:45:41  fine
// assert for St_Module (thanks Fisyak)
//
// Revision 1.7  1999/03/11 00:34:44  perev
// St_base in new maker schema
//
// Revision 1.6  1999/02/24 17:10:57  fine
//  St_Table  New and Purge method have been introdiced, some clean up for St_module as well
//
// Revision 1.5  1998/11/25 21:58:33  fisyak
// Cleanup
//
// Revision 1.4  1998/08/25 23:07:24  didenko
// New base with Tree
//
//________________________________________________________________________
