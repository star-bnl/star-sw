//*CMZ :          12/07/98  18.27.27  by  Valery Fine(fine@mail.cern.ch)
// $Id: St_Table.cxx,v 1.92 2000/01/21 02:09:52 fine Exp $ 
// 
//*-- Author :    Valery Fine(fine@mail.cern.ch)   03/07/98
// Copyright (C) Valery Fine (Valeri Faine) 1998. All right reserved
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_Table                                                             //
//                                                                      //
// Wraps the array of the STAF C-structures (one STAF Table per element)//
//                                                                      //
// Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/st2tab.gif"> </P> End_Html //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

 
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>

#ifdef WIN32
# include <float.h>
#endif

#include "TROOT.h"
#include "TBaseClass.h"
#include "TSystem.h"
#include "TBuffer.h"
#include "TMath.h"
#include "TClass.h"
#include "TString.h"
#include "Api.h"
#include "TRealData.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "St_Table.h"
#include "St_TableElementDescriptor.h"
#include "St_tableDescriptor.h"
#include "StBufferAbc.h"

#include "TGaxis.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TPad.h"
#include "TEventList.h"
#include "TPolyMarker.h"
#include "TView.h"
#include "TGaxis.h"
#include "TPolyMarker3D.h"

TH1 *gCurrentTableHist = 0;

static   Int_t         fNbins[4] = {100,100,100,100};     //Number of bins per dimension
static   Float_t       fVmin[4]  = {0,0,0,0};             //Minima of varexp columns
static   Float_t       fVmax[4]  = {20,20,20,20};         //Maxima of varexp columns

//______________________________________________________________________________
void *ReAllocate(table_head_st *header, Int_t newsize) 
{
  //
  // header - "plain" C interface to re-allocate the STAF table
  //          "this"  pointer is  supplied indirectly via 
  //          header->dsl_pointer member
  //
  // newsize - is a new size of the STAF table. 
  //           If it is smaller is the old one then nothing happens
  //
 if (header && newsize) 
   return ((St_Table *)header->dsl_pointer)->ReAllocate(newsize);
 else 
   return 0;
}


//______________________________________________________________________________
static void ArrayLayout(Int_t *layout,Int_t *size, Int_t dim)
{
  //
  // ArrayLayout - calculates the array layout recursively
  //
  // Input:
  // -----
  // dim   - dimension of the targeted array
  // size  - the max index for each dimension
  //
  // Output:
  // ------
  // layout - the "start index" for each dimension of an array
  //

  if (dim && layout && size) {
    if (++layout[dim-1] >= size[dim-1]) {
        layout[dim-1] = 0;
        dim--;
        ArrayLayout(layout,size, dim);
    }
  }
}

//______________________________________________________________________________
static void AsString(void *buf, const char *name, Int_t width=0)
{
   if (!strcmp("unsigned int", name))
      cout << setw(width) << *(unsigned int *)buf;
   else if (!strcmp("int", name))
      cout <<  setw(width) <<  *(int *)buf;
   else if (!strcmp("unsigned long", name))
      cout <<  setw(width) <<  *(unsigned long *)buf;
   else if (!strcmp("long", name))
      cout <<  setw(width) <<  *(long *)buf;
   else if (!strcmp("unsigned short", name))
      cout <<  setw(width) <<  hex << *(unsigned short *)buf;
   else if (!strcmp("short", name))
      cout <<  setw(width) << *(short *)buf;
   else if (!strcmp("unsigned char", name))
      cout <<  setw(width) <<  *(unsigned char *)buf;
   else if (!strcmp("char", name))
      cout <<   setw(width) << *(char *)buf;
   else if (!strcmp("float", name))
      cout <<   setw(width) << setprecision(width-3) << *(float *)buf;
   else if (!strcmp("double", name))
      cout <<   setw(width) << setprecision(width-3) << *(double *)buf;
}


ClassImp(St_Table)
 

//______________________________________________________________________________
St_tableDescriptor *St_Table::GetTableDescriptors() const {
    return new St_tableDescriptor(this);
}

//______________________________________________________________________________
const void *St_Table::At(Int_t i) const
{
   if (!BoundsOk("St_Table::At", i))
      i = 0;
   return (const void *)(s_Table+i*(*s_Size));
}

//______________________________________________________________________________
TH1  *St_Table::Draw(TCut varexp, TCut selection, Option_t *option, Int_t nentries, Int_t firstentry)
{
//*-*-*-*-*-*-*-*-*-*-*Draw expression varexp for specified entries-*-*-*-*-*
//*-*                  ===========================================
//
//   This function accepts TCut objects as arguments.
//   Useful to use the string operator +
//         example:
//            table.Draw("x",cut1+cut2+cut3);
//
//   TCutG object with "CUTG" name can be created via the graphics editor. 
//
 
   return St_Table::Draw(varexp.GetTitle(), selection.GetTitle(), option, nentries, firstentry);
}

//______________________________________________________________________________
TH1 *St_Table::Draw(const Text_t *varexp00, const Text_t *selection, Option_t *option,Int_t nentries, Int_t firstentry)
{
//*-*-*-*-*-*-*-*-*-*-*Draw expression varexp for specified entries-*-*-*-*-*
//*-*                  ===========================================
//
//  varexp is an expression of the general form e1:e2:e3
//    where e1,etc is a C++ expression referencing a combination of the St_Table columns
//  Example:
//     varexp = x     simplest case: draw a 1-Dim distribution of column named x
//            = sqrt(x)            : draw distribution of sqrt(x)
//            = x*y/z
//            = y:sqrt(x) 2-Dim dsitribution of y versus sqrt(x)
//            = phep[0]:sqrt(phep[3]) 2-Dim dsitribution of phep[0] versus sqrt(phep[3])
//  Note that the variables e1, e2 or e3 may contain a boolean expression as well.
//  example, if e1= x*(y<0), the value histogrammed will be x if y<0
//  and will be 0 otherwise.
//
//  selection is a C++ expression with a combination of the columns.
//  The value corresponding to the selection expression is used as a weight
//  to fill the histogram.
//  If the expression includes only boolean operations, the result
//  is 0 or 1. If the result is 0, the histogram is not filled.
//  In general, the expression may be of the form:
//      value*(boolean expression)
//  if boolean expression is true, the histogram is filled with
//  a weight = value.
//  Examples:
//      selection1 = "x<y && sqrt(z)>3.2"
//      selection2 = "(x+y)*(sqrt(z)>3.2"
//      selection3 = "signal*(log(signal)>1.2)"
//  selection1 returns a weigth = 0 or 1
//  selection2 returns a weight = x+y if sqrt(z)>3.2
//             returns a weight = 0 otherwise.
//  selection3 returns a weight = signal if log(signal)>1.2
//
//  option is the drawing option
//      see TH1::Draw for the list of all drawing options.
//      If option contains the string "goff", no graphics is generated.
//
//  nentries is the number of entries to process (default is all)
//  first is the first entry to process (default is 0)
//
//     Saving the result of Draw to an histogram
//     =========================================
//  By default the temporary histogram created is called htemp.
//  If varexp0 contains >>hnew (following the variable(s) name(s),
//  the new histogram created is called hnew and it is kept in the current
//  directory.
//  Example:
//    tree.Draw("sqrt(x)>>hsqrt","y>0")
//    will draw sqrt(x) and save the histogram as "hsqrt" in the current
//    directory.
//
//  By default, the specified histogram is reset.
//  To continue to append data to an existing histogram, use "+" in front
//  of the histogram name;
//    table.Draw("sqrt(x)>>+hsqrt","y>0")
//      will not reset hsqrt, but will continue filling.
//
//     Making a Profile histogram
//     ==========================
//  In case of a 2-Dim expression, one can generate a TProfile histogram
//  instead of a TH2F histogram by specyfying option=prof or option=profs.
//  The option=prof is automatically selected in case of y:x>>pf
//  where pf is an existing TProfile histogram.
//
//     Saving the result of Draw to a TEventList
//     =========================================
//  St_Table::Draw can be used to fill a TEventList object (list of entry numbers)
//  instead of histogramming one variable.
//  If varexp0 has the form >>elist , a TEventList object named "elist"
//  is created in the current directory. elist will contain the list
//  of entry numbers satisfying the current selection.
//  Example:
//    tree.Draw(">>yplus","y>0")
//    will create a TEventList object named "yplus" in the current directory.
//    In an interactive session, one can type (after St_Table::Draw)
//       yplus.Print("all")
//    to print the list of entry numbers in the list.
//
//  By default, the specified entry list is reset.
//  To continue to append data to an existing list, use "+" in front
//  of the list name;
//    table.Draw(">>+yplus","y>0")
//      will not reset yplus, but will enter the selected entries at the end
//      of the existing list.
//

   if (GetNRows() == 0 || varexp00 == 0 || varexp00[0]==0) return 0;
   TString  opt;
   Text_t *hdefault = (char *)"htemp";
   Text_t *varexp;
   Int_t i,j,hkeep, action;
   opt = option;
   opt.ToLower();
   Text_t *varexp0 = StrDup(varexp00);
   Text_t *hname = strstr(varexp0,">>");
   TH1 *oldh1 = 0;
   TEventList *elist = 0;
   Bool_t profile = kFALSE;
 
   gCurrentTableHist = 0;
   if (hname) {
     *hname  = 0;
      hname += 2;
      hkeep  = 1;
      i = strcspn(varexp0,">>");
      varexp = new char[i+1];
      varexp[0] = 0; //necessary if i=0
      Bool_t hnameplus = kFALSE;
      while (*hname == ' ') hname++;
      if (*hname == '+') {
         hnameplus = kTRUE;
         hname++;
         while (*hname == ' ') hname++;
         j = strlen(hname)-1;
         while (j) {
            if (hname[j] != ' ') break;
            hname[j] = 0;
            j--;
         }
      }
      if (i) {
         strncpy(varexp,varexp0,i); varexp[i]=0;
         oldh1 = (TH1*)gDirectory->Get(hname);
         if (oldh1 && !hnameplus) oldh1->Reset();
      } else {
         elist = (TEventList*)gDirectory->Get(hname);
         if (!elist) {
            elist = new TEventList(hname,selection,1000,0);
         }
         if (elist && !hnameplus) elist->Reset();
      }
   } 
  // Look for colons
  const Char_t *expressions[] ={varexp0,0,0,0,selection};
  Int_t maxExpressions = sizeof(expressions)/sizeof(Char_t *);
  Char_t *nextColon    = varexp0;
  Int_t colIndex       = 1;
  while ((nextColon = strchr(nextColon,':')) && ( colIndex < maxExpressions - 1 ) ) {
    *nextColon = 0;
     nextColon++;
     expressions[colIndex] = nextColon;
     colIndex++;    
  }

  expressions[colIndex] = selection;

  if (!hname) {
      hname  = hdefault;
      hkeep  = 0;
      varexp = (char*)varexp0;
      if (gDirectory) {
         oldh1 = (TH1*)gDirectory->Get(hname);
         if (oldh1 ) { oldh1->Delete(); oldh1 = 0;}
      }
   }
//--------------------------------------------------
    printf(" Draw %s for <%s>\n", varexp00, selection);
    Char_t *exprFileName = MakeExpression(expressions,colIndex+1);
    if (!exprFileName) return 0;

//--------------------------------------------------
//   if (!fVar1 && !elist) return 0;
 
//*-*- In case oldh1 exists, check dimensionality
   Int_t dimension = colIndex;

   TString title = expressions[0];
   for (i=1;i<colIndex;i++) {
     title += ":";
     title += expressions[i];     
   }
   Int_t nsel = strlen(selection);
   if (nsel > 1) {
      if (nsel < 80-title.Length()) {
        title += "{"; 
        title += selection; 
        title += "}"; 
      }
      else
        title += "{...}"; 
   }

   const Char_t *htitle = title.Data();

   if (oldh1) {
      Int_t mustdelete = 0;
      if (oldh1->InheritsFrom("TProfile")) profile = kTRUE;
      if (opt.Contains("prof")) {
         if (!profile) mustdelete = 1;
      } else {
         if (oldh1->GetDimension() != dimension) mustdelete = 1;
      }
      if (mustdelete) {
         Warning("Draw","Deleting old histogram with different dimensions");
         delete oldh1; oldh1 = 0;
      }
   }
//*-*- Create a default canvas if none exists
   if (!gPad && !opt.Contains("goff") && dimension > 0) {
      if (!gROOT->GetMakeDefCanvas()) return 0;
      (gROOT->GetMakeDefCanvas())();
   }
#if 0
   Int_t         fNbins[4] = {100,100,100,100};     //Number of bins per dimension
   Float_t       fVmin[4]  = {0,0,0,0};             //Minima of varexp columns
   Float_t       fVmax[4]  = {20,20,20,20};         //Maxima of varexp columns
#endif
//*-*- 1-D distribution
   if (dimension == 1) {
      action = 1;
      if (!oldh1) {
         fNbins[0] = 100;
         if (gPad && opt.Contains("same")) {
             TH1 *oldhtemp = (TH1*)gPad->GetPrimitive(hdefault);
             if (oldhtemp) {
                fNbins[0] = oldhtemp->GetXaxis()->GetNbins();
                fVmin[0]  = oldhtemp->GetXaxis()->GetXmin();
                fVmax[0]  = oldhtemp->GetXaxis()->GetXmax();
             } else {
                fVmin[0]  = gPad->GetUxmin();
                fVmax[0]  = gPad->GetUxmax();
             }
         } else {
             action = -1;
         }
      }
      TH1F *h1;
      if (oldh1) {
         h1 = (TH1F*)oldh1;
         fNbins[0] = h1->GetXaxis()->GetNbins();  // for proofserv
      } else {
         h1 = new TH1F(hname,htitle,fNbins[0],fVmin[0],fVmax[0]);
//         h1->SetLineColor(GetLineColor());
//         h1->SetLineWidth(GetLineWidth());
//         h1->SetLineStyle(GetLineStyle());
//         h1->SetFillColor(GetFillColor());
//         h1->SetFillStyle(GetFillStyle());
//         h1->SetMarkerStyle(GetMarkerStyle());
//         h1->SetMarkerColor(GetMarkerColor());
//         h1->SetMarkerSize(GetMarkerSize());
         if (!hkeep) {
            h1->SetBit(kCanDelete);
            h1->SetDirectory(0);
         }
         if (opt.Length() && opt[0] == 'e') h1->Sumw2();
      }

      EntryLoop(exprFileName,action, h1, nentries, firstentry, option);
 
//      if (!fDraw && !opt.Contains("goff")) h1->Draw(option);
        if (!opt.Contains("goff")) h1->Draw(option);
 
//*-*- 2-D distribution
   } else if (dimension == 2) {
      action = 2;
      if (!opt.Contains("same") && gPad)  gPad->Clear();
      if (!oldh1 || !opt.Contains("same")) {
         fNbins[0] = 40;
         fNbins[1] = 40;
         if (opt.Contains("prof")) fNbins[1] = 100;
         if (opt.Contains("same")) {
             TH1 *oldhtemp = (TH1*)gPad->GetPrimitive(hdefault);
             if (oldhtemp) {
                fNbins[1] = oldhtemp->GetXaxis()->GetNbins();
                fVmin[1]  = oldhtemp->GetXaxis()->GetXmin();
                fVmax[1]  = oldhtemp->GetXaxis()->GetXmax();
                fNbins[0] = oldhtemp->GetYaxis()->GetNbins();
                fVmin[0]  = oldhtemp->GetYaxis()->GetXmin();
                fVmax[0]  = oldhtemp->GetYaxis()->GetXmax();
             } else {
                fNbins[1] = 40;
                fVmin[1]  = gPad->GetUxmin();
                fVmax[1]  = gPad->GetUxmax();
                fNbins[0] = 40;
                fVmin[0]  = gPad->GetUymin();
                fVmax[0]  = gPad->GetUymax();
             }
         } else {
             action = -2;
         }
      }
      if (profile || opt.Contains("prof")) {
         TProfile *hp;
         if (oldh1) {
            action = 4;
            hp = (TProfile*)oldh1;
         } else {
            if (action < 0) action = -4;
            if (opt.Contains("profs"))
               hp = new TProfile(hname,htitle,fNbins[1],fVmin[1], fVmax[1],"s");
            else
               hp = new TProfile(hname,htitle,fNbins[1],fVmin[1], fVmax[1],"");
            if (!hkeep) {
               hp->SetBit(kCanDelete);
               hp->SetDirectory(0);
            }
#if 0
            hp->SetLineColor(GetLineColor());
            hp->SetLineWidth(GetLineWidth());
            hp->SetLineStyle(GetLineStyle());
            hp->SetFillColor(GetFillColor());
            hp->SetFillStyle(GetFillStyle());
            hp->SetMarkerStyle(GetMarkerStyle());
            hp->SetMarkerColor(GetMarkerColor());
            hp->SetMarkerSize(GetMarkerSize());
#endif
         }
 
         EntryLoop(exprFileName,action,hp,nentries, firstentry, option);
 
         if (!opt.Contains("goff")) hp->Draw(option);
      } else {
         TH2F *h2;
         if (oldh1) {
            h2 = (TH2F*)oldh1;
         } else {
            h2 = new TH2F(hname,htitle,fNbins[1],fVmin[1], fVmax[1], fNbins[0], fVmin[0], fVmax[0]);
#if 0
            h2->SetLineColor(GetLineColor());
            h2->SetFillColor(GetFillColor());
            h2->SetFillStyle(GetFillStyle());
            h2->SetMarkerStyle(GetMarkerStyle());
            h2->SetMarkerColor(GetMarkerColor());
            h2->SetMarkerSize(GetMarkerSize());
#endif
            if (!hkeep) {
               const Int_t kNoStats = BIT(9);
               h2->SetBit(kCanDelete);
               h2->SetBit(kNoStats);
               h2->SetDirectory(0);
            }
         }
         Int_t noscat = strlen(option);
         if (opt.Contains("same")) noscat -= 4;
         if (noscat) {
            EntryLoop(exprFileName,action,h2,nentries, firstentry, option);
 //           if (!fDraw && !opt.Contains("goff")) h2->Draw(option);
            if (!opt.Contains("goff")) h2->Draw(option);
         } else {
            action = 12;
            if (!oldh1 && !opt.Contains("same")) action = -12;
            EntryLoop(exprFileName,action,h2,nentries, firstentry, option);
//            if (oldh1 && !fDraw && !opt.Contains("goff")) h2->Draw(option);
            if (oldh1 && !opt.Contains("goff")) h2->Draw(option);
         }
      }
 
//*-*- 3-D distribution
   } else if (dimension == 3) {
      action = 13;
      if (!opt.Contains("same")) action = -13;
      EntryLoop(exprFileName,action,0,nentries, firstentry, option);
 
//*-* an Event List
   } else {
      action = 5;
//      Int_t oldEstimate = fEstimate;
//      SetEstimate(1);
      EntryLoop(exprFileName,action,elist,nentries, firstentry, option);
//      SetEstimate(oldEstimate);
   }
  if (exprFileName) delete [] exprFileName;
  if (hkeep) delete [] varexp;
  return gCurrentTableHist;
}
//______________________________________________________________________________
static void FindGoodLimits(Int_t nbins, Int_t &newbins, Float_t &xmin, Float_t &xmax)
{
//*-*-*-*-*-*-*-*-*Find reasonable bin values*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*              ==========================
//*-*  This mathod is a stright copy of void TTree::FindGoodLimits method
//*-*

   static TGaxis gaxis_tree;
   Float_t binlow,binhigh,binwidth;
   Int_t n;
   Float_t dx = 0.1*(xmax-xmin);
   Float_t umin = xmin - dx;
   Float_t umax = xmax + dx;
   if (umin < 0 && xmin >= 0) umin = 0;
   if (umax > 0 && xmax <= 0) umax = 0;
 
   gaxis_tree.Optimize(umin,umax,nbins,binlow,binhigh,n,binwidth);
 
   if (binwidth <= 0 || binwidth > 1.e+39) {
      xmin = -1;
      xmax = 1;
   } else {
      xmin    = binlow;
      xmax    = binhigh;
   }
 
   newbins = nbins;
}
 
//______________________________________________________________________________
Bool_t St_Table::EntryLoop(const Char_t *exprFileName,Int_t &action, TObject *obj
                          ,Int_t nentries, Int_t firstentry, Option_t *option)
{
 //
 // EntryLoop creates a CINT bytecode to evaluate the given expressions for
 // all table rows in loop and fill the appropriated histograms.
 //
 // Solution for Byte code 
 // From: Masaharu Goto <MXJ02154@nifty.ne.jp>
 // To: <fine@bnl.gov>
 // Cc: <rootdev@hpsalo.cern.ch>
 // Sent: 13-th august 1999 year  23:01
 //
 //  action =  1  Fill 1-D histogram obj
 //         =  2  Fill 2-D histogram obj
 //         =  3  Fill 3-D histogram obj
 //         =  4  Fill Profile histogram obj
 //         =  5  Fill a TEventlist
 //         = 11  Estimate Limits
 //         = 12  Fill 2-D PolyMarker obj
 //         = 13  Fill 3-D PolyMarker obj
 //  action < 0   Evaluate Limits for case abs(action)
 //
 //  Load file
  Float_t rmin[3],rmax[3];
  switch(G__loadfile((Char_t *)exprFileName)) {
  case G__LOADFILE_SUCCESS:
  case G__LOADFILE_DUPLICATE:
    break;
  default:
    fprintf(stderr,"Error: loading file %s\n",exprFileName);
    G__unloadfile((Char_t *)exprFileName);
    return kFALSE; // can not load file
  }

  // Float_t  Selection(Float_t *results[], void *address[])
  const Char_t *funcName = "SelectionQWERTY";  
#define BYTECODE
#ifdef BYTECODE
  const Char_t *argtypes = "Float_t *,float **";
  long offset;
  G__ClassInfo globals;
  G__MethodInfo func = globals.GetMethod(funcName,argtypes,&offset);

  // Compile bytecode
  struct G__bytecodefunc *pbc = func.GetBytecode();
  if(!pbc) {
    fprintf(stderr,"Error: Bytecode compilation %s\n",funcName);
    G__unloadfile((Char_t *)exprFileName);
    return kFALSE; // can not get bytecode
  }
#endif
  // Prepare callfunc object
  int i;
  St_tableDescriptor  *tabsDsc   = GetRowDescriptors();
  tableDescriptor_st  *descTable = tabsDsc->GetTable();
  Float_t  results[]    = {1,1,1,1,1};
  Char_t **addressArray = (Char_t **)new ULong_t[tabsDsc->GetNRows()];
  Char_t *thisTable     = (Char_t *)GetArray();
#ifdef BYTECODE
  G__CallFunc callfunc;
  callfunc.SetBytecode(pbc);
 
  callfunc.SetArg((long)(&results[0]));   // give 'Float_t *results[5]' as 1st argument
  callfunc.SetArg((long)(addressArray));  // give 'void    *addressArray[]' as 2nd argument
#else
  char buf[200];
  sprintf(buf,"%s((Float_t*)(%ld),(void**)(%ld))",funcName,(long int)results,(long int)addressArray);
#endif
  
  // Call bytecode in loop

#ifdef BYTECODE
#  define CALLMETHOD callfunc.Exec(0);
#else
#  define CALLMETHOD G__calc(buf);
#endif

#define TAKEACTION_BEGIN                                                                    \
            descTable = tabsDsc->GetTable();                                                \
            for (i=0; i < tabsDsc->GetNRows(); i++,descTable++ )                            \
               addressArray[i] = thisTable + descTable->m_Offset + GetRowSize()*firstentry; \
            for(i=firstentry;i<lastEntry;i++) {                                             \
            CALLMETHOD

#define TAKEACTION_END  for (int j=0; j < tabsDsc->GetNRows(); j++ ) addressArray[j] += GetRowSize(); }                                                                     \


  if (firstentry < GetNRows() ) {
    Int_t lastEntry = TMath::Min(UInt_t(firstentry+nentries),UInt_t(GetNRows()));
      if (action < 0) {
        fVmin[0] = fVmin[1] = fVmin[2] = 1e30;
        fVmax[0] = fVmax[1] = fVmax[2] = -fVmin[0];
      }
      Int_t nchans = 0;
      switch ( action ) {
        case -1: {
             TAKEACTION_BEGIN
              if (results[1]) {
                 if (fVmin[0] > results[0]) fVmin[0] = results[0];
                 if (fVmax[0] < results[0]) fVmax[0] = results[0];
              }
             TAKEACTION_END
 
             nchans = fNbins[0];
             if (fVmin[0] >= fVmax[0]) { fVmin[0] -= 1; fVmax[0] += 1;}
             FindGoodLimits(nchans,fNbins[0],fVmin[0],fVmax[0]);
             ((TH1 *)obj)->SetBins(fNbins[0],fVmin[0],fVmax[0]);
           }
        case  1:
            TAKEACTION_BEGIN
               if (results[1]) ((TH1 *)obj)->Fill(Axis_t(results[0]),Stat_t(results[1]));
            TAKEACTION_END
            gCurrentTableHist = ((TH1 *)obj);
            break;
        case  -2:
            TAKEACTION_BEGIN
              if (results[2]) {
                if (fVmin[0] > results[1]) fVmin[0] = results[1];
                if (fVmax[0] < results[1]) fVmax[0] = results[1];
                if (fVmin[1] > results[0]) fVmin[1] = results[0];
                if (fVmax[1] < results[0]) fVmax[1] = results[0];
              }
            TAKEACTION_END
            nchans = fNbins[0];
            if (fVmin[0] >= fVmax[0]) { fVmin[0] -= 1; fVmax[0] += 1;}
            FindGoodLimits(nchans,fNbins[0],fVmin[0],fVmax[0]);
            if (fVmin[1] >= fVmax[1]) { fVmin[1] -= 1; fVmax[1] += 1;}
            FindGoodLimits(nchans,fNbins[1],fVmin[1],fVmax[1]);
            ((TH1*)obj)->SetBins(fNbins[1],fVmin[1],fVmax[1],fNbins[0],fVmin[0],fVmax[0]);
        case   2:
              if (obj->IsA() == TH2F::Class()) {
                 TAKEACTION_BEGIN
                   if (results[2]) ((TH2F*)obj)->Fill(Axis_t(results[0]),Axis_t(results[1]),Stat_t(results[2]));
                 TAKEACTION_END
              }
              else if (obj->IsA() == TH2S::Class()) {
                 TAKEACTION_BEGIN
                  if (results[2]) ((TH2S*)obj)->Fill(Axis_t(results[0]),Axis_t(results[1]),Stat_t(results[2]));
                 TAKEACTION_END
              }
              else if (obj->IsA() == TH2C::Class()) {
                 TAKEACTION_BEGIN
                   if (results[2]) ((TH2C*)obj)->Fill(Axis_t(results[0]),Axis_t(results[1]),Stat_t(results[2]));
                 TAKEACTION_END
              }
              else if (obj->IsA() == TH2D::Class()) {
                 TAKEACTION_BEGIN
                   if (results[2]) ((TH2D*)obj)->Fill(Axis_t(results[0]),Axis_t(results[1]),Stat_t(results[2]));
                 TAKEACTION_END
              }
            gCurrentTableHist =  ((TH1 *)obj);
            break;
        case -4:
            TAKEACTION_BEGIN
              if (results[2]) {
                if (fVmin[0] > results[1]) fVmin[0] = results[1];
                if (fVmax[0] < results[1]) fVmax[0] = results[1];
                if (fVmin[1] > results[0]) fVmin[1] = results[0];
                if (fVmax[1] < results[0]) fVmax[1] = results[0];
              }
            TAKEACTION_END
            nchans = fNbins[1];
            if (fVmin[1] >= fVmax[1]) { fVmin[1] -= 1; fVmax[1] += 1;}
            FindGoodLimits(nchans,fNbins[1],fVmin[1],fVmax[1]);
            ((TProfile*)obj)->SetBins(fNbins[1],fVmin[1],fVmax[1]);
        case  4:
            TAKEACTION_BEGIN
               if (results[2]) ((TProfile*)obj)->Fill(Axis_t(results[0]),Axis_t(results[1]),Stat_t(results[2]));
            TAKEACTION_END
            break;
        case -12:
            TAKEACTION_BEGIN
              if (results[2]) {
                if (fVmin[0] > results[1]) fVmin[0] = results[1];
                if (fVmax[0] < results[1]) fVmax[0] = results[1];
                if (fVmin[1] > results[0]) fVmin[1] = results[0];
                if (fVmax[1] < results[0]) fVmax[1] = results[0];
              }
            TAKEACTION_END
            nchans = fNbins[0];
            if (fVmin[0] >= fVmax[0]) { fVmin[0] -= 1; fVmax[0] += 1;}
            FindGoodLimits(nchans,fNbins[0],fVmin[0],fVmax[0]);
            if (fVmin[1] >= fVmax[1]) { fVmin[1] -= 1; fVmax[1] += 1;}
            FindGoodLimits(nchans,fNbins[1],fVmin[1],fVmax[1]);
            ((TH2F*)obj)->SetBins(fNbins[1],fVmin[1],fVmax[1],fNbins[0],fVmin[0],fVmax[0]);
        case  12: {             
            if (!strstr(option,"same") && !strstr(option,"goff")) {
              ((TH2F*)obj)->DrawCopy(option);
              gPad->Update();
            }
            TPolyMarker *pm = new TPolyMarker(lastEntry-firstentry);
//            pm->SetMarkerStyle(GetMarkerStyle());
//            pm->SetMarkerColor(GetMarkerColor());
//            pm->SetMarkerSize(GetMarkerSize());
            Float_t *x = pm->GetX();
            Float_t *y = pm->GetY();
            Float_t u, v;
            Float_t umin = gPad->GetUxmin();
            Float_t umax = gPad->GetUxmax();
            Float_t vmin = gPad->GetUymin();
            Float_t vmax = gPad->GetUymax();
            Int_t pointIndex = 0;
            TAKEACTION_BEGIN
             if (results[2]) {
                u = gPad->XtoPad(results[0]);
                v = gPad->YtoPad(results[1]);
                if (u < umin) u = umin;
                if (u > umax) u = umax;
                if (v < vmin) v = vmin;
                if (v > vmax) v = vmax;
                x[pointIndex] = u;
                y[pointIndex] = v;
                pointIndex++;
             }
            TAKEACTION_END
            if (!strstr(option,"goff")) pm->Draw();
            if (!((TH2F*)obj)->TestBit(kCanDelete)) 
            for(i=firstentry;i<lastEntry;i++) ((TH2F*)obj)->Fill(x[i], y[i]);
            gCurrentTableHist = ((TH1*)obj);         
          }
          break; 
        case -13:
            TAKEACTION_BEGIN
              if (results[3]) {
                if (fVmin[0] > results[2]) fVmin[0] = results[2];
                if (fVmax[0] < results[2]) fVmax[0] = results[2];
                if (fVmin[1] > results[1]) fVmin[1] = results[1];
                if (fVmax[1] < results[1]) fVmax[1] = results[1];
                if (fVmin[2] > results[0]) fVmin[2] = results[0];
                if (fVmax[2] < results[0]) fVmax[2] = results[0];
              }
            TAKEACTION_END
            rmin[0] = fVmin[2]; rmin[1] = fVmin[1]; rmin[2] = fVmin[0];
            rmax[0] = fVmax[2]; rmax[1] = fVmax[1]; rmax[2] = fVmax[0];
            gPad->Clear();
            gPad->Range(-1,-1,1,1);
            new TView(rmin,rmax,1);
        case 13: {
            TPolyMarker3D *pm3d = new TPolyMarker3D(lastEntry-firstentry);
//            pm3d->SetMarkerStyle(GetMarkerStyle());
//            pm3d->SetMarkerColor(GetMarkerColor());
//            pm3d->SetMarkerSize(GetMarkerSize());
            Int_t pointIndex = 0;
            TAKEACTION_BEGIN
                if (results[3]) {
                  pm3d->SetPoint(pointIndex,results[0],results[1],results[2]);
                  pointIndex++;
                }
            TAKEACTION_END
            pm3d->Draw();
          }  
            break;
        default:
          Error("EntryLoop","unknown action \"%d\" for table <%s>", action, GetName());
          break;
      };
  }
  G__unloadfile((Char_t *)exprFileName);
  delete [] addressArray;
  return kTRUE;
}

//______________________________________________________________________________
Int_t St_Table::MakeWrapClass(Text_t *name)
{
 //
 // It is a static method wich must be ROOT macro
 // But I failed to pass through ROOT though and did this method instead
 //
  Char_t filename[500];
  if (!name) { return -1;}
  strcpy(filename,name); 
  strcat(filename,".h");
//  gROOT->LoadMacro(filename);
   G__loadfile(filename);
 // Pull the "structure name from the file
  Char_t *structname = strrchr(name,'/');
  if (structname) 				structname++;
  else if ((structname = strrchr(name,':'))) 	structname++;
  else 						structname = name;
  St_Table t(structname,1);
  t.StafStreamer();
  return 0;
}
//______________________________________________________________________________
St_Table::St_Table(const Text_t *name, Int_t size) : St_DataSet()
{
   // Default St_Table ctor.
   s_TableHeader = new table_head_st;
   s_Size = &s_TableHeader->rbytes;
   s_MaxIndex = &s_TableHeader->nok;
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   s_Table = 0;
   *s_MaxIndex = 0;
   if (size == 0) Warning("St_Table(0)","Wrong table format");
}
 
//______________________________________________________________________________
St_Table::St_Table(const Text_t *name, Int_t n,Int_t size) : St_DataSet()
{
   // Create St_Table object and set array size to n longs.
 
   s_Table = 0;
   s_TableHeader = new table_head_st;
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   if (n > 0) Set(n);
   *s_MaxIndex = 0;
}
 
//______________________________________________________________________________
St_Table::St_Table(const Text_t *name, Int_t n, Char_t *table,Int_t size) : St_DataSet(name)
{
   // Create St_Table object and initialize it with values of array.
 
   s_Table = 0;
   s_TableHeader = new table_head_st; 
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);          // Define the default name and type as "unknown"
   Set(n, table);
}
 
//______________________________________________________________________________
St_Table::St_Table(const Text_t *name, const Text_t *type, Int_t n, Char_t *array, Int_t size) 
         : St_DataSet(name)
{
   // Create St_Table object and initialize it with values of array.
 
   s_Table = array;
   s_TableHeader = new table_head_st; 
   LinkHeader();
   *s_Size = size;
   SetHeadFields(name);
   SetType(type);
   SetfN(n); 
}

//______________________________________________________________________________
St_Table::St_Table(const St_Table &table)
{
   // Copy constructor.
 
   s_Table = 0;
   *s_TableHeader = *(table.s_TableHeader);
   Set(table.fN, table.s_Table);
}
 
//______________________________________________________________________________
St_Table &St_Table::operator=(const St_Table &rhs)
{
   // St_Table assingment operator.
   // This operator REALLOCATEs this table to fit the number of 
   // the USED rows of the source table if any

  if (strcmp(GetType(),rhs.GetType()) == 0) {
    if (this != &rhs && rhs.GetNRows() >0 ){
        Set(rhs.GetNRows(), rhs.s_Table);
        SetUsedRows(rhs.GetNRows());
    }
  }
  else 
    Error("operator=","Can not copy <%s> table into <%s> table", rhs.GetType(),GetType());
  return *this;
}
 
//______________________________________________________________________________
St_Table::~St_Table()
{
   // Delete St_Table object.
   Delete();
   SafeDelete(s_TableHeader);
}
 
//______________________________________________________________________________
void St_Table::Adopt(Int_t n, void *arr)
{
   // Adopt array arr into St_Table, i.e. don't copy arr but use it directly
   // in St_Table. User may not delete arr, St_Table dtor will do it.
 
   Clear();
 
   SetfN(n); SetUsedRows(n);
   s_Table = (char *)arr;
}
 
//______________________________________________________________________________
void St_Table::AddAt(const void *row, Int_t i)
{
   // Add one element (row) of structure at position i. Check for out of bounds.
 
   if (!BoundsOk("St_Table::AddAt", i))
      i = 0;
   memcpy(s_Table+i*(*s_Size),row,*s_Size);
   *s_MaxIndex = TMath::Max((Int_t)i+1,Int_t(*s_MaxIndex));
}

#if 0
//_______________________________________________________________________
void St_Table::Append(St_Table *donorTable, Int_t nRows, Int_t firstRows)
{
 // (not tested yet )
 // St_Table::Append copies the data from one table to this one 
 //                  after the last used row of this table (append)
 //  
 //  Int_t nRows     - the number of the "donor" rows to be appended
 //                    = 0  by defaul all "used" rows of the donor table
 //                         will be appened
 //  Int_t firstRows - the index of the first row of the donor table to 
 //                    be copied into the this
 //
 // Note:  this St_Table must be an "owner" the current table.
 // ----

  if (!donorTable || !donorTable->GetNRows() ) return;
  assert(!TestBit(kIsNotOwn));

  // Check whether the new table has the same type 
  if (strcmp(GetTitle(),donorTable->GetTitle()) == 0 ) 
  {
    // Calculate the number of the rows to copy
    if (!nRows) nRows = donorTable->GetNRows();
    Int_t extraRows = firstRows+nRows - donorTable->GetNRows();
    if (extraRows > 0) nRows -= extraRows;
    // Calculate the size to reallocate
    Int_t reallocate = GetNRows() + nRows - GetSize();
    if ( reallocate > 0 ) ReAllocate(reallocate);
    ::memcpy((*this)[GetNRows()],(*donorTable)[firstRows],GetRowSize()*nRows);
  }
  else
     Error("Update",
           "This table is <%s> but the host one has a wrong type <%s>",GetTitle()
          ,donorTable->GetTitle());
}
#endif
 
//______________________________________________________________________________
void St_Table::CopyStruct(Char_t *dest, const Char_t *src)
{
    ::memcpy(dest,src,*s_Size*fN);
}
//______________________________________________________________________________
void St_Table::CopySet(St_Table &array)
{
  array.Set(fN); 
  CopyStruct(array.s_Table,s_Table); 
 *(array.s_TableHeader) = *s_TableHeader; 
}

//______________________________________________________________________________
void *St_Table::ReAllocate()
{
   ReAlloc(GetNRows()+1);
   return (void *)s_Table;
}

//______________________________________________________________________________
void *St_Table::ReAllocate(Int_t newsize)
{
  if (newsize > fN) ReAlloc(newsize);
  return (void *)s_Table;
}

//______________________________________________________________________________
void St_Table::ReAlloc(Int_t newsize)
{
  if (!TestBit(kIsNotOwn) && s_Size && newsize > 0) {
    void *arr =  realloc(s_Table,*s_Size*newsize);
    SetfN(newsize);
    s_Table = (char *)arr;
  }
}
//______________________________________________________________________________
Char_t *St_Table::Create()
{
  if (s_Size) 
       return (Char_t *)malloc(*s_Size*fN);
  return 0;
}
//______________________________________________________________________________
void St_Table::Browse(TBrowser *b){
  St_DataSet::Browse(b);
  Int_t nrows = TMath::Min(Int_t(GetNRows()),6);
  if (nrows == 0) nrows = 1;
  Print(0,nrows);
}
//______________________________________________________________________________
void St_Table::Clear(Option_t *opt)
{
  if (!s_Table) return;
  if (!opt || !opt[0]) {
    if (! TestBit(kIsNotOwn)) free(s_Table);
    s_Table = 0; *s_MaxIndex = 0; fN = 0;
    return;} 

  if (opt[0]=='g' || opt[0]=='G') {// Clear garbage
    if (TestBit(kIsNotOwn)) return;
    if (fN == s_MaxIndex[0])  	return;
    assert (fN>*s_MaxIndex);   
    int mx = s_MaxIndex[0]+1;
    if (mx!=fN) s_Table = (char*)realloc(s_Table,mx*s_Size[0]);
    memset(s_Table+(mx-1)*s_Size[0],127,s_Size[0]);

    if (!s_MaxIndex[0] && fN > 100) 
      Warning("Clear"," Table %s has purged from %d to zero ",GetName(),fN);
      
    fN = mx;
    return;}
}
//______________________________________________________________________________
void St_Table::Delete(Option_t *opt)
{
  Clear();
  St_DataSet::Delete(opt);
}

#if 0
//______________________________________________________________________________
void St_Table::Dump()
{
   // Dump contents of object on stdout.
   // Using the information in the object dictionary (class TClass)
   // each data member is interpreted.
   // If a data member is a pointer, the pointer value is printed
   //
   // The following output is the Dump of a TArrow object:
   //   fAngle                   0           Arrow opening angle (degrees)
   //   fArrowSize               0.2         Arrow Size
   //   fOption.*fData
   //   fX1                      0.1         X of 1st point
   //   fY1                      0.15        Y of 1st point
   //   fX2                      0.67        X of 2nd point
   //   fY2                      0.83        Y of 2nd point
   //   fUniqueID                0           object unique identifier
   //   fBits                    50331648    bit field status word
   //   fLineColor               1           line color
   //   fLineStyle               1           line style
   //   fLineWidth               1           line width
   //   fFillColor               19          fill area color
   //   fFillStyle               1001        fill area style
 
   char parent[256];
   parent[0] = 0;
   TDumpMembers dm;
   ShowMembers(dm, parent);
}
#endif 
//______________________________________________________________________________
TClass  *St_Table::GetRowClass() const 
{
 // Return TClass object defining the original STAF table

   TClass *tabClass = Class();
   TClass *cl    = IsA();
   TClass *clPrev = 0;
   if (cl == tabClass) return 0;
   TList *l = 0;
   while (cl && cl != tabClass) {
     l = cl->GetListOfBases();
     clPrev = cl;
     cl = 0;
     TBaseClass *baseCl = 0;
     if (  l && 
           l->GetSize()==1 && 
         ( baseCl = (TBaseClass *)l->First() ) && 
           baseCl
        )  cl = baseCl->GetClassPointer();
     else assert (l->GetSize() == 1);
   } 
   if (cl ) {
     TString buffer = clPrev->GetName();
     buffer.ReplaceAll("St_","");
     buffer += "_st";
     cl = gROOT->GetClass(buffer.Data());
   }
   return cl;
}
//______________________________________________________________________________
Long_t St_Table::GetNRows() const { 
// Returns the number of the used rows for the wrapped table
return *s_MaxIndex;
}
//______________________________________________________________________________
Long_t St_Table::GetRowSize() const { 
// Returns the size (in bytes) of one table row 
  return *s_Size;
}
//______________________________________________________________________________
Long_t St_Table::GetTableSize() const { 
// Returns the number of the allocated rows 
return fN;
}
//______________________________________________________________________________
void St_Table::Fit(const Text_t *formula ,const Text_t *varexp, const Text_t *selection,Option_t *option ,Option_t *goption,Int_t nentries, Int_t firstentry)
{
//*-*-*-*-*-*-*-*-*Fit a projected item(s) from a St_Table*-*-*-*-*-*-*-*-*-*
//*-*              =======================================
//
//  formula is a TF1 expression.
//
//  See St_Table::Draw for explanations of the other parameters.
//
//  By default the temporary histogram created is called htemp.
//  If varexp contains >>hnew , the new histogram created is called hnew
//  and it is kept in the current directory.
//  Example:
//    table.Fit(pol4,"sqrt(x)>>hsqrt","y>0")
//    will fit sqrt(x) and save the histogram as "hsqrt" in the current
//    directory.
//
 
   Int_t nch = strlen(option) + 10;
   char *opt = new char[nch];
   if (option) sprintf(opt,"%sgoff",option);
   else        strcpy(opt,"goff");
 
   Draw(varexp,selection,opt,nentries,firstentry);
 
   delete [] opt;
 
   TH1 *hfit = gCurrentTableHist;
   if (hfit) {
      printf("hname=%s, formula=%s, option=%s, goption=%s\n",hfit->GetName(),formula,option,goption);
      // remove bit temporary 
      Bool_t canDeleteBit = hfit->TestBit(kCanDelete);
      if (canDeleteBit)  hfit->ResetBit(kCanDelete);
      hfit->Fit(formula,option,goption);   
      if (TestBit(canDeleteBit))   hfit->SetBit(kCanDelete);
   }
   else      printf("ERROR hfit=0\n");
}

//______________________________________________________________________________
void  *St_Table::GetArray() const {
//  return the void pointer to the C-structure
 return s_Table; }
//______________________________________________________________________________
const Char_t *St_Table::GetType() const { 
//Returns the type of the wrapped C-structure kept as the TNamed title
  return GetTitle();
}

//______________________________________________________________________________
void St_Table::LinkHeader()
{
  // Link some class data members with the STAF table header
   if (s_TableHeader)
   {
     s_Size = &s_TableHeader->rbytes;
     s_MaxIndex = &s_TableHeader->nok;
   }
   else 
     Error("LinkHeader","wrong header !");
}

//______________________________________________________________________________
void St_Table::ls(Option_t *option)
{
  St_DataSet::ls(option);
  IncreaseDirLevel();
  IndentLevel();
  cout       <<Path() 
             <<"\t --> Allocated rows: "<<fN
             <<"\t Used rows: "<<*s_MaxIndex
             <<"\t Row size: "      << *s_Size << " bytes"
      <<endl;
  //  Print();
  DecreaseDirLevel();
}
//_____________________________________________________________________________
 void St_Table::ls(Int_t deep)
{
   St_DataSet::ls(deep);
   IncreaseDirLevel();
   IndentLevel();
   cout      <<Path() 
             <<"\t --> Allocated rows: "<<fN
             <<"\t Used rows: "<<*s_MaxIndex
             <<"\t Row size: " << *s_Size << " bytes"
      <<endl;
   //   Print();
   DecreaseDirLevel();
}


#ifdef WIN32
# ifndef finite
#   define finite _finite
# endif
#else
extern "C" {int finite( double x );}
#endif

//______________________________________________________________________________
Int_t St_Table::NaN() {
//
// return the total number of the NaN for float/double cells of this table
// Thanks Victor Perevoztchikov
//

  EColumnType code;
  char const *cell,*colname,*table;
  double word;
  int icol,irow,colsize,wordsize,nwords,iword,nerr,offset;

  St_tableDescriptor *rowDes = GetRowDescriptors();
  assert(rowDes);
  table = (const char*)GetArray();

  int ncols = rowDes->GetNumberOfColumns();

  int lrow  = GetRowSize();
  int nrows = GetNRows  ();
  nerr =0; 
  for (icol=0; icol < ncols; icol++) {// loop over cols
    code = rowDes->GetColumnType(icol);
    if (code!=kFloat && code!=kDouble) continue;
     
    offset   = rowDes->GetOffset    (icol); 
    colsize  = rowDes->GetColumnSize(icol); 
    wordsize = rowDes->GetTypeSize  (icol);
    nwords = colsize/wordsize;
    for (irow=0; irow < nrows; irow++) { //loop over rows
      cell = table + offset + irow*lrow;
      for (iword=0;iword<nwords; iword++,cell+=wordsize) { //words in col        
        word = (code==kDouble) ? *(double*)cell : *(float*)cell;   
        if (::finite(word)) 	continue;
//		ERROR FOUND
        nerr++; colname = rowDes->GetColumnName(icol);
        Warning("NaN"," Table %s.%s.%d\n",GetName(),colname,irow);
  }  }  } 
return nerr;
}

//______________________________________________________________________________
St_Table *St_Table::New(const Char_t *name, const Char_t *type, void *array, UInt_t size)
{
  // This static method creates a new St_Table object if provided 

  St_Table *table = 0;
  if (type && name) 
  {
    TString TableType(type); 
    TString t = TableType.Strip();
//    t.ToLower();  // remove this 

    const Char_t *classprefix="St_";
    const Int_t extralen = strlen(classprefix) + 1;
    Char_t *classname = new Char_t[strlen(t.Data())+extralen];
    strcpy(classname,classprefix);
    strcat(classname,t.Data());
    TClass *cl = gROOT->GetClass(classname);
    if (cl) {
      table = (St_Table *)cl->New();
      if (table) {
         table->SetTablePointer(array);
         table->SetName(name);
         table->SetfN(size);
         table->SetUsedRows(size);
      }
    } 
    delete [] classname;
  }
  return table; 
}
//______________________________________________________________________________
void  St_Table::Object(St_Table *&table, const table_head_st *header )
{
 // return a pointer to the C++ object by table header sullplied;
assert(header); table =  (St_Table *)header->dsl_pointer;
}

//______________________________________________________________________________
Char_t *St_Table::Print(Char_t *strbuf,Int_t lenbuf) const 
{
  Char_t buffer[100];
  strcpy(buffer,GetTitle());
  strcat(buffer,"_st");

//  ostrstream  out(strbuf,lenbuf);
  Int_t iOut = 0; 
 
  G__ClassInfo m(buffer);
  G__DataMemberInfo data(m);

  if (!m.Name()) {
      Error("Print"," No dictionary entry for <%s> structure", buffer);
      if (lenbuf>0) iOut += sprintf(strbuf+iOut," *** Errror ***");
      return strbuf;
  }
  IndentLevel();
 
  if (lenbuf>0) {
  // cut of the "_st" suffix 
     Char_t *typenam =  new Char_t [strlen(m.Name())+1];
     strcpy(typenam,m.Name());
  // look for the last "_"
     Char_t *last = strrchr(typenam,'_');
  // Check whether it is "_st"
     Char_t *eon = 0;
     if (last) eon = strstr(last,"_st");
  // Cut it off if any
     if (eon) *eon = '\0';
 //====      out << "struct " << typenam << " {";
     iOut += sprintf(strbuf+iOut,"struct %s {",typenam);
     delete [] typenam;
   }
   else
      cout << "struct " << m.Name() << " {" << endl;

  while(data.Next())
  {
    Int_t dim = data.ArrayDim();

    G__TypeInfo *t = data.Type();

    IndentLevel();

    if (lenbuf>0) {
//        out << " " << t->Name() << " " << data.Name();
       TString name = t->Name();
       name.ReplaceAll("unsigned char","octet");
       iOut += sprintf(strbuf+iOut," %s %s",name.Data(),data.Name());
    }
    else
        cout << '\t'<< t->Name() << '\t'<< data.Name();

    Int_t indx = 0;
    while (indx < dim) {
          if (lenbuf>0)
  //             out <<  "[" << data.MaxIndex(indx)<<"]";
                 iOut += sprintf(strbuf+iOut,"[%d]",data.MaxIndex(indx));
          else
               cout <<  "[" << data.MaxIndex(indx)<<"]";
          indx++;
    }
    if (lenbuf>0) 
//        out << "; " << data.Title();
//          iOut += sprintf(strbuf+iOut, "; %s", data.Title());
          iOut += sprintf(strbuf+iOut, ";");
    else 
        cout << ";\t//" <<  data.Title() << endl;
  }
  IndentLevel();
  if (lenbuf>0) {
//     out << "}";
          iOut += sprintf(strbuf+iOut, "}");
  }
  else
     cout << "}" << endl;
 
  return strbuf;
}
//______________________________________________________________________________
const Char_t *St_Table::PrintHeader() const
{
  // Print general table inforamtion 
     cout << endl << " ---------------------------------------------------------------------------------------" << endl
          <<  " " << Path() 
                 <<"  Allocated rows: "<<fN
                 <<"\t Used rows: "<<*s_MaxIndex
                 <<"\t Row size: "      << *s_Size << " bytes"
      <<endl; 
     return 0;
}

//______________________________________________________________________________
const Char_t *St_Table::Print(Int_t row, Int_t rownumber, const Char_t *, const Char_t *) const 
{
///const Char_t *St_Table::Print(Int_t row, Int_t rownumber, const Char_t *colfirst, const Char_t *collast) const 
  //  
  //  Print the contents of STAF tables per COLUMN.
  //
  //  row       - the index of the first row to print (counting from ZERO)
  //  rownumber - the total number of rows to print out (=10 by default)
  //
  //  (No use !) Char_t *colfirst, *collast - the names of the first/last 
  //                                          to print out (not implemented yet)
  //
  //--------------------------------------------------------------
   // Check bounds and adjust it
   Int_t const width = 8;
   Int_t rowStep = 10; // The maximun values to print per line
   Int_t rowNumber = rownumber;
   if (row  > Int_t(GetSize()) || GetSize() == UInt_t(0))  { 
        PrintHeader();
        cout  << " ======================================================================================" << endl
              << "   There are " << GetSize() << " allocated rows for this table only"                     << endl
              << " ======================================================================================" << endl;     
        return 0;
   }
   if (rowNumber > Int_t(GetSize()-row)) rowNumber = GetSize()-row;
   if (!rowNumber) return 0;
   rowStep = TMath::Min(rowStep,rowNumber);

   Int_t cdate = 0;
   Int_t ctime = 0;
   UInt_t *cdatime = 0;
   Bool_t isdate = kFALSE;
//   char *pname; 

//   if  (GetNRows() == 0) return 0;

   TClass *classPtr = GetRowClass();


   if (classPtr == 0) return 0;
   if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
   if (!classPtr->GetNdata()) return 0;

   TIter      next( classPtr->GetListOfDataMembers());

   //  3. Loop by "rowStep x lines"

   const Char_t  *startRow = (const Char_t *)GetArray() + row*GetRowSize(); 
   Int_t rowCount = rowNumber;
   Int_t thisLoopLenth = 0;
   const Char_t  *nextRow;
   while (rowCount) {
     PrintHeader();
     if  (GetNRows() == 0) {// to Print empty table header 
        cout  << " ======================================================================================" << endl
              << "   There is NO filled row in this table"                                                 << endl
              << " ======================================================================================" << endl;
       return 0; 
     }
      cout << " Table: " << classPtr->GetName()<< "\t";
      for (Int_t j = row+rowNumber-rowCount; j<row+rowNumber-rowCount+rowStep && j < row+rowNumber ;j++)
      { 
         Int_t hW = width-2;
         if (j>=10) hW -= (int)TMath::Log10(float(j))-1;
         cout  << setw(hW) << "["<<j<<"]";
         cout  << " :" ;
      }
      cout << endl
      <<       " ======================================================================================" << endl;
      next.Reset();
      TDataMember *member = 0;
      while ((member = (TDataMember*) next())) {
         TDataType *membertype = member->GetDataType();
         isdate = kFALSE;
         if (strcmp(member->GetName(),"fDatime") == 0 && strcmp(member->GetTypeName(),"UInt_t") == 0) {
            isdate = kTRUE;
         }

//         cout << member->GetTypeName() << "\t" << member->GetName(); 
         cout << member->GetTypeName();

         // Add the dimensions to "array" members 
         Int_t dim = member->GetArrayDim();
         Int_t indx = 0;
         Int_t *arrayLayout = 0;
         Int_t *arraySize = 0;
         if (dim) {
           arrayLayout = new Int_t[dim];
           arraySize  = new Int_t[dim];          
           memset(arrayLayout,0,dim*sizeof(Int_t));
         }
         Int_t arrayLength  = 1;
         while (indx < dim ){
            arraySize[indx] = member->GetMaxIndex(indx);
            arrayLength *= arraySize[indx];
//            cout << "["<<  arraySize[indx] <<"]";
           // Take in account the room this index will occupy
           indx++;
         }

         // Encode data value or pointer value
         Int_t offset = member->GetOffset();
         Int_t thisStepRows;
         thisLoopLenth = TMath::Min(rowCount,rowStep);
         Int_t indexOffset;
         Bool_t breakLoop = kFALSE;

         for (indexOffset=0; indexOffset < arrayLength && !breakLoop; indexOffset++) 
         {
           nextRow = startRow;
           if (!indexOffset) cout << "\t" << member->GetName();
           else              cout << "\t" << setw(strlen(member->GetName())) << " ";
//           if (dim && indexOffset) {
           if (dim) {
                for (Int_t i=0;i<dim;i++) cout << "["<<arrayLayout[i]<<"]";
                ArrayLayout(arrayLayout,arraySize,dim);
           }
           cout << "\t";
           if ( strlen(member->GetName())+3*dim < 8) cout << "\t";

           for (thisStepRows = 0;thisStepRows < thisLoopLenth; thisStepRows++,nextRow += GetRowSize())
           {
             const char *pointer = nextRow + offset  + indexOffset*membertype->Size();
             const char **ppointer = (const char**)(pointer);
 
             if (member->IsaPointer()) {
                const char **p3pointer = (const char**)(*ppointer);
                if (!p3pointer) {
                   printf("->0");
                } else if (!member->IsBasic()) {
//                   if (pass == 1) tlink = new TLink(xvalue+0.1, ytext, p3pointer);
                   cout << "N/A :" ;
                } else if (membertype) {
                   if (!strcmp(membertype->GetTypeName(), "char"))
                      cout << *ppointer;
                   else {
                        if (dim == 1) {
                          char charbuffer[11];
                          strncpy(charbuffer,*p3pointer,TMath::Min(10,arrayLength));
                          charbuffer[10] = 0;
                          cout << "\"" << charbuffer;
                          if (arrayLength > 10) cout << " . . . ";
                          cout << "\"";
                          breakLoop = kTRUE;
                        }
                        else 
//                           cout << membertype->AsString(p3pointer) << " :";
                          ::AsString(p3pointer,membertype->GetTypeName(),width);
                           cout << " :";
                   }
                } else if (!strcmp(member->GetFullTypeName(), "char*") ||
                         !strcmp(member->GetFullTypeName(), "const char*")) {
                   cout << setw(width) << *ppointer;
                } else {
//                   if (pass == 1) tlink = new TLink(xvalue+0.1, ytext, p3pointer);
                   cout << setw(width) << " N/A ";
                }
             } else if (membertype)
                if (isdate) {
                   cdatime = (UInt_t*)pointer;
                   TDatime::GetDateTime(cdatime[0],cdate,ctime);
                   cout << cdate << "/" << ctime;
                } else if (strcmp(membertype->GetFullTypeName(),"char")==0 && dim == 1) {
                    char charbuffer[11];
                    strncpy(charbuffer,pointer,TMath::Min(10,arrayLength));
                    charbuffer[10] = 0;
                    cout << "\"" << charbuffer;
                    if (arrayLength > 10) cout << " . . . ";
                    cout << "\"";
                    breakLoop = kTRUE;
                 }
                 else{
                    ::AsString((void *)pointer,membertype->GetTypeName(),width);
                    cout << " :";
//                    cout << membertype->AsString((void *)pointer) <<" :";
                 }
             else
                cout << "->" << (Long_t)pointer;
           }
        // Encode data member title
           if (indexOffset==0) {
             if (isdate == kFALSE && strcmp(member->GetFullTypeName(), "char*") &&
                 strcmp(member->GetFullTypeName(), "const char*")) {
                    cout << " " << member->GetTitle();
             }   
           }
           cout << endl;
         }
        if (arrayLayout) delete [] arrayLayout;
        if (arraySize) delete [] arraySize;
      }
      rowCount -= thisLoopLenth;
      startRow  = nextRow;
  }
  cout << "---------------------------------------------------------------------------------------" << endl;
  return 0;
 }
//______________________________________________________________________________
void St_Table::Project(const Text_t *hname, const Text_t *varexp, const Text_t *selection, Option_t *option,Int_t nentries, Int_t firstentry)
{
//*-*-*-*-*-*-*-*-*Make a projection of a St_Table using selections*-*-*-*-*-*-*
//*-*              =============================================
//
//   Depending on the value of varexp (described in Draw) a 1-D,2-D,etc
//   projection of the St_Table will be filled in histogram hname.
//   Note that the dimension of hname must match with the dimension of varexp.
//
 
   Int_t nch = strlen(hname) + strlen(varexp);
   char *var = new char[nch+5];
   sprintf(var,"%s>>%s",varexp,hname);
   nch = strlen(option) + 10;
   char *opt = new char[nch];
   if (option) sprintf(opt,"%sgoff",option);
   else        strcpy(opt,"goff");
 
   Draw(var,selection,opt,nentries,firstentry);
 
   delete [] var;
   delete [] opt;
}

//______________________________________________________________________________
Int_t St_Table::Purge(Option_t *opt)
{
  ReAllocate();
  return St_DataSet::Purge(opt);
}
//______________________________________________________________________________
void St_Table::SavePrimitive(ofstream &out, Option_t *)
{
// 		Save a primitive as a C++ statement(s) on output stream "out".
  Int_t arrayLayout[10],arraySize[10];
  const unsigned char *pointer=0,*startRow=0; 
  int i,rowCount;unsigned char ic;
  
  out << "St_DataSet *CreateTable() { " << endl;

  Int_t rowNumber =  GetNRows();
  TClass *classPtr = GetRowClass();

//			Is anything Wrong??
  if (!rowNumber || !classPtr ) {// 
     out << "// The output table was bad-defined!" << endl
         << " fprintf(stderr, \"Bad table found. Please remove me\\n\");" << endl
         << " return 0; } "    << endl;
     return;
  }

  if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();

  TIter      next( classPtr->GetListOfDataMembers());

  startRow = (const UChar_t *)GetArray();
  assert(startRow);

  const Char_t *rowId = "row";
  const Char_t *tableId = "tableSet";

// 			Generate the header

  out << "// -----------------------------------------------------------------" << endl;
  out << "// "   << Path() 
      << " Allocated rows: "<< rowNumber
      <<"  Used rows: "<<      rowNumber
      <<"  Row size: " << *s_Size << " bytes"  			<< endl; 
  out << "// "  << " Table: " << classPtr->GetName()<<"[0]--> "
      << classPtr->GetName()<<"["<<rowNumber-1 <<"]" 		<< endl;
  out << "// ====================================================================" << endl;
  out << "// ------  Test whether this table share library was loaded ------"      << endl;
  out << "  if (!gROOT->GetClass(\"" << "St_" << GetTitle() << "\")) return 0;"    << endl;
  out <<    classPtr->GetName() << " " << rowId << ";" << endl
      <<  "St_" <<  GetTitle() << " *" << tableId << " = new " 
      << "St_" << GetTitle()
      << "(\""<<GetName()<<"\"," << GetNRows() << ");" << endl 
      << "//" <<endl ;

//		Row loop
  for (rowCount=0;rowCount<rowNumber; rowCount++) {	//row loop
    out << "memset(" << "&" << rowId << ",0," << tableId << "->GetRowSize()" << ");" << endl ;
    next.Reset();
    TDataMember *member = 0;

//		Member loop
    while ((member = (TDataMember*) next())) {	//LOOP over members
      TDataType *membertype = member->GetDataType();
      TString memberName(member->GetName());
      TString memberTitle(member->GetTitle());      
      Int_t offset = member->GetOffset();
      int mayBeName = 0;
      if (memberName.Index("name",0,TString::kIgnoreCase)>=0) mayBeName=1999;
      if (memberName.Index("file",0,TString::kIgnoreCase)>=0) mayBeName=1999;
      TString memberType(member->GetFullTypeName());
      int memberSize = membertype->Size();

// 		Add the dimensions to "array" members 
      Int_t dim = member->GetArrayDim();
      if (dim) memset(arrayLayout,0,dim*sizeof(Int_t));
      Int_t arrayLength  = 1;
      for (int indx=0;indx < dim ;indx++){
         arraySize[indx] = member->GetMaxIndex(indx);
         arrayLength *= arraySize[indx];
      }

//			Special case, character array
      int charLen = (memberType.CompareTo("char")==0);
      if (charLen) { 	//Char case				
        charLen=arrayLength;
        pointer = startRow + offset;
//			Actual size of char array
        if (mayBeName) {
          charLen = strlen((const char*)pointer)+1;
          if (charLen>arrayLength) charLen = arrayLength;
        } else {
          for(;charLen && !pointer[charLen-1];charLen--){;}
          if (!charLen) charLen=1;
        }

        out << " memcpy(&" << rowId << "." << (const char*)memberName;      
        out << ",\"";      
        for (int ii=0; ii<charLen;ii++) {      
	  ic = pointer[ii];
	  if (ic && (isalnum(ic) 
	  || strchr("!#$%&()*+-,./:;<>=?@{}[]_|~",ic))) {//printable
	    out << ic;
	  } else {					//nonprintable
	    out << "\\x" << setw(2) << setfill('0') << hex << (unsigned)ic ; 
	    out << setw(1) << setfill(' ') << dec;
          }
        } 
        out << "\"," << dec << charLen << ");";
        out << "// " << (const char*)memberTitle << endl;
        continue;
      } //EndIf of char case

//			Normal member
      Int_t indexOffset;
      for (indexOffset=0; indexOffset < arrayLength ; indexOffset++) {//array loop
        out << setw(3) << " " ;  
        out << " " << rowId << "." << (const char*)memberName;

        if (dim) {
          for (i=0;i<dim;i++) {out << "["<<arrayLayout[i]<<"]";}
          ArrayLayout(arrayLayout,arraySize,dim);}

// 			Generate "="
        out << "\t = ";

        pointer = startRow + offset  + indexOffset*memberSize;
        assert(!member->IsaPointer()); 

        out << setw(10) << membertype->AsString((void *)pointer) ;

// 			Encode data member title
        if (indexOffset==0)  out << "; // " << (const char*)memberTitle;
         out << ";" << endl;
      }//end array loop
    }//end of member loop

    out << tableId << "->AddAt(&" << rowId << "," << rowCount <<");" << endl; 

    startRow  += *s_Size;

  }//end of row loop
  out << "// ----------------- end of code ---------------" << endl
      << " return (St_DataSet *)tableSet;" << endl
      << "}"  << endl;
  return;
}
 
//______________________________________________________________________________
Int_t St_Table::ReadGenericArray(TBuffer &b, void *&ii, EBufSizes membersize)
{
   // Read array of ANY type with pre-defined size from the I/O buffer. 
   // Returns the number of that read. 
   // If argument is a 0 pointer then space will be allocated for the
   // array.

  switch ( membersize) {
    case kChar1Byte:    return b.ReadArray((Char_t *&)ii);
         break;
    case kShort2Bytes:  return b.ReadArray((Short_t *&)ii);
         break;
    case kLong4Bytes:   return b.ReadArray((Long_t *&)ii);
         break;
    case kDouble8Bytes: return b.ReadArray((Double_t *&)ii); 
         break;
   default: 
         break;
  };
  return 0;
}

//______________________________________________________________________________
void St_Table::Set(Int_t n)
{
   // Set array size of St_Table object to n longs. If n<0 leave array unchanged.
   if (n < 0) return;
   if (fN != n)  Clear();
   SetfN(n);
   if (fN == 0) return;
   if (!s_Table) s_Table = Create();
   Reset();
}
//______________________________________________________________________________
void St_Table::SetHeadFields(const Text_t *name)
{
   if (name) SetName(name);      // Define "name" if supplied
   else SetName("unknown");      // Define the default name 
   SetType("unknown");           // Define the default type 
   s_TableHeader->dsl_pointer = (long)this;

}

//______________________________________________________________________________
void St_Table::SetName(const Char_t *name)
{
   SetTableName(name);
   St_DataSet::SetName(s_TableHeader->name);
}
//______________________________________________________________________________
void   St_Table::SetTableName(const Char_t *name){
// Fill staf table header with the table name information
 if (s_TableHeader)
 {
    s_TableHeader->name[0] = '\0';
    strncat(s_TableHeader->name,name,sizeof(s_TableHeader->name)); 
 }
}
//______________________________________________________________________________
void   St_Table::SetTableType(const Char_t *type){
// Fill staf table header with the table type information
   if (s_TableHeader) {
     s_TableHeader->type[0] = '\0';
     strncat(s_TableHeader->type,type,sizeof(s_TableHeader->type)); 
   }
}

//______________________________________________________________________________
void St_Table::SetTablePointer(void *table)
{ 
   if (s_Table) free(s_Table);
   s_Table = (Char_t *)table;
}

//______________________________________________________________________________
void St_Table::SetType(const Text_t *const type)
{
   SetTableType(type);
   St_DataSet::SetTitle(s_TableHeader->type);
}
//______________________________________________________________________________
int St_Table::PointerToPointer(G__DataMemberInfo &m)
{
   if (strstr(m.Type()->Name(), "**")) return 1;
   return 0;
}
//______________________________________________________________________________
static Char_t *GetExpressionFileName()
{ 
  // Create a name of the file in the temporary directory if any
  const Char_t *tempDirs =  gSystem->Getenv("TEMP");
  if (!tempDirs)  tempDirs =  gSystem->Getenv("TMP");
  if (!tempDirs) tempDirs = "/tmp";
  if (gSystem->AccessPathName(tempDirs)) tempDirs = ".";
  if (gSystem->AccessPathName(tempDirs)) return 0;
  Char_t buffer[16];
  sprintf(buffer,"C.%d.tmp",gSystem->GetPid());
  TString fileName = "Selection.";
  fileName += buffer;
  return  gSystem->ConcatFileName(tempDirs,fileName.Data());
}

//______________________________________________________________________________
Char_t *St_Table::MakeExpression(const Char_t *expressions[],Int_t nExpressions)
{
  // return the name of temporary file with the current expressions
   const Char_t *typeNames[] = {"NAN","float", "int",  "long",  "short",         "double"
                                ,"unsigned int","unsigned long", "unsigned short","unsigned char"
                                ,"char"};
   const char *resID     = "results";
   const char *addressID = "address";
   Char_t *fileName = GetExpressionFileName();
   if (!fileName) {
       Error("MakeExpression","Can not create a temoprary file");
       return 0;
   }

   ofstream str;
   str.open(fileName);
   if (str.bad() ) {
       Error("MakeExpression","Can not open the temporary file <%s>",fileName);
       delete [] fileName;
       return 0;
   }

   St_tableDescriptor *dsc = GetRowDescriptors();
   const tableDescriptor_st *descTable  = dsc->GetTable();
   // Create function
   str << "void SelectionQWERTY(float *"<<resID<<", float **"<<addressID<<")"   << endl;
   str << "{"                                                        << endl;
   int i = 0;
   for (i=0; i < dsc->GetNRows(); i++,descTable++ ) {
    // Take the column name
    const Char_t *columnName = descTable->m_ColumnName;
    const Char_t *type = 0;
    // First check whether we do need this column
    for (Int_t exCount = 0; exCount < nExpressions; exCount++) {
       if (expressions[exCount] && expressions[exCount][0] && strstr(expressions[exCount],columnName)) goto LETSTRY;
    }
    continue;
LETSTRY:
    Bool_t isScalar = !(descTable->m_Dimensions);
    Bool_t isFloat = descTable->m_Type == kFloat;
    type = typeNames[descTable->m_Type];
                    str << type << " ";
    if (!isScalar)  str << "*";
    
                    str << columnName << " = " ;
    if (isScalar)   str << "*(";
    if (!isFloat)   str << "(" << type << "*)";
                    str << addressID << "[" << i << "]";
    if (isScalar)   str << ")" ;
                    str << ";" << endl;
   }
   // Create expressions
   for (i=0; i < nExpressions; i++ ) {
      if (expressions[i] && expressions[i][0]) 
                   str << " "<<resID<<"["<<i<<"]=(float)(" << expressions[i] << ");"  << endl;
//      if (i == nExpressions-1 && i !=0 ) 
//          str  << "  if ("<<resID<<"["<<i<<"] == 0){ return; }" << endl;
   };
   str << "}" << endl;
   str.close();
   // Create byte code and check syntax
   if (str.good()) return fileName;
   delete [] fileName; 
   return 0;
}
//______________________________________________________________________________
void St_Table::MakeHeader(const Char_t *prefix,const Char_t *tablename,
                         const Char_t *suffix, FILE *fl)
{
  Char_t *include = new Char_t [strlen(prefix)+strlen(tablename)+strlen(suffix)+5];
  sprintf(include,"%s%s%s.h",prefix,tablename,suffix);
  //*-*
  //*-* Open file if that is not supplied via parameter
  //
   FILE *fp = (fl == 0 ? fopen(include, "w") : fl) ;
   fprintf(fp, "#ifndef STAF_%s%s_Table\n",prefix,tablename);
   fprintf(fp, "#define STAF_%s%s_Table\n",prefix,tablename);
   fprintf(fp, "\n");
   fprintf(fp, "#include \"St_Table.h\"\n");
   fprintf(fp, "\n");
   fprintf(fp, "#include \"%s.h\"\n",tablename);
   fprintf(fp, "\n");
   fprintf(fp, "class St_%s : public St_Table\n",tablename);
   fprintf(fp, "{\n");
   fprintf(fp, "public:\n");
   fprintf(fp, "  %s%s() : St_Table(\"%s\",sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Text_t *name) : St_Table(name,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Int_t n): St_Table(\"%s\",n,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename,tablename);
   fprintf(fp, "  %s%s(Text_t *name,Int_t n): St_Table(name,n,sizeof(%s_st)) {SetType(\"%s\");}\n"
          ,prefix,tablename,tablename,tablename);
   fprintf(fp, "  %s_st *GetTable(){ return (%s_st *)s_Table;}\n"
          ,tablename,tablename);
   fprintf(fp, "\n");
   fprintf(fp, "  ClassDef(%s%s,0) // class particle STAF tables\n"
          ,prefix,tablename);
   fprintf(fp, "};\n");
   fprintf(fp, "\n");
   fprintf(fp, "#endif  \n");
  fclose(fp);
  delete [] include;
}
//______________________________________________________________________________
void St_Table::StafStreamer(Char_t *structname, FILE *fl)
{

  ///////////////////////////////////////////////////////////////////////////////
  //
  // Method StafStreamer(Char_t *structname, FILE *fl)
  // Creates the implemetation of the class to wrap the "plain"
  // C data strucutre.
  //
  //  It is assumes if "name" is the name of the C-structure
  // 
  //    1. The name of this class is created as "prefix"+"name"
  //    2. The real name of the base C-structure is "name"+"tabsuffux"
  //    3. The of the generetared file with this class implementation
  //       is "prefix" + "name" + "suffix" + ".cxx" if any
  //
  //  Parameters:
  //      Char_t *structname  - The "name" of the "extrernal C data structure
  //                            (see p.1 above)
  //                      = 0  - The name is defined with GetType() method
  //      FILE *fl            - the FILE handler used to write the generated 
  //                            C++ code out, otherwise this method opens file
  //                            as p.3 defines alone and close it before return
  //
  ///////////////////////////////////////////////////////////////////////////////

   Char_t datamem[256];
   Char_t *tablename=0;;
   Char_t datatype[256];
   Char_t *thisclassname=0;
   const Char_t *prefix = "St_";      // Prefix for all STAF wrapper (table & modules) classes
   const Char_t *suffix = "_Table";   // Siffix to distinguish "module" and "table" wrappers
   const Char_t *tabsuffix ="_st";    // Suffix defined with STAF C structures
   Bool_t thisclassbase;
   const Char_t row[]="row";
   
   Bool_t typeunknown = strcmp(GetType(),"unknown") == 0;

   if (structname || typeunknown ) 
   {
        if (!structname) {
    //
    // The name of the original structure has not been supplied
    // Try to guess it
    //
          tablename = new Char_t [strlen(GetName())+1];
          strcpy(tablename,GetName());
        }
        else {
    //
    // The name of the original structure has been supplied
    //
          tablename = new Char_t [strlen(structname)+1];
          strcpy(tablename,structname);
        }
        strcpy(datamem, prefix);
        strcat(datamem, tablename);
        thisclassbase = kTRUE;
   }
   else if (!typeunknown)
   {
       tablename = new Char_t[strlen(GetType())+1];
       strcpy(tablename,GetType());
       strcpy(datamem, ClassName());
       thisclassbase = kFALSE;
   }
   else
   {
     Error("StafStreamer", "Can't guess the C-structure typedef");
     return;
   }
   thisclassname = new Char_t[strlen(datamem)+1]; 
   strcpy(thisclassname,datamem);
   strcat(datamem,suffix);
   strcat(datamem,".cxx");
  //*-*
  //*-* Open file if that is not supplied via parameter
  //
   FILE *fp = (fl == 0 ? fopen(datamem, "w") : fl) ;

//    Create ClassInfo
   strcpy(datatype,tablename);
   strcat(datatype,tabsuffix);

   G__ClassInfo cl(datatype);

   strcpy(datamem,"((");
   strcat(datamem,tablename);
   strcat(datamem,tabsuffix);
   strcat(datamem," *)s_Table)");

   // Create include file

   MakeHeader(prefix,tablename,suffix);

   fprintf(fp, "#include \"%s%s%s.h\"\n",prefix,tablename,suffix);
   fprintf(fp, "#include \"Stypes.h\"\n");
   fprintf(fp, "/////////////////////////////////////////////////////////////////////////\n");
   fprintf(fp, "//\n");
   fprintf(fp, "//  Class %s wraps the STAF table %s \n",thisclassname,datatype);
   fprintf(fp, "//  It has been generated \"by automatic\". Please don\'t change it \"by hand\"\n");
   fprintf(fp, "//\n");
   fprintf(fp, "///////////////////////////////////////////////////////////////////////// \n\n");
   fprintf(fp, "TableImp(%s)\n",tablename);

   fprintf(fp, "//_______________________________________");
   fprintf(fp, "_______________________________________\n");
   fprintf(fp, "void %s::Streamer(TBuffer &R__b)\n{\n", thisclassname);
   fprintf(fp, "   // Stream an array of the \"plain\" C-structures <%s>.\n\n", cl.Name());
 
   // In case of VersionID<=0 write dummy streamer only calling
   // its base class Streamer(s). If no base class(es) let Streamer
   // print error message, i.e. this Streamer should never have been called.

//   int i = pass;
//   sprintf(a, "%s::Class_Version()", cl.Name());
//   version = (int)G__int(G__calc(a));
 
   // loop twice: first time write reading code, second time writing code
   for (int i = 0; i < 2; i++) {
 
      int decli = 0;

      if (i == 0) {
         fprintf(fp, "   if (R__b.IsReading()) {\n");
         fprintf(fp, "      Version_t R__v = R__b.ReadVersion(); if (R__v) { }\n");
      } else {
         fprintf(fp, "   } else {\n");
         fprintf(fp, "      R__b.WriteVersion(%s::IsA());\n",thisclassname);
      }

      // Stream base classes if any
      if (!thisclassbase)
      {
        TIter next(IsA()->GetListOfBases());
        TClass *obj;
        while ((obj = (TClass *)next())) 
           fprintf(fp, "      %s::Streamer(R__b);\n", obj->GetName());
      }
      else 
           fprintf(fp, "      %s::Streamer(R__b);\n", IsA()->GetName());

     // Create a loop for IMaxIndex

      fprintf(fp, "      if (*s_MaxIndex <= 0) return; \n\n");
      fprintf(fp, "      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {\n");
      fprintf(fp, "        %s &%s=*(%s+indx);\n",datatype, row, datamem);

      // Stream base class(es) when they have the Streamer() method
      G__BaseClassInfo b(cl);
 
      while (b.Next())
         if (b.HasMethod("Streamer"))
            fprintf(fp, "        %s::Streamer(R__b);\n", b.Name());
 
      // Stream data members
      G__DataMemberInfo m(cl);
 
      while (m.Next()) {
 
         // we skip:
         //  - static members
         //  - members with an ! as first character in the title (comment) field
         //  - the member G__virtualinfo inserted by the CINT RTTI system
 
         if (!(m.Property() & G__BIT_ISSTATIC) &&
             strncmp(m.Title(), "!", 1)        &&
             strcmp(m.Name(), "G__virtualinfo")) {
 
            // fundamental type: short, int, long, etc....
            if (((m.Type())->Property() & G__BIT_ISFUNDAMENTAL) ||
                ((m.Type())->Property() & G__BIT_ISENUM)) {
               if (m.Property() & G__BIT_ISARRAY &&
                   m.Property() & G__BIT_ISPOINTER) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                 if (i == 0) {
                     fprintf(stderr,"*** Datamember %s::%s: array of pointers to fundamental type (need manual intervention)\n", cl.Name(), m.Name());
                     fprintf(fp, "           ;//R__b.ReadArray(%s.%s);\n",row, m.Name());
                  } else {
                     fprintf(fp, "           ;//R__b.WriteArray(%s.%s, __COUNTER__);\n",row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISPOINTER) {
                  if (i == 0) {
                     fprintf(stderr,"*** Datamember %s::%s: pointer to fundamental type (need manual intervention)\n", cl.Name(), m.Name());
                     fprintf(fp, "        //R__b.ReadArray(%s.%s);\n",row, m.Name());
                  } else {
                     fprintf(fp, "        //R__b.WriteArray(%s.%s, __COUNTER__);\n",row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISARRAY) {
                  if (i == 0) {
                     if (m.ArrayDim() > 1) {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.ReadStaticArray((Int_t*)(%s.%s));\n", row,m.Name());
                        else
                           fprintf(fp, "        R__b.ReadStaticArray((%s*)(%s.%s));\n", m.Type()->TrueName(), row, m.Name());
                     } else {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.ReadStaticArray((Int_t*)(%s.%s));\n", row, m.Name());
                        else
                           fprintf(fp, "        R__b.ReadStaticArray(%s.%s);\n",row, m.Name());
                      }
                  } else {
                     int s = 1;
                     for (int dim = 0; dim < m.ArrayDim(); dim++)
                        s *= m.MaxIndex(dim);
                     if (m.ArrayDim() > 1) {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.WriteArray((Int_t*)(%s.%s), %d);\n", row,m.Name(), s);
                        else
                           fprintf(fp, "        R__b.WriteArray((%s*)(%s.%s), %d);\n", m.Type()->TrueName(), row, m.Name(), s);
                     } else {
                        if ((m.Type())->Property() & G__BIT_ISENUM)
                           fprintf(fp, "        R__b.WriteArray((Int_t*)(%s.%s), %d);\n", row, m.Name(), s);
                        else
                           fprintf(fp, "        R__b.WriteArray(%s.%s, %d);\n", row, m.Name(), s);
                     }
                  }
               } else if ((m.Type())->Property() & G__BIT_ISENUM) {
                  if (i == 0)
                     fprintf(fp, "        R__b >> (Int_t&)(%s.%s);\n", row, m.Name());
                  else
                     fprintf(fp, "        R__b << (Int_t)(%s.%s);\n", row, m.Name());
               } else {
                  if (i == 0)
                     fprintf(fp, "        R__b >> %s.%s;\n", row, m.Name());
                  else
                     fprintf(fp, "        R__b << %s.%s;\n", row, m.Name());
               }
            } else {
               // we have an object
               if (m.Property() & G__BIT_ISARRAY &&
                   m.Property() & G__BIT_ISPOINTER) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                  if (i == 0) {
                     fprintf(fp, "           R__b >> (%s.%s)[R__i];\n", row, m.Name());
                  } else {
                     fprintf(fp, "           R__b << (%s.%s)[R__i];\n", row, m.Name());
                  }
               } else if (m.Property() & G__BIT_ISPOINTER) {
                  // This is always good. However, in case of a pointer
                  // to an object that is guarenteed to be there and not
                  // being referenced by other objects we could use
                  //     xx->Streamer(b);
                  // Optimize this with control statement in title.
                  if (PointerToPointer(m)) {
                     if (i == 0) {
                        fprintf(stderr,"*** Datamember %s::%s: pointer to pointer (need manual intervention)\n", cl.Name(), m.Name());
                        fprintf(fp, "        //R__b.ReadArray(%s.%s);\n", row, m.Name());
                     } else {
                        fprintf(fp, "        //R__b.WriteArray(%s.%s, __COUNTER__);\n", row, m.Name());
                     }
                  } else {
                     if (strstr(m.Type()->Name(), "TClonesArray")) {
                        fprintf(fp, "        (%s.%s)->Streamer(R__b);\n", row, m.Name());
                     } else {
                        if (i == 0)
                           fprintf(fp, "        R__b >> %s.%s;\n", row, m.Name());
                        else {
                           if (m.Type()->IsBase("TObject") && m.Type()->IsBase("TArray"))
                              fprintf(fp, "        R__b << (TObject*)(%s.%s);\n", row, m.Name());
                           else
                              fprintf(fp, "        R__b << %s.%s;\n", row, m.Name());
                        }
                     }
                  }
               } else if (m.Property() & G__BIT_ISARRAY) {
                  int s = 1;
                  for (int dim = 0; dim < m.ArrayDim(); dim++)
                     s *= m.MaxIndex(dim);
                  if (!decli) {
                     fprintf(fp, "        int R__i;\n");
                     decli = 1;
                  }
                  fprintf(fp, "        for (R__i; R__i < %d; R__i++)\n", s);
                  fprintf(fp, "           (%s.%s)[R__i].Streamer(R__b);\n", row, m.Name());
               } else {
                  if ((m.Type())->HasMethod("Streamer"))
                     fprintf(fp, "        (%s.%s).Streamer(R__b);\n", row, m.Name());
                  else {
                     if (i == 0)
                        fprintf(stderr, "*** Datamember %s::%s: object has no Streamer() method (need manual intervention)\n",
                                cl.Name(), m.Name());
                     fprintf(fp, "        //(%s.%s).Streamer(R__b);\n", row, m.Name());
                  }
               }
            }
         }
      }

   fprintf(fp, "     }\n");
   }
   fprintf(fp, "   }\n");
   fprintf(fp, "}\n\n");
   if (!fl) fclose(fp);  // Close file if that was opened with this method

   // Clean things

   if (thisclassname) delete [] thisclassname;
   if (tablename)     delete [] tablename;
}

//______________________________________________________________________________
void St_Table::Reset(Int_t c)
{
  if (s_Table) ::memset(s_Table,c,*s_Size*fN);
}

//______________________________________________________________________________
void St_Table::Set(Int_t n, Char_t *array)
{
   // Set array size of St_Table object to n longs and copy array.
   // If n<0 leave array unchanged.
 
   if (n < 0) return;
   if (fN < n) Clear();

   SetfN(n);

   if (fN == 0) return;
   if (!s_Table) s_Table = Create();
   CopyStruct(s_Table,array);
   *s_MaxIndex = n;   
}
 
//_______________________________________________________________________
Int_t St_Table::StreamerTable(StBufferAbc &)
{
 return 0;
}
//_______________________________________________________________________
void St_Table::StreamerTable(TBuffer &b)
{
   // Stream a St_Table object.
   // Stream an object of class St_Table.


   if (b.IsReading()) {
     St_DataSet::Streamer(b);
     b >> fN;
     StreamerHeader(b);
     LinkHeader();
    //   Create a table to fit nok rows
     Set(*s_MaxIndex);
     //     printf(" Read:s_Size = %i fN = %i \n", *s_Size, fN);
   } else {
      St_DataSet::Streamer(b);
      b << fN;
      StreamerHeader(b);
      //      printf(" Write: s_Size = %i  fN = %i head size =  %i \n", *s_Size, fN, sizeof(table_head_st));
   }
}
 
//_______________________________________________________________________
void St_Table::StreamerHeader(TBuffer &b)
{
  if (b.IsReading()) 
  {
   b.ReadStaticArray(s_TableHeader->name);         /* table name */
   b.ReadStaticArray(s_TableHeader->type);         /* table type */
   b >> s_TableHeader->maxlen;       /* # rows allocated */
   b >> s_TableHeader->nok;          /* # rows filled */
   b >> s_TableHeader->rbytes;       /* number of bytes per row */
   b >> s_TableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
   b >> s_TableHeader->data_pointer; /* swizzled (char*) */
  }
  else {
   b.WriteArray(s_TableHeader->name,sizeof(s_TableHeader->name));    /* table name */
   b.WriteArray(s_TableHeader->type,sizeof(s_TableHeader->type));    /* table type */
   b << s_TableHeader->maxlen;       /* # rows allocated */
   b << s_TableHeader->nok;          /* # rows filled */
   b << s_TableHeader->rbytes;       /* number of bytes per row */
   b << s_TableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
   b << s_TableHeader->data_pointer; /* swizzled (char*) */
  }
}

#if 0
//_______________________________________________________________________
TBuffer &operator>>(TBuffer &buf, St_Table *&obj)
{
   // Read St_Table object from buffer. Declared in ClassDef.
 
   obj = (St_Table *) TArray::ReadArray(buf, St_Table::Class());
   return buf;
}

#endif

//_______________________________________________________________________
Int_t St_Table::SetfN(Long_t len)
{
   fN = len;
   if (s_TableHeader)
   {
//     s_TableHeader->nok = fN;
     s_TableHeader->maxlen = fN;
   } 
   return fN;
}

//____________________________________________________________________________
#ifdef StreamElelement
#define __StreamElelement__ StreamElelement
#undef StreamElelement
#endif

#define StreamElementIn(type)  case St_TableElementDescriptor::_NAME2_(k,type):            \
 if (nextCol->m_Dimensions)                                   \
   R__b.ReadStaticArray((_NAME2_(type,_t) *)(row+nextCol->m_Offset));                      \
 else                                                         \
   R__b >> *(_NAME2_(type,_t) *)(row+nextCol->m_Offset);      \
 break

#define StreamElementOut(type) case St_TableElementDescriptor::_NAME2_(k,type):            \
 if (nextCol->m_Dimensions)                                   \
    R__b.WriteArray((_NAME2_(type,_t) *)(row+nextCol->m_Offset), nextCol->m_Size/sizeof(_NAME2_(type,_t))); \
 else                                                         \
    R__b << *(_NAME2_(type,_t) *)(row+nextCol->m_Offset);     \
 break

#define StreamNMElementIn(type)  case St_TableElementDescriptor::_NAME2_(k,type):           \
 if (nextCol->m_Dimensions)                                   \
   R__b.ReadStaticArray((_NAME2_(type,_t) *)(row+nextCol->m_Offset),nextCol->m_ColumnName); \
 else                                                         \
   R__b.ReadScalar(*(_NAME2_(type,_t) *)(row+nextCol->m_Offset),nextCol->m_ColumnName);     \
 break

#define StreamNMElementOut(type) case St_TableElementDescriptor::_NAME2_(k,type):           \
 if (nextCol->m_Dimensions)                                   \
    R__b.WriteArray((_NAME2_(type,_t) *)(row+nextCol->m_Offset), nextCol->m_Size/sizeof(_NAME2_(type,_t)),nextCol->m_ColumnName); \
 else                                                         \
    R__b.WriteScalar(*(_NAME2_(type,_t) *)(row+nextCol->m_Offset),nextCol->m_ColumnName);   \
 break

//______________________________________________________________________________
Int_t St_Table::StreamerHeader(StBufferAbc &){ return 0;}
//______________________________________________________________________________
St_tableDescriptor  *St_Table::GetRowDescriptors() const 
{
  St_tableDescriptor *dsc = GetDescriptorPointer();
  if (!dsc) {
    dsc = GetTableDescriptors();
    SetDescriptorPointer(dsc);
  }
  return dsc;
}
//______________________________________________________________________________
St_tableDescriptor *St_Table::GetDescriptorPointer() const { assert(0); return 0;}
//______________________________________________________________________________
void St_Table::SetDescriptorPointer(St_tableDescriptor *) const { assert(0); }

//______________________________________________________________________________
Int_t St_Table::Streamer(StBufferAbc &R__b)
{
   // Stream an array of the "plain" C-structures

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      St_Table::StreamerTable(R__b);
      if (*s_MaxIndex <= 0) return -1; 
      char *row= s_Table;
      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {
        tableDescriptor_st *nextCol = GetRowDescriptors()->GetTable();
        Int_t maxColumns = GetNumberOfColumns();
        for (Int_t colCounter=0; colCounter < maxColumns; colCounter++) 
        {
          // Stream one table row supplied
          switch(nextCol->m_Type) {
           StreamNMElementIn(Float);
           StreamNMElementIn(Int);
           StreamNMElementIn(Long);
           StreamNMElementIn(Short);
           StreamNMElementIn(Double);
           StreamNMElementIn(UInt);
           StreamNMElementIn(ULong);
           StreamNMElementIn(UChar);
           StreamNMElementIn(Char);
          default:
            break;
        };
        nextCol++;
      }
      row += GetRowSize();
     }
   } else {
      St_Table::StreamerTable(R__b);
      if (*s_MaxIndex <= 0) return -1; 
      char *row= s_Table;
      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {
        tableDescriptor_st *nextCol = GetRowDescriptors()->GetTable();
        Int_t maxColumns = GetNumberOfColumns();
        for (Int_t colCounter=0; colCounter < maxColumns; colCounter++) 
        {
          // Stream one table row supplied
          switch(nextCol->m_Type) {
           StreamNMElementOut(Float);
           StreamNMElementOut(Int);
           StreamNMElementOut(Long);
           StreamNMElementOut(Short);
           StreamNMElementOut(Double);
           StreamNMElementOut(UInt);
           StreamNMElementOut(ULong);
           StreamNMElementOut(UChar);
           StreamNMElementOut(Char);
          default:
            break;
        };
        nextCol++;
      }
      row += GetRowSize();
     }
   }   
   return 0;
}
//______________________________________________________________________________
void St_Table::Streamer(TBuffer &R__b)
{
   // Stream an array of the "plain" C-structures

   if (R__b.IsReading()) {
      Version_t R__v = R__b.ReadVersion(); if (R__v) { }
      St_Table::StreamerTable(R__b);
      if (*s_MaxIndex <= 0) return; 
      char *row= s_Table;
      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {
        tableDescriptor_st *nextCol = GetRowDescriptors()->GetTable();
        Int_t maxColumns = GetNumberOfColumns();
        for (Int_t colCounter=0; colCounter < maxColumns; colCounter++) 
        {
          // Stream one table row supplied
          switch(nextCol->m_Type) {
           StreamElementIn(Float);
           StreamElementIn(Int);
           StreamElementIn(Long);
           StreamElementIn(Short);
           StreamElementIn(Double);
           StreamElementIn(UInt);
           StreamElementIn(ULong);
           StreamElementIn(UChar);
           StreamElementIn(Char);
          default:
            break;
        };
        nextCol++;
      }
      row += GetRowSize();
     }
   } else {
//      R__b.WriteVersion(St_ev0_track2::IsA());
      St_Table::StreamerTable(R__b);
      if (*s_MaxIndex <= 0) return; 
      char *row= s_Table;
      for (Int_t indx=0;indx<*s_MaxIndex;indx++) {
        tableDescriptor_st *nextCol = GetRowDescriptors()->GetTable();
        Int_t maxColumns = GetNumberOfColumns();
        for (Int_t colCounter=0; colCounter < maxColumns; colCounter++) 
        {
          // Stream one table row supplied
          switch(nextCol->m_Type) {
           StreamElementOut(Float);
           StreamElementOut(Int);
           StreamElementOut(Long);
           StreamElementOut(Short);
           StreamElementOut(Double);
           StreamElementOut(UInt);
           StreamElementOut(ULong);
           StreamElementOut(UChar);
           StreamElementOut(Char);
          default:
            break;
        };
        nextCol++;
      }
      row += GetRowSize();
     }
   }   
}
#ifdef __StreamElelement__
#define StreamElelement __StreamElelement__ 
#undef __StreamElelement__ 
#endif

//_______________________________________________________________________
void St_Table::Update(){;}
//_______________________________________________________________________
void St_Table::Update(St_DataSet *set, UInt_t opt)
{
 // Kill the table current data
 // and adopt those from set
  if (set->HasData()) 
  {
    // Check whether the new table has the same type 
    if (strcmp(GetTitle(),set->GetTitle()) == 0 ) 
    {
      St_Table *table =  (St_Table *)set;
      Adopt(table->GetSize(),table->GetArray());
     *s_TableHeader   = *(table->GetHeader());
      // mark that object lost the STAF table and can not delete it anymore
      table->SetBit(kIsNotOwn);
      // mark we took over of this STAF table
      ResetBit(kIsNotOwn);
    }
    else
       Error("Update",
             "This table is <%s> but the updating one has a wrong type <%s>",GetTitle(),set->GetTitle());
  }
  St_DataSet::Update(set,opt);
}

 //  ----   Table descriptor service   ------
const Char_t *St_Table::GetColumnName(Int_t columnIndex) const {return GetRowDescriptors()->GetColumnName(columnIndex); }
UInt_t      *St_Table::GetIndexArray(Int_t columnIndex)  const {return GetRowDescriptors()->GetIndexArray(columnIndex); }
UInt_t       St_Table::GetNumberOfColumns()              const {return GetRowDescriptors()->GetNumberOfColumns();       }

UInt_t       St_Table::GetOffset(Int_t columnIndex)      const {return GetRowDescriptors()->GetOffset(columnIndex); }
Int_t        St_Table::GetOffset(const Char_t *columnName) const {return GetRowDescriptors()->GetOffset(columnName); }

UInt_t       St_Table::GetColumnSize(Int_t columnIndex)  const {return GetRowDescriptors()->GetColumnSize(columnIndex); }
Int_t        St_Table::GetColumnSize(const Char_t *columnName) const {return GetRowDescriptors()->GetColumnSize(columnName); }

UInt_t       St_Table::GetTypeSize(Int_t columnIndex)    const {return GetRowDescriptors()->GetTypeSize(columnIndex); }
Int_t        St_Table::GetTypeSize(const Char_t *columnName) const {return GetRowDescriptors()->GetTypeSize(columnName); }

UInt_t       St_Table::GetDimensions(Int_t columnIndex)  const {return GetRowDescriptors()->GetDimensions(columnIndex); }
Int_t        St_Table::GetDimensions(const Char_t *columnName) const {return GetRowDescriptors()->GetDimensions(columnName); }

St_Table::EColumnType  St_Table::GetColumnType(Int_t columnIndex)  const {return GetRowDescriptors()->GetColumnType(columnIndex); }
St_Table::EColumnType  St_Table::GetColumnType(const Char_t *columnName) const {return GetRowDescriptors()->GetColumnType(columnName); }


// $Log: St_Table.cxx,v $
// Revision 1.92  2000/01/21 02:09:52  fine
// several parameters were converted to be const
//
// Revision 1.91  2000/01/12 01:24:50  fine
// several methods to use St_Table class from the <converted> C program to C++
//
// Revision 1.90  1999/11/30 23:54:04  fine
// Remove ToLower for Table types
//
// Revision 1.89  1999/11/10 00:23:55  fine
// adjusted to ne ROOT 2.23
//
// Revision 1.88  1999/10/28 16:24:34  fine
// St_DataSet major correction: it may be built with TList (default) or with TObjArray
//
// Revision 1.87  1999/10/28 00:32:54  fine
// method At() has been removed
//
// Revision 1.86  1999/10/22 22:53:17  fine
// TGaxis include added
//
// Revision 1.85  1999/10/17 23:19:41  fine
// The draft os St_Table::Append has been added
//
// Revision 1.84  1999/10/12 16:20:41  fine
// Advanced version of St_Table::GetRowClass() method
//
// Revision 1.83  1999/10/04 14:39:07  fine
// the fixed header size 20 was replaced with sizeof() operator
//
// Revision 1.82  1999/09/13 13:13:21  fine
// One blank has been deleted to Draw method comment in place
//
// Revision 1.81  1999/09/08 00:18:08  fisyak
// Add implementation for copy ctor of StObjArray
//
// Revision 1.80  1999/09/07 19:30:29  fine
// table descriptor access has been changed. All tables are affected and must be re-compiled
//
// Revision 1.79  1999/09/04 00:28:02  fine
// St_Table::NaN from VP and gloabl dataset have been introduced
//
// Revision 1.78  1999/09/01 22:32:54  fine
// St_Table::Fit fixed Rene\'s bug
//
// Revision 1.77  1999/08/30 23:15:08  fine
// St_Table::Fit method has been introduced
//
// Revision 1.76  1999/08/30 00:54:54  fine
// Working version of St_Table::Draw NO crash on either system
//
// Revision 1.75  1999/08/29 02:01:33  fine
// St_Table::Draw clean up, G__Calc is activated since it works well but too slow
//
// Revision 1.74  1999/08/28 01:23:12  fine
// St_Table::Draw work well under. With no bytecode works under Linux as well
//
// Revision 1.73  1999/08/26 04:35:46  fine
// St_Table::Draw() works somehow for Sun but still fail for Linux
//
// Revision 1.72  1999/08/25 02:45:47  fine
// StTable::Draw works under Sun and crashes under Linux
//
// Revision 1.71  1999/08/21 00:40:54  fine
// GetExpressionFileName function has been introduced
//
// Revision 1.70  1999/08/20 13:22:24  fine
// new method St_Table::Draw
//
// Revision 1.69  1999/08/13 23:27:56  fisyak
// remove endif
//
// Revision 1.68  1999/08/13 21:38:48  fine
// if endif fixed
//
// Revision 1.67  1999/08/13 17:42:34  fine
// Comments clean up
//
// Revision 1.66  1999/08/12 18:53:48  fine
// clash between St_tableDescriptor::GetSize and St_Table::GetSize resolved
//
// Revision 1.65  1999/08/12 16:41:30  fine
// Clean up
//
// Revision 1.64  1999/08/12 16:39:48  fine
// clash between St_Table::GetSize and TArray::GEtSize has been resolved
//
// Revision 1.63  1999/08/11 14:44:38  fine
// name clash with ROOT over enum resolved
//
// Revision 1.62  1999/08/11 00:42:33  fine
// new I/O via St_baseDescriptor table has been implemented
//
// Revision 1.61  1999/08/02 00:53:14  fine
// St_Table::New - protection against of zero pointers was removed by VP demand
//
// Revision 1.60  1999/07/07 16:44:40  fine
// Some print statements clean ups
//
// Revision 1.59  1999/07/02 21:00:31  fine
// Browse method prints empty table now
//
// Revision 1.58  1999/07/01 01:45:31  fisyak
// GetRowDescritors => GetRowDescriptors
//
// Revision 1.57  1999/06/27 00:46:56  fine
// Obsolete comment deleted
//
// Revision 1.56  1999/06/26 01:40:56  fisyak
// Add Valery's abstract buffer
//
// Revision 1.55  1999/06/25 17:29:14  fine
// Some bugs with a new Streamer fixed
//
// Revision 1.54  1999/06/25 01:35:53  fine
// New streamers for St_Tables
//
// Revision 1.53  1999/05/06 02:17:37  perev
// Supress warnings in Table if <100 rows
//
// Revision 1.52  1999/04/02 15:48:19  fine
// Minor casting problem for VC++ has been solved
//
// Revision 1.51  1999/04/01 20:17:20  fine
//  St_Table::Browse uses GetNRows rows at most now
//
// Revision 1.50  1999/03/30 23:24:50  fine
//  Some comments fixed
//
// Revision 1.49  1999/03/11 00:34:45  perev
// St_base in new maker schema
//
// Revision 1.48  1999/03/04 01:26:03  fine
// minor change in the comments
//
// Revision 1.47  1999/02/28 20:21:43  fine
// operator = reallocates the target table to fit used rows only, not allocated as before
//
// Revision 1.46  1999/02/24 17:10:57  fine
//  St_Table  New and Purge method have been introdiced, some clean up for St_module as well
//
// Revision 1.45  1999/02/22 23:54:37  fine
// St_Table::New() - method has been prepared but not activated yet
//
// Revision 1.44  1999/02/17 22:38:23  fisyak
// Victor fix for short print out
//
// Revision 1.43  1999/02/17 16:09:41  fisyak
// Fix Adopt
//
// Revision 1.42  1999/02/15 14:20:33  fisyak
// remove staf stuff
//
// Revision 1.41  1999/02/04 15:38:45  fine
//  St_Table::SavePrimitive - some extra protections have been introduced. Thanks Victor Perevoztchikov
//
// Revision 1.40  1999/02/03 23:19:15  fine
// St_Table:: protection for SavePrimitive has been introduced
//
// Revision 1.39  1999/01/31 02:03:08  fine
// St_DataSetIter::Notify - new method + clean up
//
// Revision 1.38  1999/01/30 18:29:29  fine
// Clean up
//
// Revision 1.37  1999/01/30 04:24:22  fine
// St_Table: Print memory leak fixed
//
// Revision 1.36  1999/01/29 19:20:55  fine
// St_Table::SavePrimitive The protection against of undefined table has been introduced
//
// Revision 1.35  1999/01/27 15:30:36  fine
// ArrayIndex is renamed to ArrayLayout to reflect its function
//
// Revision 1.34  1998/12/30 22:30:18  fine
// St_Table::PrintHrader method has been introduced
//
// Revision 1.33  1998/12/21 19:45:47  fisyak
// Move ROOT includes to non system
//
// Revision 1.32  1998/12/17 16:57:56  fine
// St_Table: some extra protections have been established (safe "strncat" have replaced the  unsafe  "strncpy")
//
// Revision 1.31  1998/12/17 14:36:51  fisyak
// fix Print for empty tablea, fix copy ctor
//
// Revision 1.30  1998/12/09 20:55:28  fine
// St_Table::Print() method bug has been fixed, some cosmetic improvements
//
// Revision 1.29  1998/12/07 20:23:11  fine
// St_Table:: working versions of the Print() and SavePrimitive methods
//
// Revision 1.28  1998/12/07 03:57:42  fine
// St_Table::SavePrimitive () method has been introduced
//
// Revision 1.27  1998/12/06 00:38:16  fisyak
// Add SavePrimitive
//
// Revision 1.25  1998/12/04 01:54:44  fine
// St_Table::Print(...) - The first version of the table browser has been introduced
//
// Revision 1.24  1998/10/31 00:20:14  fisyak
// Add ds2ReallocTable
//
// Revision 1.23  1998/10/14 22:40:48  fine
// St_Table::ReAllocate method and "plain" C interface to that has been introduced
//
// Revision 1.20  1998/10/04 02:20:12  fine
// St_Table.h Some clashes with TNamed and TObject have been fixed (affected Delete() method)
//
// Revision 1.19  1998/09/28 01:47:48  fisyak
// Use system includes for ROOT
//
// Revision 1.18  1998/09/23 20:22:24  fisyak
// Set kIsNotOwn=23
//
// Revision 1.16  1998/09/21 15:43:02  fine
// St_Table::Update bug has been fixed
//
// Revision 1.15  1998/09/16 22:08:52  fine
// St_DataSetIter - big in dtor has been fixed
// St_Table, St_DataSet - ls method has been improved
//
// Revision 1.14  1998/09/15 20:55:33  fisyak
// Split St_DataSet -> St_DataSet + St_DataSetIter
//
// Revision 1.11  1998/09/07 19:23:39  fine
// St_Table::Print() - malloc/free have been replaced with new [] / delete []  due a problem under Linux
// St_DataSet::~St_DataSet has been changed to take in account the "structural" links. Some opt have been done too
//
// Revision 1.10  1998/08/18 14:05:07  fisyak
// Add to bfc dst
//
// Revision 1.9  1998/07/23 22:12:00  fisyak
// Recover after Correction for root 2.09
//
// Revision 1.8  1998/07/23 21:09:14  fisyak
// Adjust for ROOT 2.09

