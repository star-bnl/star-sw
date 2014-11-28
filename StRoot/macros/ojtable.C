#ifndef __CINT__
#include <stdlib.h>
#include <stdio.h>
#include "TObject.h"
#include "TClass.h"
#include "TH1.h"
#include "TObjectTable.h"
#include "TMath.h"
#include "TRegexp.h"
#include "TClonesArray.h"
//#include "TDataSet.h"
#include "StMkDeb.h"
#include <typeinfo>
#endif // !__CINT__

void ojtable(const char *className=0);

#ifndef __CINT__
int SizeOfH(TObject *to);  

void ojtable(const char *className)
{
  static int fTableOffset = 0;
  if (!fTableOffset) fTableOffset = gObjectTable->IsA()->GetDataMemberOffset("fTable");
//  gObjectTable->Print();
  TClass *kl;
  if (className && !*className) className=0;

  const char *qwe = ".*";
  if (className) qwe = className;
  TRegexp regexp(qwe); int regexpLen=0;

  int sz = gObjectTable->GetSize();
  int *ptr = new int[sz];
  int *idx = new int[sz];

  TObject **tab = *((TObject ***)((char*)gObjectTable+fTableOffset));
  TObject *to;
  printf ("tab %p[%d]\n",tab,sz);
  int i,num=0;
  double hSize=0;
  int    hNumb=0;
  const char *info_name = 0;
  for (i=0;i<sz;i++)
  {
    to = tab[i];
    if (!to) 						continue;
    if (!to->TestBit(TObject::kNotDeleted))  		continue;
    int hs = SizeOfH(to);
    if (hs) {hSize+=hs; hNumb++;}
    if (className && regexp.Index(to->ClassName(),&regexpLen)<0)	continue;
    const char *mk = StMkDeb::GetUser(to);
    if(mk && *mk) printf("%s(%p) in %s\n",to->ClassName(),to,mk);
    ptr[num++]=int(to);
  }
  printf("TH1 N=%d, Size = %g\n",hNumb,hSize);

  TMath::Sort(num,ptr,idx,0);
  int last = 0;
  printf("Selected %d objects\n",num);
  for (i=0;i<num;i++) {
    int ix = idx[i];
    to = (TObject*)ptr[ix];
    int dist = 0;
    if (i) dist = ptr[ix]-last;
    last = ptr[ix];

//    if ((int)to == 0xc94ff34) {
//       printf("Skipped %p\n",to); continue; }

    info_name = "??";
    info_name=typeid(*to).name();
    kl = to->IsA();
  printf ("%4d +%6d : obj = %p(%3d) %s::%s \tinfo=%s\n",i,dist,to,kl->Size(),kl->GetName(),to->GetName(),info_name);

  if (strcmp("TClonesArray",kl->GetName())) continue;
  TClonesArray *tcl = ((TClonesArray*)to);
  printf(" Sizes = %d %d\n",tcl->GetLast()+1,tcl->Capacity());
  tcl->ls("");
  
  
  
  
}
  delete [] ptr;
  delete [] idx;
}
//______________________________________________________________________________
int SizeOfH(TObject *to)  
{
    TClass *kl = to->IsA();
    if (!kl) return 0;
    if (!kl->InheritsFrom(TH1::Class())) return 0;     
    int s0 = kl->Size();
    TH1 *h = (TH1 *)to;
    int nbins = h->GetNbinsX()*h->GetNbinsY()*h->GetNbinsZ();
    int szw = 0;
         if (kl->InheritsFrom("TArrayC")) szw=1;
    else if (kl->InheritsFrom("TArrayS")) szw=2;
    else if (kl->InheritsFrom("TArrayI")) szw=4;
    else if (kl->InheritsFrom("TArrayF")) szw=4;
    else if (kl->InheritsFrom("TArrayD")) szw=8;
    return s0 + nbins*szw;
}
#endif //-__CINT__
