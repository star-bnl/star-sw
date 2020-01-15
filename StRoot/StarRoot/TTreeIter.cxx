// Author: Victor Perev   08/04/01

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TTreeIter                                                            //
//                                                                      //
// is a helper class for TTree.                          		//
// It is analysis tool to TTree.                                      	//
//  Functionality is similar to TTree::MakeClass()                      //
//  But:                                                                //
//  1. user do not need to create special class, TTreeIter is           //
//     universal;                                                       //
//                                                                      //
//  2. AUTOMATICALLY, only branches wich user needs to use are READ.    //
//     For complicated TTree it is much faster.                         //
//                                                                      //
//                                                                      //
//  Example (see tutorials/tree4.C) :                                                            //
//    TFile f("t4.root");                                           	//
//    TTree* t4 = (TTree*)f.Get("t4");              			//
//    TTreeIter TH(t4);                                                 //
//                                                                      //
// init user variables							//
//    const Float_t  &temp = TH("fTemperature");   //temperature	//
//    const Int_t    &ntrk = TH("fTracks");        //size of clone array//
//    const Float_t* &pz   = TH("fTracks.fPz");	   //pz array     	//
//                                                                      //
//    TH1F *hz = new TH1F("PX","Px distr", 100,-.5,.5)        		//
//  //loop								//
//    while(TH.Next()) {                                                //
//                                                                      //
//      if (temp >100 ) continue;   // too hot                        	//
//      for (int itr=0; itr<ntrk; itr++) {hz->Fill( pz[itr] );}         //
//    }                                                                 //
//    TH.Reset(); //ready for next loop                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "TROOT.h"

#if ROOT_VERSION_CODE < ROOT_VERSION(3,00,0)
#define __OLDROOT__
#endif

#include "TFile.h"
#include "TKey.h"
#include "TTree.h"
#include "TChain.h"
#include "TBranch.h"
#ifndef __OLDROOT__
#include "TBranchElement.h"
#include "TFriendElement.h"
#endif
#include "TLeaf.h"
#include "TStreamerInfo.h"
#include "TStreamerElement.h"
#include "TTreeIter.h"
#include "TList.h"
#include "TObjArray.h"
#include "TNamed.h"
#include "TSystem.h"
#include "TRegexp.h"
#include "TError.h"
#include "TDirIter.h"

enum ETTI { kUnknown=0
     ,kChar   =  1,  kShort   =  2,  kInt     =  3,  kLong    =  4,  kFloat = 5, kDouble  =  8,kDouble32 = 9
     ,kUChar  = 11,  kUShort  = 12,  kUInt    = 13,  kULong   = 14};

const char* NTTI[] = {"Unknown"
,"Char_t"	 ,"Short_t"	   ,"Int_t"	   ,"Long_t"	    ,"Float_t"
,"_______"	 ,"_______" 	   ,"Double_t"	   ,"Double32_t"    ,"_______"
,"UChar_t"	 ,"UShort_t"	   ,"UInt_t"       ,"ULong_t"	    ,"_______" 
,"_______"       ,"_______"        ,"_______"      ,"_______"       ,"_______"      
,"Char_t*"	 ,"Short_t*"	   ,"Int_t*"	   ,"Long_*t"	    ,"Float_t*"
,"_______*"	 ,"_______*"	   ,"Double_t*"	   ,"_______*"	    ,"_______*"
,"UChar_t*"	 ,"UShort_t*"	   ,"UInt_t*"	   ,"ULong_t*"	    ,"_______*"
,"_______"       ,"_______"        ,"_______"      ,"_______"       ,"_______"      
,"char"  	 ,"short"  	   ,"int"  	   ,"long"  	    ,"float"  
,"_____"	 ,"_____"	   ,"double"  	   ,"_____"  	    ,"_____"  
,"uchar"  	 ,"ushort"  	   ,"uint"         ,"ulong"  	    ,"_____"   
,"_____"         ,"_____"          ,"_____"        ,"_____"         ,"_____"        
,"char*"  	 ,"short*"  	   ,"int*"  	   ,"long_*t"	    ,"float*"  
,"_____*"  	 ,"_____*"  	   ,"double*"  	   ,"_____*"  	    ,"_____*"  
,"uchar*"  	 ,"ushort*"  	   ,"uint*"  	   ,"ulong*"  	    ,"_____*"  
,"_____"         ,"_____"          ,"_____"        ,"_____"         ,"_____"        
,"char"  	 ,"short"  	   ,"int"  	   ,"long"  	    ,"float"  
,"_____"	 ,"_____"	   ,"double"  	   ,"_____"  	    ,"_____"  
,"unsigned char" ,"unsigned short" ,"unsigned int" ,"unsigned long" ,"_____"   
,"_____"         ,"_____"          ,"_____"        ,"_____"         ,"_____"        
,"char*"  	 ,"short*"  	   ,"int*"  	   ,"long_*t"	    ,"float*"  
,"_____*"  	 ,"_____*"  	   ,"double*"  	   ,"_____*"  	    ,"_____*"  
,"unsigned char*","unsigned short*","unsigned int*","unsigned long*","_____*"  
,"_____"         ,"_____"          ,"_____"        ,"_____"         ,"_____"        
,0};



//______________________________________________________________________________
void TTreeIterCast::Set(void* v,Int_t t,const char* name)
{
if (t==kDouble32   ) t = kDouble;
if (t==kDouble32+20) t = kDouble+20;
fV=v;fT=t;fN=name;
}
//______________________________________________________________________________
void *TTreeIterCast::Addr(Int_t outType)
{
  void *v = (outType>20) ? fV: *((void**)fV);
  if (fT+20 == outType) {
     Warning("Addr","*** Possible wrong cast:variable %s %s to %s ACCEPTED ***",
             TTreeIter::TypeName(fT),fN,TTreeIter::TypeName(outType));
  }
  else if (fT != outType) {
     Error("Addr","*** Wrong cast:variable %s %s to %s IGNORED ***",
           TTreeIter::TypeName(fT),fN,TTreeIter::TypeName(outType));
     v = 0;
  }
  if (!v) fE[0]++;
//  printf("TTreeIterCast::Addr = %p\n",v);
  return v;
}

//______________________________________________________________________________
TTreeIterCast::operator const Char_t		&()
{return *((const Char_t*)Addr(kChar));}
//______________________________________________________________________________
TTreeIterCast::operator const Short_t 	&()
{return *((const Short_t*)Addr(kShort));}
//______________________________________________________________________________
TTreeIterCast::operator const Int_t  		&()
{return *((const Int_t*)Addr(kInt));}
//______________________________________________________________________________
TTreeIterCast::operator const Long_t  	&()
{return *((const Long_t*)Addr(kLong));}
//______________________________________________________________________________
TTreeIterCast::operator const Float_t 	&()
{return *((const Float_t*)Addr(kFloat));}
//______________________________________________________________________________
TTreeIterCast::operator const Double_t 	&()
{return *((const Double_t*)Addr(kDouble));}

//______________________________________________________________________________
TTreeIterCast::operator const UChar_t 	&()
{return *((const UChar_t*)Addr(kUChar));}
//______________________________________________________________________________
TTreeIterCast::operator const UShort_t 	&()
{return *((const UShort_t*)Addr(kUShort));}
//______________________________________________________________________________
TTreeIterCast::operator const UInt_t  	&()
{return *((const UInt_t*)Addr(kUInt));}
//______________________________________________________________________________
TTreeIterCast::operator const ULong_t  	&()
{return *((const ULong_t*)Addr(kULong));}

//______________________________________________________________________________
TTreeIterCast::operator const Char_t	       *&()
{return *((const Char_t**)Addr(kChar+20));}
//______________________________________________________________________________
TTreeIterCast::operator const Short_t        *&()
{return *((const Short_t**)Addr(kShort+20));}
//______________________________________________________________________________
TTreeIterCast::operator const Int_t  	       *&()
{return *((const Int_t**)Addr(kInt+20));}
//______________________________________________________________________________
TTreeIterCast::operator const Long_t         *&()
{return *((const Long_t**)Addr(kLong+20));}
//______________________________________________________________________________
TTreeIterCast::operator const Float_t        *&()
{return *((const Float_t**)Addr(kFloat+20));}
//______________________________________________________________________________
TTreeIterCast::operator const Double_t       *&()
{return *((const Double_t**)Addr(kDouble+20));}

//______________________________________________________________________________
TTreeIterCast::operator const UChar_t        *&()
{return *((const UChar_t**)Addr(kUChar+20));}
//______________________________________________________________________________
TTreeIterCast::operator const UShort_t       *&()
{return *((const UShort_t**)Addr(kUShort+20));}
//______________________________________________________________________________
TTreeIterCast::operator const UInt_t         *&()
{return *((const UInt_t**)Addr(kUInt+20));}
//______________________________________________________________________________
TTreeIterCast::operator const ULong_t        *&()
{return *((const ULong_t**)Addr(kULong+20));}




//______________________________________________________________________________
class TTreeIterMem  : public TNamed { //special class for TTreeIter
public:
   Int_t  fType;
   Int_t  fUnits;
   Int_t  fSize;
   TString fTyName;
   Char_t *fMem;

public:
   TTreeIterMem(const char *name,Int_t type,Int_t units,const char *tyName);
  ~TTreeIterMem(){ delete [] fMem;}
   void 	**Alloc(int units=-1);
   void 	**GetMem(){return (void**)&fMem;}   
};
//______________________________________________________________________________
TTreeIterMem::TTreeIterMem(const char *name,Int_t type,Int_t units,const char *tyName)
                :TNamed(name,"")
{
  fType   = type;
  fTyName = tyName;
  fUnits  = units;
  fSize   = 0;
  fMem    = 0;
  Alloc();
}

//______________________________________________________________________________
void **TTreeIterMem::Alloc(int units)
{
   if (units>-1) fUnits = units;
   if (!fUnits) fUnits=1;
   delete [] fMem; fMem=0;
   TClass *kl = 0;
   int uSize  = sizeof(void*);
   if (fType) {
     fSize = TTreeIter::TypeSize(fType)*fUnits;
   } else { //real class
     if (fTyName[fTyName.Length()-1]!='*') { // not a pointer 
       kl = gROOT->GetClass(fTyName);
       if (!kl) {
         Warning("Alloc","No dictionary for class %s",fTyName.Data());
         return 0;}
       uSize = kl->Size();
     }
     fSize = uSize*fUnits;
   }     
   fMem = new char[fSize+8];
   memset(fMem,0,fSize);
   if (kl){for (char *cc=fMem; cc < fMem+fSize; cc+=uSize){kl->New((void*)cc);}}
   strcpy(fMem+fSize,"Perev");
   return (void**)&fMem;
} 

//______________________________________________________________________________
ClassImp(TTreeIter)
//______________________________________________________________________________

TTreeIter::TTreeIter(TTree *tree):fCast(&fNErr)
{
  fNFiles = 0;
  fTree   = 0;
  if (tree) {
    fTree  = new TChain(tree->GetName());
    if (tree->IsA() == TChain::Class()) fTree->Add((TChain*)tree);
  }
  Init();
}
//______________________________________________________________________________

TTreeIter::TTreeIter(TChain *tree):fCast(&fNErr)
{
  fNFiles = 0;
  fTree   = 0;
  fTree  = tree;
  Init();
}
//______________________________________________________________________________

TTreeIter::TTreeIter(const char *treeName):fCast(&fNErr)
{
  fTree = 0;
  fNFiles = 0;
  if (treeName && treeName[0] && treeName[0]!=' ') fTree  = new TChain(treeName);
  Init();
}
//______________________________________________________________________________

void TTreeIter::Init()
{
  fCint  = 0;
  fNErr  = 0;
  fEntry = 0;
  fUnits = 0;
  if (fTree==0) return;
#ifndef __OLDROOT__
  fTree->SetMakeClass(1);
#endif
  fTree->SetBranchStatus("*",0);
  fTree->SetNotify(this);

}
//______________________________________________________________________________
TTreeIter::~TTreeIter()
{
  fEntry = 0;
  delete fTree;
  fMemList.Delete();
  
}

//______________________________________________________________________________
void TTreeIter::GetInfo(const TBranch *tbp, const char *&tyName
                        ,Int_t &units,void  *&add, Int_t &brType) 
{
   tyName = 0;
   brType = 0;
   TBranch *tb = (TBranch*)tbp;
   add = tb->GetAddress();
   units = 0;
   char *nxt=0;
   const char *des = strchr(tb->GetName(),'[');
   if (des) {
     units = 1;
     while(des){
       int ii = strtol(des+1,&nxt,10);
       if (des+1 != nxt) 	units *=ii;
       if ( !*nxt) 		break;
       des = nxt;
   } }

   int max = 0;
   int kase = 0;
   if (strcmp(tb->ClassName(),"TBranchElement")==0) kase = 1;
   if (strcmp(tb->ClassName(),"TBranchClones" )==0) kase = 2;
   switch (kase) {
#ifndef __OLDROOT__
     case 1: {TBranchElement *te = (TBranchElement*)tb;
              max = te->GetMaximum();
              brType  = te->GetType();
              TString ts(te->GetName());
              int i = 0;              
              while ((i=ts.Index("."))>=0) { ts.Replace(0,i+1,"");}
              i=ts.Index("["); if (i>=0) ts.Replace(i,9999,"");
              TStreamerInfo *si = te->GetInfo();
              assert(si); 
              TStreamerElement *se = si->GetStreamerElement(ts.Data(),i);
              if (!se) { tyName = si->GetName(); }
              else     { tyName = se->GetTypeName();}
             }
              if (strcmp("TClonesArray",tyName)==0 && tb->GetSplitLevel()) tyName=0;
              break;
#endif
     case 2: max = 0; tyName = "Int_t"; return;

     case 0:
     TLeaf *lf = (TLeaf*)tb->GetListOfLeaves()->First();
     TLeaf *lc = 0;
     if (lf) lc = lf->GetLeafCount();
     if (lc) max = lc->GetMaximum();

   }
   if (max) {if (!units) units = 1; units *= max;}
   if (brType==3) units=0;
   if (tyName) return;

   TObjArray *lfList = tb->GetListOfLeaves();
   TLeaf *tl = (lfList) ? (TLeaf*)lfList->UncheckedAt(0):0;
   tyName= (tl) ? tl->GetTypeName():0;
}
//______________________________________________________________________________
TBranch *TTreeIter::GetBranch(int idx) const
{
   TObjArray *ll = fTree->GetListOfLeaves();
   if (!ll) return 0;
   if (idx>ll->GetLast()) return 0;

   return ((TLeaf*)ll->At(idx))->GetBranch();
}

//______________________________________________________________________________
void **TTreeIter::Void(const TString varname)
{
   fCint = 1;
   return Void(varname.Data());
}
//______________________________________________________________________________
void **TTreeIter::Void(const char *varname)
{
   fCast.Set(0,0,varname);
//   TBranch *br = GetBranch(varname);
   TBranch *br = fTree->GetBranch(varname);
   if (br && strcmp(br->ClassName(),"TBranchClones")==0) {//wrong object
     TString ts(varname); ts+="_";
     br = fTree->GetBranch(ts.Data());
   }
   if (!br) {
     Warning("operator()","Branch %s NOT FOUND",varname);
     return 0;
   }

   void *addr,**pddr;
   const char *tyName;
   Int_t brType;
   GetInfo(br,tyName,fUnits,addr,brType);  

   int tyCode = TypeCode(tyName);
   if (!tyCode) {
//   Warning("operator()","Branch %s non basic %s type,Units %d brType %d",varname,tyName,fUnits,brType);
   }
   TTreeIterMem *mem;
   mem = (TTreeIterMem*)fMemList.FindObject(br->GetName());
   if (!mem) {
     mem = new TTreeIterMem(br->GetName(),tyCode,fUnits,tyName);
     fMemList.Add(mem);
     pddr = mem->GetMem();
     fTree->SetBranchStatus(br->GetName(),1);
     br->SetBit(1);
     fTree->SetBranchAddress(br->GetName(),*pddr);
     fBraList.Add(br);
   } else {
     pddr = mem->GetMem();
   }
//    addr = *pddr;
//    if (fUnits) { if(tyCode) tyCode+=20; addr = (void*)pddr;}
//    fCast.Set(addr,tyCode,varname);
   if (fUnits) { if(tyCode) tyCode+=20;}
   fCast.Set(pddr,tyCode,varname);

   return pddr;
}     
//______________________________________________________________________________
TTreeIterCast &TTreeIter::operator() (const TString varname)
{
   fCint = 1;
   return operator() (varname.Data());
}
//______________________________________________________________________________
TTreeIterCast &TTreeIter::operator() (const char *varname)
{
   Void(varname);
   void *addr = fCast.Addr();
   if (fCint)  {
     fCint = 0;
     TTreeIterCast *v =(TTreeIterCast*)addr;
//     printf("CINT Address %p\n",(void*)v);
     return *v;//CINT workaround
   }
   return fCast;
}     
//______________________________________________________________________________
Int_t TTreeIter::Next(Int_t entry)
{
  if (fNErr) {
    Warning("Next","It was %d errors in Init. Loop ignored",fNErr);
    fEntry=0; return 0;}
  if (!TestBit(1)) { SetBit(1); Notify();}

  int ientry =  (entry >= 0) ? entry:fEntry++;

  Int_t ans = 0;

  ans = fTree->GetEntry(ientry);
#if 0
     int n = fBraList.GetEntriesFast();
     for (int i=0;i<n;i++) {
       TBranch *b = (TBranch*)fBraList.UncheckedAt(i);
       ans +=b->GetEntry(ientry); 
     }
#endif //0
  assert(!IsCorrupted());
  if (ans) return ans;
  fEntry=0;
  return 0;

}
//______________________________________________________________________________
Bool_t TTreeIter::Notify()
{
  const char *tyName;
  Int_t units,brType;
  void  *add;
  assert(!IsCorrupted());
  fTree->SetBranchStatus("*",0);
  fBraList.Clear();
  int n = fMemList.GetEntriesFast();
  for (int i=0;i<n;i++) {
    TTreeIterMem *t = (TTreeIterMem*)fMemList.UncheckedAt(i);
    fTree->SetBranchStatus(t->GetName(),1);
    TBranch *b = fTree->GetBranch(t->GetName());
    assert(b);
    GetInfo(b,tyName,units,add,brType);
    void **pddr = t->GetMem();
      if (units > t->fUnits) {
      pddr = t->Alloc(units);
    }
    fTree->SetBranchAddress(t->GetName(),*pddr);
  }

  TBranch *br=0;
  int added = 1;
  while(added) {
    added = 0;
    for (int idx=0;(br=GetBranch(idx));idx++) {
      if (br->TestBit(kDoNotProcess)) 		continue;
      if (fMemList.FindObject(br->GetName())) 	continue;
      added++;

      GetInfo(br,tyName,units,add,brType);
      if (brType==3 || brType==4 ) {//add counter
        (*this)(br->GetName());   
        printf("Branch %s activated\n",br->GetName());
        					continue;
      }
      TObjArray *brl = br->GetListOfBranches();
      if (brl && brl->GetEntriesFast()) {//Node
        printf("Node Branch %s ignored\n",br->GetName());
        added--;
        					continue;
      }

      {// We are here because of ROOT bug. Do workaround
        fTree->SetBranchStatus(br->GetName(),0);
        br->SetBit(kDoNotProcess);		
        printf("Branch %s desactivated\n",br->GetName());
        					continue;
      }  
    }
  }
  fMemList.Sort();
  n = fMemList.GetEntriesFast();
  for (int i=0;i<n;i++) {
    TTreeIterMem *t = (TTreeIterMem*)fMemList.UncheckedAt(i);
    TBranch *b = fTree->GetBranch(t->GetName());
    assert(b);
    fBraList.Add(b);
  }
  return 0;
}
//______________________________________________________________________________
const char *TTreeIter::IsCorrupted() const
{

  int n = fMemList.GetEntriesFast();
  assert(n>=0 && n<10000);
  for (int i=0;i<n;i++) {
    TTreeIterMem *t = (TTreeIterMem*)fMemList.UncheckedAt(i);
    assert(t);
    assert(t->fMem);
    assert(t->fSize>0);

    char *perev = t->fMem+t->fSize;
    if (strcmp(perev,"Perev") ==0 ) continue;
    Error("IsCorrupted","Branch=%s Units=%d Mem=%p ***\n",t->GetName(),fUnits,perev);
    return t->GetName();
  }
  return 0;
}
//______________________________________________________________________________
void TTreeIter::ls(const TObjArray *brList,Int_t lvl,Option_t* option)
{
   TBranch *branch;
   if (!brList) return;
   
   Int_t nb = brList->GetEntriesFast();
   for (int iBr=0;iBr<nb;iBr++) {
      branch = (TBranch*)brList->UncheckedAt(iBr);
      if (!branch)	continue;
      Print(branch,lvl,option);
      ls(branch->GetListOfBranches(),lvl+1,option);
   }
}
//______________________________________________________________________________
void TTreeIter::ls(const TTree *ttp, Option_t* option)
{
  TTree *tt = (TTree *)ttp;
  if (!tt) return;
  ls(tt->GetListOfBranches(),0,option);
}
//______________________________________________________________________________
void TTreeIter::ls(Option_t* option) const
{
  if(option && strstr(option,"fil")) {
    if(!fTree) return;
    if(!fTree->GetListOfFiles()) return;
    fTree->GetListOfFiles()->ls();
    return;
  }	
  ls(fTree,option);
}
//______________________________________________________________________________
void TTreeIter::Print(Option_t* option) const
{
   ls(fTree,option);
}
//______________________________________________________________________________
void TTreeIter::Print(const TBranch *tb,Int_t lvl, Option_t* option)
{
   const char *tyName;
   Int_t units,brType;
   char active[2]={0,0};
   void  *add;

   GetInfo(tb,tyName,units,add,brType);
   active[0]='*';
   if (tb->TestBit(kDoNotProcess)) active[0]=0;
   
   printf("%10p(%10p)  -  ",(void*)tb,(void*)add);
   for (int i=0;i<lvl;i++){printf("    ");}
   
   printf("%s%s(%s)",active,tb->GetName(),tb->ClassName());
   
   printf("\t //  Max=%d Type=%s,brType=%d",units,tyName,brType);
   printf("\n");
}
//______________________________________________________________________________
const char* TTreeIter::TypeName(Int_t ity)
{
  return NTTI[ity];
}
//______________________________________________________________________________
Int_t TTreeIter::TypeSize(Int_t ity)
{
  int t = ity%10;
  switch(t) {
     case kChar:; case kShort:; return t;
     case kInt:   		return sizeof(Int_t);
     case kLong:   		return sizeof(Long_t);
     case kFloat: 		return sizeof(Float_t);
     case kDouble: 		return sizeof(Double_t);
     case kDouble32: 		return sizeof(Double_t);
     default: 			return 0;
  }
}

//______________________________________________________________________________
Int_t  TTreeIter::TypeCode(const char *typeName)
{
   for (int i=1; NTTI[i]; i++) {if (strcmp(typeName,NTTI[i])==0) return i%20;}
// printf("*** TypeCode ERROR: %s is UNKNOWN ***\n",typeName);
   return 0;
} 
//______________________________________________________________________________
void TTreeIter::Streamer(TBuffer &) {assert(0);}
//_____________________________________________________________________________
Int_t TTreeIter::AddFile(const Char_t *file)
{
   const char* fullname;
   int num = 0;
   TDirIter dirIter(file);
   while((fullname=dirIter.NextFile())) { 
     fNFiles++; num++;
     printf("%04d -  TTreeIter::AddFile %s\n",fNFiles,fullname);
     if (fTree == 0) WhichTree(fullname);
     if (fTree) fTree->Add(fullname);
   }
   
  Init();
  return num;
}
//______________________________________________________________________________
void TTreeIter::WhichTree(const char *fileName)
{
   TString fileNameS = fileName;
   gSystem->ExpandPathName(fileNameS);
//   printf(" fileName = %s\n",fileNameS.Data());
   
   

   TFile *tfile = TFile::Open(fileNameS.Data());
   if (! tfile || tfile->IsZombie()) {
     printf("*** Can NOT open %s ***\n",fileNameS.Data());
     return;}
   
   TList *keyList = tfile->GetListOfKeys();
   TListIter NextKey(keyList);
   TKey *key; const char *ttName=0;
   while ( (key = (TKey*)NextKey()) ) 
   { 
     if (strcmp("TTree"  ,key->GetClassName())!=0
     &&  strcmp("TNtuple",key->GetClassName())!=0) continue;
     ttName = key->GetName(); break;
   }  
   if (ttName==0) return;
   printf(" Got TTree = %s\n",ttName);

   fTree = new TChain(ttName);
   delete tfile;
   Init();
}



