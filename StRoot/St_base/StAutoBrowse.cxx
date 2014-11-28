
#include <stdio.h>
#include <stdlib.h>
#include "TROOT.h"
#include "TClass.h"
#include "TBaseClass.h"
#include "TDataMember.h"
#include "TMethod.h"
#include "TMethodArg.h"
#include "TDataType.h"
#include "Api.h"
#include "TBrowser.h"
#include "TMemberInspector.h"
#include "TError.h"
#include "StAutoBrowse.h"

class StAutoInspector : public TMemberInspector {
public:
StAutoInspector(TBrowser *b){fBrowser=b;fCount=0;};
virtual ~StAutoInspector(){};
virtual void Inspect(TClass* cl, const char* parent, const char* name, const void* addr);

Int_t fCount;
TBrowser *fBrowser;
};      
//______________________________________________________________________________
void StAutoInspector::Inspect(TClass* kl, const char* tit , const char* name, const void* addr)
{
  if(tit && strchr(tit,'.'))	return ;
  if (fCount && !fBrowser) return;

  TString ts;

  if (!kl) return;
  if (*(kl->GetName()) == 'T') return;
  if (*name == '*') name++;
  int ln = strcspn(name,"[ ");
  TString iname(name,ln);
  
  G__ClassInfo *classInfo = (G__ClassInfo*) kl->GetClassInfo();  	
  if (!classInfo)		return;
  G__ClassInfo &cl = *classInfo;


// 		Browse data members
  G__DataMemberInfo m(cl);
  TString mname;

  int found=0;
  while (m.Next()) {	// MemberLoop
     mname = m.Name();
     mname.ReplaceAll("*","");
     if ((found = (iname==mname))) break;
  }     
  assert(found);

  // we skip: non TObjects
  //  - the member G__virtualinfo inserted by the CINT RTTI system

  long prop = m.Property() | m.Type()->Property();
  if (prop & G__BIT_ISSTATIC) 	return;
  if (prop & G__BIT_ISFUNDAMENTAL) 	return;
  if (prop & G__BIT_ISENUM) 		return;
  if (strcmp(m.Type()->Fullname(),"TObject") && !m.Type()->IsBase("TObject"))
  					return;
  if (mname == "G__virtualinfo")	return;

  int  size = sizeof(void*);
  if (!(prop&G__BIT_ISPOINTER)) size = m.Type()->Size(); 

  int nmax = 1;
  if (prop & G__BIT_ISARRAY) {
    for (int dim = 0; dim < m.ArrayDim(); dim++) nmax *= m.MaxIndex(dim);
  }

  for(int i=0; i<nmax; i++) {
    char *ptr = (char*)addr + i*size;
    TObject *obj = (prop&G__BIT_ISPOINTER) ? *((TObject**)ptr) : (TObject*)ptr;
    if (!obj) 		continue;
    fCount++;
    if (!fBrowser) 	return;
    const char *bwname = obj->GetName();
    if (!bwname[0] || strcmp(bwname,obj->ClassName())==0) {
      bwname = name;
      if (strcmp(bwname,"fOrBrowser")==0) {
        ts.Replace(0,999,tit,strlen(tit)-1);
        bwname = (const char*)ts;
      } else {
	int l = strcspn(bwname,"[ ");
	if (bwname[l]=='[') {
          char cbuf[12]; sprintf(cbuf,"[%02d]",i);
          ts.Replace(0,999,bwname,l);
          ts += cbuf;
          bwname = (const char*)ts;
	}
      }  
    }  
   
    fBrowser->Add(obj,bwname);
  }

}    

//______________________________________________________________________________
Int_t StAutoBrowse::Browse(TObject *obj,TBrowser *browser)
{
  if(!obj)	return 0;
  StAutoInspector insp(browser);
#if ROOT_VERSION_CODE < 334597
  char cbuf[1000]; *cbuf=0;

  ((TObject*)obj)->ShowMembers(insp,cbuf);
#else
  ((TObject*)obj)->ShowMembers(insp);
#endif
  return insp.fCount;
}
