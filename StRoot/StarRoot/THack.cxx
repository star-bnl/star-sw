/***************************************************************************
 *
 * $Id: THack.cxx,v 1.2 2004/01/27 02:53:32 perev Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "THack.h"
#include "TClonesArray.h"

class myClonesArray :public TClonesArray
{
public:
      myClonesArray(){;}
TObject **GetCont(TClonesArray *klon);
TObject **GetKeep(TClonesArray *klon);
};
//______________________________________________________________________________
TObject **myClonesArray::GetCont(TClonesArray *klon)
{
   int offsetC = (char*)&fCont - (char*)this;   
return *((TObject***)((char*)klon+offsetC));
}   

//______________________________________________________________________________
TObject **myClonesArray::GetKeep(TClonesArray *klon)
{
   int offsetK = (char*)&fKeep - (char*)this;   
   TObjArray *k = *((TObjArray**)((char*)klon+offsetK));
   int offsetC = (char*)&fCont - (char*)this;   
   return  *(TObject***)((char*)k+offsetC);
}   

//______________________________________________________________________________
//______________________________________________________________________________
void THack::DeleteClonesArray(TClonesArray *clone)
{
   myClonesArray mycl;

   TObject **keep = mycl.GetKeep(clone);
   TObject **cont = mycl.GetCont(clone);
   int sz = clone->Capacity();
   int i=0;
   if (keep) {
     for (i=0;i<sz;i++) {
       if(!keep[i]) break;
       cont[i]=keep[i];
     }
   }
   delete clone;
}

//______________________________________________________________________________
void THack::ClearClonesArray(TClonesArray *clone)
{
   myClonesArray mycl;

   TObject **keep = mycl.GetKeep(clone);
// TObject **cont = mycl.GetCont(clone); 
   int sz = clone->Capacity();
   int i=0;
   clone->Delete();
   if (keep) {
     TObject *to;
     for (i=0;i<sz;i++) {
       to = keep[i];
       if(!to) break;
       if (to->TestBit(TObject::kNotDeleted)) 	continue;//alive
       clone->New(i);
     }
   }
   clone->Clear();
}













