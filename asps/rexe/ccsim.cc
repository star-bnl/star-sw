/*-- Author :    Valeri fine   28/12/98*/
/*****************************************************/
/*        a'la   S T A F   i n t e r f a c e         */
/*****************************************************/
#include "PAM.h"

#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "St_Table.h"
 
//__________________________________________________________________
#define tdm_get_ccount_      F77_NAME(tdm_get_ccount,TDM_GET_CCOUNT)
#define tdm_get_column_      F77_NAME(tdm_get_column,TDM_GET_COLUMN)
//__________________________________________________________________
 
//__________________________________________________________________
extern "C" int type_of_call tdm_get_ccount_(TABLE_HEAD_ST* pTab)
 
{ unsigned ccount;
  St_Table *table = (St_Table *)pTab->dsl_pointer;
  if (!table) return 0;
  TClass *classPtr = table->GetRowClass();
  ccount = classPtr->GetNdata();
  return (int) ccount;
}
//__________________________________________________________________
 
extern "C" int type_of_call tdm_get_column_(TABLE_HEAD_ST* pTab,int* k,char* c,char* d,
                                unsigned* l, unsigned* e, unsigned* m)
{ char *cc; char *dd;

   St_Table *table = (St_Table *)pTab->dsl_pointer;
   if (!table) return 0;
   TClass *classPtr = table->GetRowClass();

   if (classPtr == 0) return 0;
   if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
   if (!classPtr->GetNdata()) return 0;

   TList *list = classPtr->GetListOfRealData();
   TDataMember *member = (TDataMember*) (list->At(*k));
   strcpy(d,member->GetTypeName());
   strcpy(c,member->GetTypeName());
   *m = member->GetArrayDim();
   TDataType *membertype = member->GetDataType();
   *l = membertype->Size();
   *e = 1;

  return strlen(c)+1;
}

/**************************************************************************/

