// Author: Victor Perev   08/04/01


#ifndef ROOT_TAttr
#define ROOT_TAttr


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TAttr                                                          //
// General attribute collection                                                                    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TNamed.h"
#include "TString.h"

class TAttr: public TList
{
public:
            TAttr(const char *name="");
           ~TAttr();
void        SetAttr(const char *key,const char* val);
void        SetAttr(const char *key,int         val);
void        SetAttr(const char *key,UInt_t      val);
void        SetAttr(const char *key,double      val);
void        RemAttr(const char *key                )	{return SetAttr(key,".remove");}
int         IAttr(const char *key) const;
UInt_t      UAttr(const char *key) const;
double      DAttr(const char *key) const;
const char *SAttr(const char *key) const;
void        PrintAttr() const;
//		Data members
private:

ClassDef(TAttr,1)

};
#endif //ROOT_TAttr
