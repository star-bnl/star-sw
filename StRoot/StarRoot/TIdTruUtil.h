// Author: Victor Perev   08/04/01


#ifndef ROOT_TIdTruUtil
#define ROOT_TIdTruUtil


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TIdTruUtil                                                           //
// IdTruth utility                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TNamed.h"
#include <map>
class TIdTruUtil: public TNamed
{
public:
            TIdTruUtil(const char *name="");
           ~TIdTruUtil();
void        Clear(const char* opt=0);
void        Add(int idTru,int qua=100);
int         Size() const {return mSize;}
int         GetIdTru();
double      GetQua();

//		Data members
private:
void Eval();

private:
int mSize;
int mEvalted;
int mIdTru;
double mQua;
std::map      <int,double> mDetWt;
ClassDef(TIdTruUtil,0)

};
#endif //ROOT_TIdTruUtil
