// Author: Victor Perev   08/04/01


#ifndef ROOT_TTreeHelper
#define ROOT_TTreeHelper


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TTreeHelper                                                          //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TTreeIter.h"
class TTreeHelper : public TTreeIter {

public:

    TTreeHelper(TTree *tree):TTreeIter(tree){}
    TTreeHelper(const char *treeName=""):TTreeIter(treeName){}
    virtual ~TTreeHelper(){};

ClassDef(TTreeHelper,0)    
};
#endif //ROOT_TTreeHelper
