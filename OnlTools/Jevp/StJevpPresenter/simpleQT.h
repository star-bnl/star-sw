#ifndef _SIMPLEQT_H_
#define _SIMPLEQT_H_

#include "TObject.h"

class simpleQT : public TObject 
{
 private:
    simpleQT() {
    }  

 public:
    virtual ~simpleQT(){}
    static int main();
    ClassDef(simpleQT,0);
};

#endif
