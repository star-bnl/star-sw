//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// header file for class StTpcEvalOutput
//-------------------------------------------------

#ifndef StTpcEvalOutput_H
#define StTpcEvalOutput_H

#include "StObject.h"

class TFile; 

class StTpcEvalOutput : public StObject {
public:
    StTpcEvalOutput();
    void Open(); 
    void Write(); 
    int  Write(const char* k, int i1, int i2){return TObject::Write(k, i1, i2);}//WarnOff
    void Close(); 
    TFile* mOutputFile; //!
private:
    ClassDef(StTpcEvalOutput,1)
};

#endif
