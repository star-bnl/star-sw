#ifndef STAR_St_xdfin_Maker
#define STAR_St_xdfin_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_xdfin_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "St_XDFFile.h"

class St_xdfin_Maker : public StMaker {
private:
TString fFileName;
St_XDFFile fXdfin;
int m_InitDone;
public:
   St_xdfin_Maker(const char *name,const char *inputFile=0);
   virtual       ~St_xdfin_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_xdfin_Maker, 0)   //StAF chain virtual base class for Makers
};

#endif
