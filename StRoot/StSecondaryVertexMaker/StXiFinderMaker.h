/**
  \class StXiFinderMaker
  
  StXiFinderMaker finds Xi secondary vertices

*/

#ifndef StXiFinderMaker_hh
#define StXiFinderMaker_hh

#include "StV0FinderMaker.h"

class St_exi_exipar;
class exi_exipar_st;
class StXiVertex;


class StXiFinderMaker : public StV0FinderMaker {

 public:
  StXiFinderMaker(const char* name="XiFinderMaker");
  virtual ~StXiFinderMaker();

  virtual Int_t Init();
  virtual Int_t InitRun(int runumber);
  virtual Int_t Make();

  virtual Bool_t UseV0();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StXiFinderMaker.h,v 1.4 2014/08/06 11:43:37 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 protected:
  St_exi_exipar* exipar;           //!
  exi_exipar_st* parsXi;           //!
  StXiVertex* xiVertex;            //!
  int det_id_xi;

  ClassDef(StXiFinderMaker,0)

};

#endif

//_____________________________________________________________________________
// $Id: StXiFinderMaker.h,v 1.4 2014/08/06 11:43:37 jeromel Exp $
// $Log: StXiFinderMaker.h,v $
// Revision 1.4  2014/08/06 11:43:37  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.3  2008/04/03 19:58:37  fisyak
// move parameters initialization from Init into InitRun
//
// Revision 1.2  2003/04/30 19:15:55  faivre
// Fix storage part. ITTF vs TPT Xis.
//
//
