#ifndef StPicoBTofHit_hh
#define StPicoBTofHit_hh

class StPicoDst;

#include "TObject.h"
#include "stdio.h"

class StPicoBTofHit : public TObject {
 public:
  StPicoBTofHit();
  ~StPicoBTofHit();
  StPicoBTofHit(int);
  void    Clear(const Option_t *opt="");
  virtual void Print(const Char_t *option = "") const;  ///< Print trigger info
 
  Int_t   id() const             { return (Int_t)mId; }
  Int_t   tray() const           { return (Int_t)mId/192 + 1; }
  Int_t   module() const         { return ((Int_t)mId%192)/6 + 1; }
  Int_t   cell() const           { return (Int_t)mId/6 + 1; }
    

 protected:
  Short_t mId;     // (tray-1)*192+(module-1)*6+(cell-1):

  friend class StPicoDst;

  ClassDef(StPicoBTofHit, 1)
};

#endif
