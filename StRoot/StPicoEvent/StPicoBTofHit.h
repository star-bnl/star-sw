#ifndef StPicoBTofHit_h
#define StPicoBTofHit_h

#include "TObject.h"

class StPicoBTofHit : public TObject
{
public:
  StPicoBTofHit();
  StPicoBTofHit(int);
  virtual ~StPicoBTofHit();

  virtual void Print(const Char_t* option = "") const;  ///< Print trigger info

  Int_t   id() const;
  Int_t   tray() const;
  Int_t   module() const;
  Int_t   cell() const;

protected:
  Short_t mId;     // (tray-1)*192+(module-1)*6+(cell-1):

  ClassDef(StPicoBTofHit, 1)
};

inline Int_t StPicoBTofHit::id() const { return mId; }
inline Int_t StPicoBTofHit::tray() const { return mId / 192 + 1; }
inline Int_t StPicoBTofHit::module() const { return (mId % 192) / 6 + 1; }
inline Int_t StPicoBTofHit::cell() const { return mId / 6 + 1; }
#endif
