#ifndef ROOT_TMyQButton
#define ROOT_TMyQButton
#include "Rtypes.h"

class QPushButton;
class TVirtualPad;

class TMyQButton {
  private:
     QPushButton *fMyButton;
  public:
      TMyQButton(const char*name="");
      virtual ~TMyQButton();
      void SetSize(UInt_t w, UInt_t h);
      void Show(Bool_t show=kTRUE);
      void SetText(const char *text);
      void SetOn(Bool_t on=kTRUE);
      void SetStyle(const char * style);
      void CatchPad(TVirtualPad *pad=0);
};

#endif
