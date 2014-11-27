#include "TMyQButton.h"
#include <qpushbutton.h>

// This class alow you tocreate and manipulat 
// the QPushButton interactively

//__________________________________________________________
TMyQButton::TMyQButton(const char *name)
{
   // Create Qt QPushButton interactively
   fMyButton = new QPushButton(name,0);
}
//__________________________________________________________
TMyQButton::~TMyQButton() { delete fMyButton; }
//__________________________________________________________
void TMyQButton::SetSize(UInt_t w, UInt_t h)
{
   // Resize the Qt buttom
  fMyButton->resize(w,h);
}
//__________________________________________________________
void TMyQButton::Show(Bool_t show)
{
     // Show / hide the button
   if (show)
      fMyButton->show();
   else 
      fMyButton->hide();
}
//__________________________________________________________
void TMyQButton::SetText(const char *text)
{
  // Set change the button text
  fMyButton->setText(text);
}
//__________________________________________________________
void TMyQButton::SetOn(Bool_t on) {
  fMyButton->setOn(on);
}
//__________________________________________________________
void TMyQButton::SetStyle(const char * style) {
   // The the look and feel for this button.
   // the possible styles are defined by the local Qt inmstallation
   // For example the possible style can be
   //   "window"   "motif",  "cde"        "motifplus", "platinum"
   //   "sgi"      "compact" "windowsxp"  "aqua"       "macintosh
  fMyButton->setStyle(style);
}
//__________________________________________________________
void TMyQButton::CatchPad(TVirtualPad *)
{

}
