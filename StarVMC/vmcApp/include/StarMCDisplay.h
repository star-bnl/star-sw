// $Id: StarMCDisplay.h,v 1.1 2004/07/13 19:03:22 potekhin Exp $
// $Log: StarMCDisplay.h,v $
// Revision 1.1  2004/07/13 19:03:22  potekhin
// Initial check in
//

#ifndef STARMCDISPLAY_H
#define STARMCDISPLAY_H

#include <TNamed.h>
#include <TCanvas.h>

class StarMCDisplay
{
 public:
  StarMCDisplay();
  virtual ~StarMCDisplay() {};

  virtual void DrawVolume(void);
  virtual void DrawHits  (TObjArray* h_) const ;
  
 private:
  TCanvas          _c;
  ClassDef(StarMCDisplay,1)  // display
};

// inline functions


#endif //STARMCDISPLAY_H

