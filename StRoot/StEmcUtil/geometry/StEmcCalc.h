/*!\Class StEmcCalc
\author Alexandre Suaide

This class is a GUI for various EMC coordinates transformations. It
transforms real eta, phi into Offline coordinates (id, module, eta division and
sub division) and Daq Ids. It works for towers and Shower Max detectors.

run StEmcUtil/geometry/macros/runCalc.C to bring the GUI up.
*/

#include <TGClient.h>
#include <TGButton.h>
#include <TGSlider.h>
#include <TGTextEntry.h>
#include <TGLabel.h>
#include <TGCanvas.h>
#include <iostream.h>
#include <fstream.h>
#include "TTimer.h"
#include "TString.h"
#include "TROOT.h"
#include "TTimer.h"
class StEmcGeom;
class StEmcDecoder;

class StEmcCalc : public TGMainFrame 
{
  private:
    TGTextButton    *button1[4],*button2[4],*help,*exit;
    TGRadioButton   *radio[4][5];
    TGTextEntry     *coord[4][5][4];       
    TGGroupFrame    *group[4];
    StEmcGeom       *geom[4];
    StEmcDecoder    *daqconv;
    
    Bool_t          radioSt[4][5];
    
    Bool_t          ClearAll(Int_t);
    Bool_t          Convert(Int_t,Int_t);
    Bool_t          FindDaq(Int_t,Int_t,Int_t,Int_t,Int_t&,Int_t&,Int_t&,Int_t&);
    Bool_t          BadCoord(Int_t);
    Bool_t          NotImplemented(Int_t);
    Bool_t          NotPossible(Int_t,char*);
    Bool_t          FillSpaces(Int_t,Float_t,Float_t,Int_t,Int_t,Int_t,Int_t,Int_t,Int_t,Int_t,Int_t);
    Bool_t          Help();
        
  public:
                StEmcCalc(const TGWindow *p, UInt_t w, UInt_t h, TROOT*);
    virtual     ~StEmcCalc();
    Bool_t      ProcessMessage(Long_t msg, Long_t parm1, Long_t parm2);
    
    ClassDef(StEmcCalc, 1)
};
