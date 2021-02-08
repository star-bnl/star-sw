#include "StarGenPPEvent.h"
ClassImp(StarGenPPEvent);

StarGenPPEvent::StarGenPPEvent( const Char_t *name, const Char_t *title )
  : StarGenEvent(name,title),
    idBlue(0),
    idYell(0),
    process(0),
    subprocess(0),
    idParton1(0),
    idParton2(0),
    xParton1(0.),
    xParton2(0.),
    xPdf1(0.),
    xPdf2(0.),
    Q2fac(0.),
    Q2ren(0.),
    valence1(false),
    valence2(false),
    sHat(0.),
    tHat(0.),
    uHat(0.),
    ptHat(0.),
    thetaHat(0.),
    phiHat(0.),
    weight(0.)    
{ 
  /* nada */ 
};
