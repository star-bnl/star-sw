#include "StarGenEPEvent.h"
ClassImp(StarGenEPEvent)

StarGenEPEvent::StarGenEPEvent( const Char_t *name, const Char_t *title ) 
: StarGenEvent(name,title),
  idBlue(0),
  idYell(0),
  process(0),
  subprocess(0),
  idParton(0),
  xParton(0.),
  xPdf(0.),
  Q2(0.),
  valence(false),
  y(0.),
  W2(0.),
  nu(0.),
  weight(0.)
{ 
  /* nada */ 
};
