/**********************************************************************
 *
 * Author: Duncan Prindle
 *         Interface modeled after THijing by Chunhui Han and functionality
 *         based on therm_events.cxx by
 * Authors of the model: Wojciech Broniowski, Wojciech.Broniowski@ifj.edu.pl, *
 *                       Wojciech Florkowski, Wojciech.Florkowski@ifj.edu.pl  *
 * Authors of the code:  Adam Kisiel, kisiel@if.pw.edu.pl                     *
 *                       Tomasz Taluc, ttaluc@if.pw.edu.pl                    *
 * Code designers: Adam Kisiel, Tomasz Taluc, Wojciech Broniowski,            *
 *                 Wojciech Florkowski                                        *
 *
 *
 **********************************************************************/
#ifndef __THERMINATOR__H
#define __THERMINATOR__H

#include "TMath.h"
#include "Event.h"


class Therminator : public TObject {
 public:
  void init();
  int mNevent;
  int mITrack;
  Integrator *calka;
  Event *mEvent;
  Particle *mCurrParticle;

  Therminator( const char *baseDir );
  virtual ~Therminator() {};
// Could modify Event::Randomize() to accept optional seed.
//  void SetRandomSeed(int iseed);
  void ReadParameters();
  void GenerateEvent();
  int EventsGenerated() const { return mNevent; }
  void writeOut();
  int GetNParticles();
  int GetPdg(int i);
  float GetPx(int i);
  float GetPy(int i);
  float GetPz(int i);
  float GetVx(int i);
  float GetVy(int i);
  float GetVz(int i);
  float GetEnergy(int i);
  float GetMass(int i);
  float GetRapidity(int i);
  float GetPt(int i);

  ClassDef(Therminator,1)
};
#endif
