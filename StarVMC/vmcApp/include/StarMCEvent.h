// $Id: StarMCEvent.h,v 1.1 2004/09/03 00:10:57 potekhin Exp $

#ifndef STARMCEVENT_H
#define STARMCEVENT_H

#include <TObject.h>
#include <TRef.h>
#include <TRefArray.h>
#include <TParticle.h>

class StarMCEvent : public TObject
{
  public:
    StarMCEvent();
    virtual ~StarMCEvent();     

  private:

    ClassDef(StarMCEvent,1) // Extended TParticle
};

#endif //STARMCEVENT_H   
   

