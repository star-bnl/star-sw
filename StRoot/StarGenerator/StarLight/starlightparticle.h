///////////////////////////////////////////////////////////////////////////
//
//    Copyright 2010
//
//    This file is part of starlight.
//
//    starlight is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    starlight is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with starlight. If not, see <http://www.gnu.org/licenses/>.
//
///////////////////////////////////////////////////////////////////////////
//
// File and Version Information:
// $Rev::                             $: revision of last commit
// $Author: jwebb $: author of last commit
// $Date: 2012/11/27 22:27:33 $: date of last commit
//
// Description:
//
//
//
///////////////////////////////////////////////////////////////////////////


#ifndef STARLIGHTPARTICLE_H
#define STARLIGHTPARTICLE_H


#include "lorentzvector.h"


class starlightParticle : public lorentzVector
{
   public:
      
      starlightParticle();
      starlightParticle ( double px, double py, double pz, double e, double mass, int pdgCode, short charge);
      virtual ~starlightParticle();
   
      void setPdgCode(int pdgCode) { _pdgCode = pdgCode; }
      int getPdgCode() const { return _pdgCode; }
      
      void setCharge(short charge) { _charge = charge; }
      short getCharge() const { return _charge; }
      
   private:
    
    int _pdgCode;
    short _charge;
    double _mass;
};


#endif  // STARLIGHTPARTICLE_H
