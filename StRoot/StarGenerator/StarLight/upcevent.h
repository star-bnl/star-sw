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


#ifndef UPCEVENT_H
#define UPCEVENT_H


#include <vector>

#include "starlightconstants.h"
#include "starlightparticle.h"


class upcEvent 
{
   public:

      upcEvent();
      upcEvent(starlightConstants::event &ev);
      ~upcEvent();

//      upcEvent & operator  = (const starlightConstants::event &);

      void addParticle(starlightParticle &part) { _particles.push_back(part); }
      void addVertex(vector3 &vertex) { _vertices.push_back(vertex); }
      
      const std::vector<starlightParticle> * getParticles() const { return &_particles; }
      const std::vector<vector3> * getVertices() const { return &_vertices; }
      
   private:
      
      int _NTracks;
      std::vector<starlightParticle> _particles;
      std::vector<vector3> _vertices;
};


#endif  // UPCEVENT_H
