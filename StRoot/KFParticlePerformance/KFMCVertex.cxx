/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "KFMCVertex.h"

KFMCVertex::KFMCVertex():fDaughterTracks(0),fIsReconstructable(0),fIsMCReconstructable(0),fIsReconstructed(0),fNReconstructedDaughters(0),fIsTriggerPV(0)
{
  for( int i = 0; i < 3; i++) fPar[i] = 0;
}

std::ostream& operator<<(std::ostream& out, const KFMCVertex &a)
{
  /** Operator to print coordinates of the MC vertex "a".
   ** \param[in] out - stream, where coordinates will be printed
   ** \param[in] a - vertex to be printed
   **/
  for (int i = 0; i < 3; i++) out << a.fPar[i] << std::endl;
  return out;
}

std::istream& operator>>(std::istream& in, KFMCVertex &a)
{
  /** Operator to read coordinates of the vertex from the input stream. 
   ** \param[in] in - input stream
   ** \param[in] a - vertex, where the coordinates will be read in
   **/
  for (int i = 0; i < 3; i++) in >> a.fPar[i];
  return in;
}

