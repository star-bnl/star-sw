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

#include "KFMCParticle.h"

#ifndef KFParticleStandalone
ClassImp(KFMCParticle)
#endif

KFMCParticle::KFMCParticle() :fDaughterIds(), fMCTrackID(-1), fMotherId(-1), fPDG(0), fInitialParticleId(-1)
{
  for(int i=0; i<3; i++)
  {
    fIsReconstructable[i] = 0;
    fIsV0[i] = 0;
  }
  fIsReconstructable[3] = 0;
  fIsReconstructable[4] = 0;
}

KFMCParticle::~KFMCParticle()
{
}

void KFMCParticle::AddDaughter( int i )
{
  fDaughterIds.push_back(i);
}
