/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2009 Matthias Kretz <kretz@kde.org>
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "AliHLTTPCCATrackletVector.h"

AliHLTTPCCATrackletVector::AliHLTTPCCATrackletVector()
    : fNHits( Vc::Zero ), fFirstRow( Vc::Zero ), fLastRow( Vc::Zero )
{
  const uint_v x = std::numeric_limits<uint_v>::max();
  for ( unsigned int i = 0; i < fRowHits.size(); ++i ) {
    reinterpret_cast<uint_v&>(fRowHits[i]) = x;
  }
}
