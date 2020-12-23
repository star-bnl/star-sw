/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
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

#include "AliHLTTPCCARow.h"


#include "BinaryStoreHelper.h"

void AliHLTTPCCARow::StoreToFile( FILE *f, const char *startPointer ) const
{
  BinaryStoreWrite( fGrid, f );
  BinaryStoreWrite( fNHits, f );
  BinaryStoreWrite( fMaxY, f );
  BinaryStoreWrite( fHitNumberOffset, f );

  BinaryStoreWrite( fLinkUpData, startPointer, f );
  BinaryStoreWrite( fLinkDownData, startPointer, f );

  BinaryStoreWrite( fHitPDataY, startPointer, f );
  BinaryStoreWrite( fHitPDataZ, startPointer, f );
  
  BinaryStoreWrite( fClusterDataIndex, startPointer, f );

  BinaryStoreWrite( fHitWeights, startPointer, f );

  BinaryStoreWrite( fFirstHitInBin, startPointer, f );
}

void AliHLTTPCCARow::RestoreFromFile( FILE *f, char *startPtr )
{
  BinaryStoreRead( fGrid, f );
  BinaryStoreRead( fNHits, f );
  BinaryStoreRead( fMaxY, f );
  BinaryStoreRead( fHitNumberOffset, f );

  BinaryStoreRead( fLinkUpData, startPtr, f );
  BinaryStoreRead( fLinkDownData, startPtr, f );

  BinaryStoreRead( fHitPDataY, startPtr, f );
  BinaryStoreRead( fHitPDataZ, startPtr, f );
  
  BinaryStoreRead( fClusterDataIndex, startPtr, f );

  BinaryStoreRead( fHitWeights, startPtr, f );

  BinaryStoreRead( fFirstHitInBin, startPtr, f );
}
