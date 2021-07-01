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

#include "AliHLTTPCCAPackHelper.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCADef.h"

// 1e2f is chousen because in this case multiplication of floats is very fast
PackHelper::TPackedY PackHelper::PackY( const AliHLTTPCCARow& row, float y ) {
  UNUSED_PARAM1(row);
  return y*1e2f;
}
PackHelper::TPackedZ PackHelper::PackZ( const AliHLTTPCCARow& row, float z ) {
  UNUSED_PARAM1(row);
  return z*1e2f;
}

float PackHelper::UnpackY( const AliHLTTPCCARow& row, PackHelper::TPackedY y ) {
  UNUSED_PARAM1(row);
  return static_cast<float>(y)*1e-2f;
}
float PackHelper::UnpackZ( const AliHLTTPCCARow& row, PackHelper::TPackedZ z ) {
  UNUSED_PARAM1(row);
  return static_cast<float>(z)*1e-2f;
}

