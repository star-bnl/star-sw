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

#ifndef ALIHLTTPCCAPACKHELPER_H
#define ALIHLTTPCCAPACKHELPER_H

#include "AliHLTTPCCADef.h"
class AliHLTTPCCARow;

 // version w\o this transformation at revision 21937
class PackHelper
{
 public:
  typedef short int TPackedY;
  typedef short int TPackedZ;

  static TPackedY PackY( const AliHLTTPCCARow& row, float y );
  static TPackedZ PackZ( const AliHLTTPCCARow& row, float z );
  static float UnpackY( const AliHLTTPCCARow& row, TPackedY y );
  static float UnpackZ( const AliHLTTPCCARow& row, TPackedZ z );
};

#endif
