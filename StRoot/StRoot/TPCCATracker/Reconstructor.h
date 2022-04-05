/*
    Copyright (C) 2009 Matthias Kretz <kretz@kde.org>

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) version 3.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.

*/

#ifndef RECONSTRUCTOR_H
#define RECONSTRUCTOR_H

#include "AliHLTTPCCATracker.h"

#ifdef USE_TBB
#include <tbb/task.h>

class AliHLTTPCCATracker::Reconstructor : public tbb::task
{
  public:
    Reconstructor( AliHLTTPCCATracker *tracker ) : d( tracker ) {}
    tbb::task *execute();

  private:
    AliHLTTPCCATracker *const d;
};
#else //USE_TBB
class AliHLTTPCCATracker::Reconstructor
{
  public:
    Reconstructor( AliHLTTPCCATracker *tracker ) : d( tracker ) {}
    int execute();

  private:
    AliHLTTPCCATracker *const d;
};
#endif //USE_TBB

#endif // RECONSTRUCTOR_H
