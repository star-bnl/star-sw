#include "wcpplib/stream/prstream.h"
/*
Copyright (c) 2001 I. B. Smirnov

Permission to use, copy, modify, distribute and sell this file
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice, this permission notice,
and notices about any modifications of the original text
appear in all copies and in supporting documentation.
It is provided "as is" without express or implied warranty.
*/

namespace Heed {

indentation indn;

std::ostream& noindent(std::ostream& f) {
  indn.s_not = 1;
  return f;
}
std::ostream& yesindent(std::ostream& f) {
  indn.s_not = 0;
  return f;
}
int s_short_output = 0;

}

#ifndef USE_DEFAULT_STREAMS

long HelperForMcout::count = 0;

#endif
