/*!
  \author G. Van Buren, BNL

  Determination of which out/err streams to use

*/

#ifndef StMessageStream
#define StMessageStream

#include <Stiostream.h>

static ostream& myout(cout);
static ostream& myerr(cerr);

#endif

// $Id: StMessageStream.h,v 1.1 2003/09/25 21:19:22 genevb Exp $
// $Log: StMessageStream.h,v $
// Revision 1.1  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
//
