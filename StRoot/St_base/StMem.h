/***************************************************************************
 *
 * $Id: StMem.h,v 1.2 2001/03/05 00:50:56 perev Exp $
 *
 * Author: Victor Perev, Jul 2000
 ***************************************************************************
 *
 * Description:
 * Simplified version of StMemoryInfo class of Thomas Ullrich
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StMem_h
#define StMem_h

class StMem {
friend class nobody;
public:
    static  double Used();
    static  double ESize();
    static void   Print(const char *tit="");
private:
    StMem(){};
   ~StMem(){};

static double fUsed;
/*
ClassDef(StMem,0)
*/
};

#endif
