#ifndef StHbtHisto
#define StHbtHisto

#ifdef __ROOT__

#ifndef ROOT_TH1
#include "TH1.h"
#include "TF1.h"
#endif

#ifndef ROOT_TH2
#include "TH2.h"
#include "TF2.h"
#endif

//#ifndef ROOT_TH3
#include "TH3.h"
#include "TF3.h"
//#endif

#include "StHbtMaker/Infrastructure/CTH.hh"

// typedef CTH1D StHbt1DHisto;
// typedef CTH2D StHbt2DHisto;
// typedef CTH3D StHbt3DHisto;

typedef TH1D StHbt1DHisto;
typedef TH2D StHbt2DHisto;
typedef TH3D StHbt3DHisto;
typedef TF1 StHbtTF1;
typedef TF2 StHbtTF2;
typedef TF3 StHbtTF3;

#else // __ROOT__

#inclue "StHbtMaker/Infrastructure/franks2HistoD.h"
typedef franks1HistoD StHbt1DHisto;
typedef franks2HistoD StHbt2DHisto;

#endif // __ROOT__

#endif

