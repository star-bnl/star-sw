#ifndef STAR_StGeomBrowser
#define STAR_StGeomBrowser
#include "TString.h"
class GeomBrowser;
class StGeomBrowser { 
   private:
      TString fFileName;
      GeomBrowser *fBrowser;
   public:
      StGeomBrowser(const char *filename="complete");
      void SetFile(const char *filename="complete");      
      void Show();      
};

#endif
