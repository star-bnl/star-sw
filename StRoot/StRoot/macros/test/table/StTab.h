#ifndef StTab_h
#define StTab_h

struct Tab_st
{
  float pt;
  int gid;
};

#include <TTable.h>
class  StTab : public TTable
{
public:
  ClassDefTable(StTab, Tab_st)
  ClassDef(StTab, 1)  // Particles or cells table for jet finder   
};                                                                    

#endif                                                                
