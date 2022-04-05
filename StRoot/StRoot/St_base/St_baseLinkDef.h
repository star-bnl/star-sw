#ifdef __CINT__
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#pragma link C++ enum  EReturnCodes;

#pragma link C++ class table_head_st-;

#pragma link C++ class St_Module-;

#pragma link C++ class StTree;
#pragma link C++ class StBranch;
#pragma link C++ class StFileI;
#pragma link C++ class StUKey;
#pragma link C++ class StFile;
#pragma link C++ class StIOEvent-;
#pragma link C++ class StIO-;
#pragma link C++ class StObject-;
#pragma link C++ class StUUId-;
#pragma link C++ class StXRef-;
#pragma link C++ class StXRefMain-;
#pragma link C++ class StObjArray-;
#pragma link C++ class StRefArray-;
#pragma link C++ class StStrArray-;
#pragma link C++ class StObjLink-;
#pragma link C++ class StMem-;
#pragma link C++ class TPageMap-;

 //  STAR logger
#pragma link C++ class StMessMgr;
#pragma link C++ global gMessMgr;
#pragma link C++ global gMessage;
// #pragma link C++ global gMess;
#pragma link C++ global endm;
#pragma link C++ function operator<<(ostream& ,StMessage*);
#pragma link C++ function operator++(StMessMgr&);
#pragma link C++ function operator-(StMessMgr&);
#pragma link C++ function operator--(StMessMgr&);
#pragma link C++ function operator~(StMessMgr&);
 

#endif
