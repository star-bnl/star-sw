#ifndef ROOT_THack
#define ROOT_THack


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// THack                                                             //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class TClonesArray;
class TPad;
class TDirectory;
class THack 
{
public:
static void DeleteClonesArray(TClonesArray *clone);
static void ClearClonesArray(TClonesArray *clone);
static void PadRefresh(TPad *pad,int flag);
static void HistRelease(TDirectory *dir);
};
#endif //ROOT_THack
