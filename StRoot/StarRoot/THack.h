#ifndef ROOT_THack
#define ROOT_THack


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// THack                                                             //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class TClonesArray;
class THack 
{
public:
static void DeleteClonesArray(TClonesArray *clone);
static void ClearClonesArray(TClonesArray *clone);
};
#endif //ROOT_THack
