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
class TString;
class TTree; 

class THack 
{
public:
static void DeleteClonesArray(TClonesArray *clone);
static void ClearClonesArray(TClonesArray *clone);
static void PadRefresh(TPad *pad,int flag=0);
static void HistRelease(TDirectory *dir);
static int  LineToD(const char *line, const char **lend,
		    int nItems, double *Items, TString *Names=0);
static bool IsTreeWritable(const TTree *tree, bool fatal=true);
};
#endif //ROOT_THack
