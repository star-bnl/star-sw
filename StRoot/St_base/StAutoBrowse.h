#ifndef _STAUTOBROWSE_
#define _STAUTOBROWSE_ 2000
class TObject;

class StAutoBrowse  
{
 StAutoBrowse(){};
~StAutoBrowse(){};
public:
friend class NOBODY;
static void  Browse(TObject *obj, TBrowser *browser);
};
#endif //  _STAUTOBROWSE_
