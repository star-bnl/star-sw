/*!\class StPreEclMaker
\author Alexandre A. P. Suaide
 
This is the BEMC cluster finder maker
 
 
*/
#ifndef STAR_StPreEclMaker
#define STAR_StPreEclMaker

#include "StMaker.h"
#include "StEmcVirtualFinder.h"
#include "StEmcOldFinder.h"
#include "StEmcUtil/others/emcInternalDef.h"
#include "StEmcRawMaker/defines.h"

#include "EmcClusterAlgorithm.h"

class StPreEclMaker : public StMaker
{
private:
    StEmcVirtualFinder  *mFinder;
    EmcClusterAlgorithm mAlg;

    StEvent*      getEvent();
    Bool_t        mPrint;

protected:
public:
    StPreEclMaker(const char *name="ecl", const char *title="event/data/emc/hits");
    virtual       ~StPreEclMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    void  setPrint(Bool_t a = kTRUE)
    {
        mPrint = a;
        if(mFinder)
            mFinder->setPrint(a);
    } ///< turns on/off the printout information
    void  setAlgorithm(EmcClusterAlgorithm a)
    {
        mAlg = a;
    } ///< defines the cluster algorithm. Should be set before Init() is called

    EmcClusterAlgorithm algorithm()
    {
        return mAlg;
    } ///< returns the cluster finder algorithm id being used
    StEmcVirtualFinder* finder()
    {
        return mFinder;
    } ///< returns a pointer to the finder. The pointer is available only after Init() is called

    // this method is for background compatibility with the old cluster finder interface
    // it will work only if the algorithm used is kEmcClOld
    void SetClusterConditions(char*,Int_t, Float_t, Float_t, Float_t, Bool_t = kFALSE); ///< this is for background compatibility with the old finder

    ClassDef(StPreEclMaker,1)
};

#endif
