/*******************************************************************
 *
 * $Id: StBTofTables.h,v 1.1 2009/08/28 17:22:02 dongx Exp $
 *
 *****************************************************************
 *
 * $Log: StBTofTables.h,v $
 * Revision 1.1  2009/08/28 17:22:02  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STBTOFTABLES_H
#define STBTOFTABLES_H

#include "TObject.h"
#ifndef __TFG__VERSION__

#include "tables/St_tofTrayConfig_Table.h"
#include "tables/St_tofStatus_Table.h"

#endif /* ! __TFG__VERSION__ */
class StMaker;

/**
   \class StBTofTables
   Class to retrieve the tof tray/channel status from data base
 */
class StBTofTables : public TObject
{
protected:
#ifndef __TFG__VERSION__
    static const Int_t mNTray = 120;
    static const Int_t mNModule = 32;
    static const Int_t mNCell = 6;
    static const Int_t mNChanMax = 24000;  /// A large number for total channels

    /// Tray/Channel Status    
    UShort_t     mBTofTrayConfig[mNTray];    
    UShort_t     mBTofStatus[mNTray][mNModule][mNCell];

public:
#else /* __TFG__VERSION__ */
 public:
#endif /* __TFG__VERSION__ */
    /// Default constructor
#ifndef __TFG__VERSION__
    StBTofTables();
    virtual ~StBTofTables();
    void    Reset();
#else /* __TFG__VERSION__ */
   StBTofTables() {Reset();}
   virtual ~StBTofTables() {Reset();}
   void    Reset() {}
#endif /* __TFG__VERSION__ */
    
#ifndef __TFG__VERSION__
    /// load status tables from data base
    void    loadTables(StMaker* anyMaker);
    
    /// function to return the tray status
    bool  trayValid(int trayId) const;
#else /* __TFG__VERSION__ */
   /// load status tables from data base
   void    loadTables(StMaker */* mk */) {loadTables();}
   void    loadTables() {}
#endif /* __TFG__VERSION__ */
    /// function to return the channel status
#ifndef __TFG__VERSION__
    int   status(int trayId, int moduleId, int cellId) const;
#else /* __TFG__VERSION__ */
    static Int_t   status(Int_t trayId, Int_t moduleId, Int_t cellId);
#endif /* __TFG__VERSION__ */
    
    ClassDef(StBTofTables, 1);
        
};

#endif
