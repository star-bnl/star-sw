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
class StMaker;
/**
   \class StBTofTables
   Class to retrieve the tof tray/channel status from data base
 */
class StBTofTables : public TObject
{
protected:
#if 0
  static const Int_t mNTray = 120;
  static const Int_t mNModule = 32;
  static const Int_t mNCell = 6;
  static const Int_t mNChanMax = 24000;  /// A large number for total channels
    
  /// Tray/Channel Status    
  UShort_t     mBTofTrayConfig[mNTray];    
  UShort_t     mBTofStatus[mNTray][mNModule][mNCell];
#endif
 public:
    /// Default constructor
   StBTofTables() {Reset();}
   virtual ~StBTofTables() {Reset();}
   void    Reset();
    
   /// load status tables from data base
   void    loadTables(StMaker */* mk */) {loadTables();}
   void    loadTables();
#if 0    
    /// function to return the tray status
    bool  trayValid(int trayId) const;
#endif
    /// function to return the channel status
    static Int_t   status(Int_t trayId, Int_t moduleId, Int_t cellId);
    
    ClassDef(StBTofTables, 1);
        
};

#endif
