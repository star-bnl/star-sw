/***************************************************************************
 *
 * $Id: StEmcRawData.h,v 2.2 2004/07/15 16:36:24 ullrich Exp $
 *
 * Author: Alex Suaide, Mar 2004
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcRawData.h,v $
 * Revision 2.2  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.1  2004/03/26 21:53:45  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StEmcRawData_hh
#define StEmcRawData_hh

#include "StObject.h"
#include "TArrayS.h"

class StEmcRawData : public StObject {
public:
    enum {MAXEMCDATABANK=60};

    StEmcRawData();
    StEmcRawData(const StEmcRawData&);
    ~StEmcRawData();
        
          unsigned short* header(int); 
    const unsigned short* header(int) const;

          unsigned short* data(int);
    const unsigned short* data(int) const; 
    
          unsigned short  header(int,int);
    const unsigned short  header(int,int) const;
    
          unsigned short  data(int,int);
    const unsigned short  data(int,int) const;
    
          int     sizeHeader(int);
    const int     sizeHeader(int) const;
    
          int     sizeData(int);
    const int     sizeData(int) const;
    
          int     getNBlocks() { return MAXEMCDATABANK;}
    
    void          createBank(int, int, int);
    void          deleteBank(int);
    
    void          setHeader(int, unsigned short*);
    void          setData(int, unsigned short*);
    void          setHeader(int, int, unsigned short);
    void          setData(int, int, unsigned short);

protected:
    TArrayS       mHeader[MAXEMCDATABANK];
    TArrayS       mData[MAXEMCDATABANK];
    
    ClassDef(StEmcRawData,1)
};
#endif
