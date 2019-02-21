/***************************************************************************
 *
 * $Id: StMuETofCollection.h,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: Data collection for storing eTOF information (header, digis,
 * hits) in the muDsts
 *
 ***************************************************************************
 *
 * $Log: StMuETofCollection.h,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/ 
#ifndef STMUETOFCOLLECTION_H
#define STMUETOFCOLLECTION_H

#include <vector>

#include "TObject.h"

class StETofCollection;
class StMuETofHeader;
class StMuETofDigi;
class StMuETofHit;


class StMuETofCollection : public TObject {

public:

    StMuETofCollection();
    StMuETofCollection( const StETofCollection* );
    
    ~StMuETofCollection();

    const StMuETofHeader*  etofHeader() const;
    StMuETofHeader*        etofHeader();

    StMuETofDigi* etofDigi( int i );
    StMuETofHit*  etofHit( int i );
    
    int digisPresent();
    int hitsPresent();
    
protected:

    std::vector< StMuETofHeader > mETofHeader;
    std::vector< StMuETofDigi   > mETofDigis;    
    std::vector< StMuETofHit    > mETofHits;  
    

    ClassDef( StMuETofCollection, 1 )
};

#endif // STMUETOFCOLLECTION_H