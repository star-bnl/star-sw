/*!
 * \class St_pp2pp_Cluster 
 * \author Kin Yip, Oct. 2009
 */
/***************************************************************************
 *
 * $Id: St_pp2pp_Cluster.h,v 1.3 2009/10/08 18:38:17 yipkin Exp $
 *
 * Author: Kin Yip, Oct. 2009
 ***************************************************************************
 *
 * Description: pp2pp Silicon clusters (after clustering)
 *
 ***************************************************************************
 *
 **************************************************************************/
#ifndef St_pp2pp_Cluster_hh
#define St_pp2pp_Cluster_hh

#include <Stiostream.h>
#include "StObject.h"

class St_pp2pp_Cluster : public StObject {

 public:
    St_pp2pp_Cluster();

    St_pp2pp_Cluster(UChar_t sequencer, UChar_t chain, UChar_t length, Double_t position, Double_t energy, Double_t x, Double_t y, Double_t z);

    ~St_pp2pp_Cluster();    

    int operator==(const St_pp2pp_Cluster&) const;
    int operator!=(const St_pp2pp_Cluster&) const;

    UChar_t   sequencer() const;
    UChar_t   chain() const;
    UChar_t   length() const;
    Double_t   position() const;
    Double_t   energy() const;
    Double_t   x() const;
    Double_t   y() const;
    Double_t   z() const;


    void      setSequencer(UChar_t);
    void      setChain(UChar_t);
    void      setLength(UChar_t);
    void      setPosition(Double_t);
    void      setEnergy(Double_t);
    void      setX(Double_t);
    void      setY(Double_t);
    void      setZ(Double_t);
    
 protected:

    UChar_t  m_sequencer;     // sequencer id: ( 1 - 8 ) .. Roman Pot
    UChar_t  m_chain;         // 0 - 3 (A: 0, B: 1, C: 2, D: 3) .. Si plane in Roman Pot
    UChar_t  m_length ;       // > 0 
    Double_t m_position ;     // 0 - 755
    Double_t m_energy ;       // in ADC
    Double_t m_x ;
    Double_t m_y ;
    Double_t m_z ;

    ClassDef(St_pp2pp_Cluster,1)
};

ostream& operator<<(ostream&, const St_pp2pp_Cluster&); // Printing operator

inline void St_pp2pp_Cluster::setSequencer(UChar_t sequencer_no) { m_sequencer = sequencer_no; }
inline void St_pp2pp_Cluster::setChain(UChar_t chain_no) { m_chain = chain_no; }
inline void St_pp2pp_Cluster::setLength(UChar_t length) { m_length = length; }
inline void St_pp2pp_Cluster::setPosition(Double_t position) { m_position = position; }
inline void St_pp2pp_Cluster::setEnergy(Double_t energy) { m_energy = energy; }
inline void St_pp2pp_Cluster::setX(Double_t x) { m_x = x; }
inline void St_pp2pp_Cluster::setY(Double_t y) { m_y = y; }
inline void St_pp2pp_Cluster::setZ(Double_t z) { m_z = z; }


inline UChar_t St_pp2pp_Cluster::sequencer() const { return m_sequencer; }
inline UChar_t St_pp2pp_Cluster::chain() const { return m_chain; }
inline UChar_t St_pp2pp_Cluster::length() const { return m_length; }
inline Double_t St_pp2pp_Cluster::position() const { return m_position; }
inline Double_t St_pp2pp_Cluster::energy() const { return m_energy; }
inline Double_t St_pp2pp_Cluster::x() const { return m_x; }
inline Double_t St_pp2pp_Cluster::y() const { return m_y; }
inline Double_t St_pp2pp_Cluster::z() const { return m_z; }

#endif
