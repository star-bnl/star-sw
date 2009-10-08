/***************************************************************************
 *
 * $Id: St_pp2pp_Cluster.cxx,v 1.2 2009/10/08 21:30:34 yipkin Exp $
 *
 * Author: Kin Yip, Oct. 2009
 ***************************************************************************
 *
 * Description: pp2pp Silicon clusters (after clustering)
 *
 ***************************************************************************
 *
 **************************************************************************/

#include "St_pp2pp_Cluster.h"

static const char rcsid[] = "$Id: St_pp2pp_Cluster.cxx,v 1.2 2009/10/08 21:30:34 yipkin Exp $";

ClassImp(St_pp2pp_Cluster)

St_pp2pp_Cluster::St_pp2pp_Cluster() {

  m_sequencer = 0 ;
  m_chain = 0;
  m_length = 0;
  m_position = -999.;
  m_energy = -999.;
  m_x = 0.;
  m_y = 0.;
  m_z = 0.;
  m_quality = 0;

}

St_pp2pp_Cluster::St_pp2pp_Cluster(UChar_t sequencer, UChar_t chain, UChar_t length, Double_t position, Double_t energy, Double_t x, Double_t y, Double_t z, UChar_t quality) {

    m_sequencer = sequencer;
    m_chain = chain;
    m_length = length;
    m_position = position;
    m_energy = energy;
    m_x = x;
    m_y = y;
    m_z = z;
    m_quality = quality ;

}

St_pp2pp_Cluster::~St_pp2pp_Cluster() { /* noop */ }
    
int St_pp2pp_Cluster::operator==(const St_pp2pp_Cluster& p) const {
    return (p.m_sequencer == m_sequencer &&
	    p.m_chain == m_chain &&
            p.m_length == m_length &&
            p.m_position == m_position &&
            p.m_energy == m_energy &&
            p.m_x == m_x &&
            p.m_y == m_y &&
            p.m_z == m_z &&
	    p.m_quality == m_quality );
}

int St_pp2pp_Cluster::operator!=(const St_pp2pp_Cluster& p) const {
    return !(*this == p);  // use operator==()
}

ostream& operator<<(ostream &os, const St_pp2pp_Cluster& cluster) {
  os << "St_pp2pp_Cluster : Sequencer " << cluster.sequencer() << " , "
     << "Chain " << cluster.chain() << " , "
     << "Length " << cluster.chain() << " , "
     << "Position " << cluster.position() << " , "
     << "Energy " << cluster.energy() << " , "
     << "x = " << cluster.x() << " , y = " << cluster.y() << " , z = " << cluster.z() << " , "
     << "quality : " << cluster.quality() << endl ;

  return os;

}
