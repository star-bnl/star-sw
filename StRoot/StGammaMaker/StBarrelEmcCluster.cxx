#include <iomanip>

#include "StGammaTower.h"
#include "StBarrelEmcCluster.h"

ClassImp(StBarrelEmcCluster);

ostream& operator<<(ostream& out, const StBarrelEmcCluster& cluster)
{

    out << "Energy [GeV]:\t" << cluster.energy() << '\n';
    out << "Transverse energy [GeV]:\t" << cluster.momentum().Pt() << '\n';
    out << "Eta:\t" << cluster.momentum().Eta() << '\n';
    out << "Phi [radians]:\t" << cluster.momentum().Phi() << '\n';
    out << "Position [cm]: [ x = " << cluster.position().x() << ", y = " 
        << cluster.position().y() << ", z = " << cluster.position().z() << " ]\n";

    out << "Tower id:\n";
    for (int dphi = 1; dphi >= -1; --dphi) 
    {
    
        out << "+------+------+------+\n";
        out << "| ";
        
        for (int deta = -1; deta <= 1; ++deta) 
        {
            StGammaTower* tower = cluster.tower(deta, dphi);
            int id = tower ? tower->id : 0;
            out << setw(4) << id << " | ";
        }
        
        out << '\n';
        
    }
    
    out << "+------+------+------+\n";

    out << "Energy [GeV]:\n";
    for (int dphi = 1; dphi >= -1; --dphi) 
    {
    
        out << "+--------+--------+--------+\n";
        out << "| ";
        
        for (int deta = -1; deta <= 1; ++deta) 
        {
        
            StGammaTower* tower = cluster.tower(deta, dphi);
            float energy  = tower ? tower->energy : 0;
            out << fixed << setprecision(3) << setw(6) << energy << " | ";
            
        }
        
        out << '\n';
    }
    out << "+--------+--------+--------+\n";

  return out;
}
