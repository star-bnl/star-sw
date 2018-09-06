#include "ComponentUserMapBase.hh"

namespace Garfield
{

ComponentUserMapBase::ComponentUserMapBase(){ 
  m_className = "ComponentUserMapBase"; 
}

ComponentUserMapBase::~ComponentUserMapBase(){}

Medium* ComponentUserMapBase::GetMedium(const double x, const double y, 
                                        const double z){
  double  p1 = x ,  p2 = y ,  p3 = z ;
  double u1x = 1., u2x = 0., u3x = 0.;
  double u1y = 0., u2y = 1., u3y = 0.;
  double u1z = 0., u2z = 0., u3z = 1.;
  ComponentBase* pComponent = NULL;
  MapCoordinates(p1, p2, p3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, 
                 pComponent);
  
  if(!pComponent){
    if(m_debug){
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    ComponentBase pointer is NULL for point (" << x << ", "
                << y << "," << z << ")\n";
    }
    return NULL;
  }
  
  if(m_debug){
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Coordinates: \n";
    std::cerr << "      Global: (" << x << ", " << y << ", " << z << ")\n";
    std::cerr << "      Local:  (" << p1 << ", " << p2 << ", " << p3 << ")\n";
    std::cerr << "    Local base vectors: \n";
    std::cerr << "      u1:  (" << u1x << ", " << u1y << ", " << u1z << ")\n";
    std::cerr << "      u2:  (" << u2x << ", " << u2y << ", " << u2z << ")\n";
    std::cerr << "      u3:  (" << u3x << ", " << u3y << ", " << u3z << ")\n";
  }
  
  return pComponent->GetMedium(p1, p2, p3);
}

void ComponentUserMapBase::ElectricField(const double x, const double y, 
                                         const double z, double& ex, double& ey,
                                         double& ez, Medium*& m, int& status){
  double v;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

void ComponentUserMapBase::ElectricField(const double x, const double y, 
                                         const double z, double& ex, double& ey,
                                         double& ez, double& v, Medium*& m, 
                                         int& status){
  double  p1 = x ,  p2 = y ,  p3 = z ;
  double u1x = 1., u2x = 0., u3x = 0.;
  double u1y = 0., u2y = 1., u3y = 0.;
  double u1z = 0., u2z = 0., u3z = 1.;
  double  e1 = 0.,  e2 = 0.,  e3 = 0.;
  ComponentBase* pComponent = NULL;
  
  MapCoordinates(p1, p2, p3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, 
                 pComponent);
            
  if(!pComponent){
    if(m_debug){
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    ComponentBase pointer is NULL for point (" << x << ", "
                << y << "," << z << ")\n";
    }
    status = -6;
    m = NULL;
    return;
  }
  
  if(m_debug){
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Coordinates: \n";
    std::cerr << "      Global: (" << x << ", " << y << ", " << z << ")\n";
    std::cerr << "      Local:  (" << p1 << ", " << p2 << ", " << p3 << ")\n";
    std::cerr << "    Local base vectors: \n";
    std::cerr << "      u1:  (" << u1x << ", " << u1y << ", " << u1z << ")\n";
    std::cerr << "      u2:  (" << u2x << ", " << u2y << ", " << u2z << ")\n";
    std::cerr << "      u3:  (" << u3x << ", " << u3y << ", " << u3z << ")\n";
  }
  
  pComponent->ElectricField(p1, p2, p3, e1, e2, e3, v, m, status);
  UnmapField(e1, e2, e3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, ex, ey, 
             ez);
}

void ComponentUserMapBase::WeightingField(const double x, const double y, 
                                          const double z, double& wx, 
                                          double& wy, double& wz, 
                                          const std::string& label) {
  double  p1 = x ,  p2 = y ,  p3 = z ;
  double u1x = 1., u2x = 0., u3x = 0.;
  double u1y = 0., u2y = 1., u3y = 0.;
  double u1z = 0., u2z = 0., u3z = 1.;
  double  w1 = 0.,  w2 = 0.,  w3 = 0.;
  ComponentBase* pComponent = NULL;
  std::string label_ = label;
  
  MapCoordinates(p1, p2, p3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, 
                 pComponent, label_);
                 
  if(!pComponent){
    if(m_debug){
      std::cerr << m_className << "::WeightingField:\n";
      std::cerr << "    ComponentBase pointer is NULL for point (" << x << ", "
                << y << "," << z << ")\n";
    }
    wx = wy = wz = 0.;
    return;
  }
  
  if(m_debug){
    std::cerr << m_className << "::WeightingField:\n";
    std::cerr << "    Coordinates: \n";
    std::cerr << "      Global: (" << x << ", " << y << ", " << z << ")\n";
    std::cerr << "      Local:  (" << p1 << ", " << p2 << ", " << p3 << ")\n";
    std::cerr << "    Local base vectors: \n";
    std::cerr << "      u1:  (" << u1x << ", " << u1y << ", " << u1z << ")\n";
    std::cerr << "      u2:  (" << u2x << ", " << u2y << ", " << u2z << ")\n";
    std::cerr << "      u3:  (" << u3x << ", " << u3y << ", " << u3z << ")\n";
  }
  
  pComponent->WeightingField(p1, p2, p3, w1, w2, w3, label_);
  UnmapField(w1, w2, w3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, wx, wy, 
             wz);
}

double ComponentUserMapBase::WeightingPotential(const double x, const double y, 
                                                const double z, 
                                                const std::string& label) {
  double  p1 = x ,  p2 = y ,  p3 = z ;
  double u1x = 1., u2x = 0., u3x = 0.;
  double u1y = 0., u2y = 1., u3y = 0.;
  double u1z = 0., u2z = 0., u3z = 1.;
  ComponentBase* pComponent = NULL;
  std::string label_ = label;
  MapCoordinates(p1, p2, p3, u1x, u2x, u3x, u1y, u2y, u3y, u1z, u2z, u3z, 
                 pComponent, label_);
  
  if(!pComponent){
    if(m_debug){
      std::cerr << m_className << "::WeightingPotential:\n";
      std::cerr << "    ComponentBase pointer is NULL for point (" << x << ", "
                << y << "," << z << ")\n";
    }
    return 0.;
  }
  
  if(m_debug){
    std::cerr << m_className << "::WeightingPotential:\n";
    std::cerr << "    Coordinates: \n";
    std::cerr << "      Global: (" << x << ", " << y << ", " << z << ")\n";
    std::cerr << "      Local:  (" << p1 << ", " << p2 << ", " << p3 << ")\n";
    std::cerr << "    Local base vectors: \n";
    std::cerr << "      u1:  (" << u1x << ", " << u1y << ", " << u1z << ")\n";
    std::cerr << "      u2:  (" << u2x << ", " << u2y << ", " << u2z << ")\n";
    std::cerr << "      u3:  (" << u3x << ", " << u3y << ", " << u3z << ")\n";
  }
  
  return pComponent->WeightingPotential(p1, p2, p3, label_);
}

void ComponentUserMapBase::UpdatePeriodicity(){
    if(m_debug){
      std::cerr << m_className << "::UpdatePeriodicity:\n";
      std::cerr << "    Periodicities should be implemented by overloading the "
                <<     "MapCoordinates function.\n";
    } 
}


}
