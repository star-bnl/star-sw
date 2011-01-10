// Abstract base class for plotting engines

#ifndef G_PLOTTING_ENGINE_H
#define G_PLOTTING_ENGINE_H

#include "Medium.hh"
#include "Sensor.hh"

namespace Garfield {

class PlottingEngine {

  public:
    // Constructor
    PlottingEngine() :
      className("PlottingEngine"), 
      colorLine1("dark-blue"), colorLine2("olive"), 
      colorElectron("orange"), colorHole("red"), colorIon("dark-red"),
      colorPhoton("blue"), colorChargedParticle("dark-green"),
      debug(false) {}

    // Destructor
    virtual ~PlottingEngine() {}
    
    // Set/get colors.
    void SetLineColor1(const std::string col)    {colorLine1 = col;}
    void SetLineColor2(const std::string col)    {colorLine2 = col;}
    void SetElectronColor(const std::string col) {colorElectron = col;}
    void SetHoleColor(const std::string col)     {colorHole = col;}
    void SetIonColor(const std::string col)      {colorIon = col;}
    void SetPhotonColor(const std::string col)   {colorPhoton = col;}
    void SetChargedParticleColor(const std::string col) {
                                                colorChargedParticle = col;}

    std::string GetLineColor1()    {return colorLine1;}
    std::string GetLineColor2()    {return colorLine2;}
    std::string GetElectronColor() {return colorElectron;}
    std::string GetHoleColor()     {return colorHole;}
    std::string GetIonColor()      {return colorIon;}
    std::string GetPhotonColor()   {return colorPhoton;}
    std::string GetChargedParticleColor() {return colorChargedParticle;}

    // Switch on/off debugging messages
    void EnableDebugging()  {debug = true;}
    void DisableDebugging() {debug = false;}

  protected:

    std::string className;

    std::string colorLine1, colorLine2;
    std::string colorElectron;
    std::string colorHole;
    std::string colorIon;
    std::string colorPhoton;
    std::string colorChargedParticle;

    bool debug;

};

}

#endif
