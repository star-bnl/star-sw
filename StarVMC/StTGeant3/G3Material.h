#ifndef G3MATERIAL_H
#define G3MATERIAL_H
/* Copyright(c) 1998-1999, ALICE Experiment at CERN, All rights reserved. *
 * See cxx source for full Copyright notice                               */

/* $Id: G3Material.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $ */

#include "TMaterial.h"

class G3Material : public TMaterial 
{
public:
    G3Material(){}
    G3Material(char* name, char* title,
		   Float_t a, Float_t z, Float_t dens, Float_t radl, Float_t intl);
    
    virtual ~G3Material(){}
    // Dump material parameters
    virtual void  Dump();
    // Get material id
    virtual Int_t Id()    {return fId;}
    virtual void  SetId(Int_t id) {fId = id;}
    
private:
    Int_t   fId;          // Id number of the material
    G3Material(const G3Material & mat): TMaterial(mat) {}
    G3Material &operator=(const G3Material &) {return *this;}

    ClassDef(G3Material,1) // G3 Material Class for the G3 GUI 
};

#endif








