#include "loadR6StarLibs.h"
#include "tables/St_g2t_geant4star_Table.h"
#include <iostream>
void PrintConfig(const g2t_geant4star_st* cfg);
void check_g2tTable()
{
    StChain* chain = new StChain();
    StIOMaker* ioMaker = new StIOMaker("IO", "r", "particleGun.geant.root", "bfcTree");

    chain->Init();

    for(int i = 0; i < 99999; i++){
        chain->Clear();
        int status = chain->Make();
        if (status == kStEOF || status == kStFatal) break;
        St_g2t_geant4star* config = dynamic_cast<St_g2t_geant4star*>(chain->GetDataSet("g2t_geant4star"));
        if(config){
            g2t_geant4star_st* cfg = config->GetTable();
            if(cfg){ 
                PrintConfig(cfg);
            } else { 
                std::cout << "no cfg\n" ;
            }
        } else { 
            std::cout << "no config\n";
        }
    }

}

void PrintConfig(const g2t_geant4star_st* cfg)
{
    std::cout << "========== g2t_geant4star ==========\n";

    std::cout << "is_geant4star   = " << cfg->is_geant4star << '\n';
    std::cout << "is_geant3_mode  = " << cfg->is_geant3_mode << '\n';
    std::cout << "is_multi_engine = " << cfg->is_multi_engine << '\n';
    std::cout << "physics_list    = " << cfg->physics_list << '\n';
    std::cout << "engine_mode     = " << cfg->engine_mode << '\n';

    std::cout << "cutgam = " << cfg->cutgam << '\n';
    std::cout << "cutele = " << cfg->cutele << '\n';
    std::cout << "cuthad = " << cfg->cuthad << '\n';
    std::cout << "cutneu = " << cfg->cutneu << '\n';
    std::cout << "cutmuo = " << cfg->cutmuo << '\n';
    std::cout << "bcute  = " << cfg->bcute << '\n';
    std::cout << "bcutm  = " << cfg->bcutm << '\n';
    std::cout << "dcute  = " << cfg->dcute << '\n';
    std::cout << "dcutm  = " << cfg->dcutm << '\n';
    std::cout << "ppcutm = " << cfg->ppcutm << '\n';
    std::cout << "tofmax = " << cfg->tofmax << '\n';

    std::cout << "pair = " << cfg->pair << '\n';
    std::cout << "comp = " << cfg->comp << '\n';
    std::cout << "phot = " << cfg->phot << '\n';
    std::cout << "pfis = " << cfg->pfis << '\n';
    std::cout << "dray = " << cfg->dray << '\n';
    std::cout << "anni = " << cfg->anni << '\n';
    std::cout << "brem = " << cfg->brem << '\n';
    std::cout << "hadr = " << cfg->hadr << '\n';
    std::cout << "munu = " << cfg->munu << '\n';
    std::cout << "dcay = " << cfg->dcay << '\n';
    std::cout << "loss = " << cfg->loss << '\n';
    std::cout << "muls = " << cfg->muls << '\n';
    std::cout << "ckov = " << cfg->ckov << '\n';
    std::cout << "rayl = " << cfg->rayl << '\n';
    std::cout << "labs = " << cfg->labs << '\n';
    std::cout << "sync = " << cfg->sync << '\n';

    std::cout << "field = " << cfg->field << '\n';

    std::cout << "random_seed = " << cfg->random_seed << '\n';

    std::cout << "zmax = " << cfg->zmax << '\n';
    std::cout << "rmax = " << cfg->rmax << '\n';

    std::cout << "vertex_x = " << cfg->vertex_x << '\n';
    std::cout << "vertex_y = " << cfg->vertex_y << '\n';
    std::cout << "vertex_z = " << cfg->vertex_z << '\n';

    std::cout << "vertex_sigmax = " << cfg->vertex_sigmax << '\n';
    std::cout << "vertex_sigmay = " << cfg->vertex_sigmay << '\n';
    std::cout << "vertex_sigmaz = " << cfg->vertex_sigmaz << '\n';

    std::cout << "punchout_stop = " << cfg->punchout_stop << '\n';
    std::cout << "punchout_rmin = " << cfg->punchout_rmin << '\n';
    std::cout << "punchout_zmin = " << cfg->punchout_zmin << '\n';

    std::cout << "scoring_rmax = " << cfg->scoring_rmax << '\n';
    std::cout << "scoring_zmax = " << cfg->scoring_zmax << '\n';
    std::cout << "scoring_emin = " << cfg->scoring_emin << '\n';

    std::cout << "default_engine = " << cfg->default_engine << '\n';
    std::cout << "wcal_engine = " << cfg->wcal_engine << '\n';
    std::cout << "hcal_engine = " << cfg->hcal_engine << '\n';

    std::cout << "embedding_mode = " << cfg->embedding_mode << '\n';

    std::cout << "run_number = " << cfg->run_number << '\n';

    std::cout << "====================================\n";
}