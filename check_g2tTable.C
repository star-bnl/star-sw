#include "loadR6StarLibs.h"
#include "tables/St_g2t_geant4star_Table.h"
#include <iostream>
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
                std::cout << "cfg->field = " << cfg->field << '\n'; 
            }else { 
                std::cout << "no cfg\n" ;
            }
        }else { 
            std::cout << "no config\n";
        }
    }

}
