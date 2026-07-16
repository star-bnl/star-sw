# CRMC+RHICf event generator interface

CRMC+RHICf interface developed from the CRMCv2.2.1

Please find the detailed information about CRMC on <https://gitlab.iap.kit.edu/AirShowerPhysics/crmc> 

## Requirements
- [ROOT](https://root.cern/) with TParticle library
- [HepMC3](https://gitlab.cern.ch/hepmc/HepMC3)
- [BOOST](https://www.boost.org/)
- [fastjet](http://fastjet.fr/) (Optional)

## Installation

    mkdir build
    cd build
    cmake ../ -DCMAKE_INSTALL_PREFIX=/path/to/install -DHepMC3_DIR=/path/to/HepMC3/cmake/dir -Dfastjet_PATH=/path/to/fastjet/if-you-needed
    make -j <cpu num>
    make install

## Avaliable event generators
CRMC+RHICf interface can generate a few models

1. EPOS-LHC-R  (`-m 0`)
2. EPOS-LHC-R (Fast) (`-m 1`)
3. SIBYLL 2.3e (`-m 6`)
4. QGSJETII-04 (`-m 7`)
5. QGSJETIII-01 (`-m 13`)

## How to excute

    cd /path/to/install/bin
    ./crmcrhicf -m <model idx> -r <RHICf run type> -n <event number> -j <job index>

* Options:
  * -m : model index, please find it "Avaliable event generators"
  * -r : RHICf Run type (Default: ALL) [TL, TS, TOP, ALL]
    * If you set to "ALL", model generate without any selection
  * n : the total number of selected event
  * j : specific job number
