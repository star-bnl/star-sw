# RHICf+STAR Simulation
RHICf+STAR Simulation developed from the RHICf simulation <https://github.com/hmenjo/RHICf-Simulation> (v10)

---

* RHICf+STAR Simulation propagates the STAR simulation data into RHICf and STAR ZDC simulation

* This is designed to be compatible with StRHICfSimDst format data and STAR's scheduler.

* The input file could need StRHICfSimDst format (See the **Producing the StRHICfSimDst file** part)

* HepMC format input to be updated

## Preparing installation

1. **ROOT** (Required)
2. **GEANT4** with GDML, version recommended below 10.6 (Required)
3. **STAR Library**, version require above SL23d (Optional, see the <https://github.com/star-bnl/star-sw>)

## Installation
### Before the installation

You should make sure the geant4 and root library

* Source to geant4 `source /path/to/geant4/install/geant4.csh`

* Check the ROOT library path `${ROOTSYS}`

* Note: If you are not working on STAR server system, you should set the ROOT path with **CMake**
    
  `cmake -DROOTPATH=/path/to/ROOT/install`

### Compile

```
git clone https://github.com/ggfdsa10/RHICfSTARSim.git

cd RHICfSTARSim

mkdir build

cd build

cmake ../

make 

```

## Input options
Input options can be inserted as two types, argument, and text file.

### Argument type

* **-m** (Input mode)

    * **STARSim** is default option 

    * Available mode

        * **STARSim** : STAR simulation propagating mode (needed StRHICfSimDst input)

        * **HepMC** : HepMC format event generator file mode (To be updated)

* **-i** (Input file)

    * Path the input file (Only one file acceptable)

* **-r** (RHICf run type)

    * If **STARSim** mode, run-type set using the input file automatically 

    * Available mode

        * **TS** 
        * **TL**
        * **TOP**

* **-g** (geometry directory)

    * Path the simulation detector geometry (GDML format) directory path

    * If not setting, the simulation can find the geometry directory automatically

* **-t** (table directory)

    * Path the simulation correction tables directory path

    * If not setting, the simulation can find tables directory automatically

* **-xxx** (user customized option)

    * Format : -OptionName OptionValue 

    * Available type : **int, double, bool, string**
      
      (see the <https://github.com/ggfdsa10/RHICfSTARSim/blob/main/source/Util/RHICfSimOptions.hh>)

### Text file type

* Input text file extension : **.txt, .dat, .par**

* Format
   * OptionName = OptionValue
   * Comment use to "#"
    
* Necessary options can see the **Argument type** part

## Runs
Now you can run the RHICf+STAR simulation.

* Example
  
```
cd build 

./mainRHICfSTARSim -i /path/to/StRHICfSimDSt.root # default STARSim mode with argument option

./mainRHICfSTARSim ./input.dat # run to specific options with text file option

```

## Producing the StRHICfSimDst file (optional)
StRHICfSimDst format file can be produced using only StRHICfSimConvertor with STAR Library

See the StRHICfPool: https://github.com/ggfdsa10/RHICfSTARLibrary
