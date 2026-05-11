# DPMJET-III and PHOJET

### Version: 19.1.1
### Status: development
### [Official releases](https://github.com/afedynitch/dpmjet/releases)

## Description:

DPMJET-III is a Monte Carlo event generator for hadron, photon and nuclear collisions with a several decade-long history. DPMJET is an integral part of [FLUKA](https://www.fluka.org), a fully integrated particle physics
MonteCarlo simulation package, acting as event generator for nucleus-nucleus collisions above 5 GeV/nucleon and for hadronic collisions at high energies.
DPMJET-III embeds the PHOJET event generator, which is used for simulations of
hadron-hadron, photon-hadron and photon-photon collisions. This and future versions of PHOJET will be distributed as part of this DPMJET package. Both codes are interfaced to [Pythia 6](https://pythiasix.hepforge.org) for hadronization.

## Availability:

Visit the [GitHub repository](https://github.com/afedynitch/dpmjet) for the latest version. Official releases are [available for download from here](https://github.com/afedynitch/dpmjet/releases). 

Please use [Issues](https://github.com/afedynitch/dpmjet/issues) for reporting bugs, questions or complaints.

The code is distributed with a copy of [PYTHIA 6.4.27](https://pythiasix.hepforge.org) and relies on its fragmentation routines. Torbjorn Sjostrand is the copyright holder of PYTHIA 6.


## User interfaces:

### Traditional steering cards:

This and newer versions will continue supporting configuration and running though ASCII files. Instructions and options are listed in [input card interface](docs/dpmjet_steering_cards.md).

### Python interface (experimental):

The Python interface is based on [NumPy's](https://docs.scipy.org/doc/numpy/index.html) [f2py](https://docs.scipy.org/doc/numpy/f2py/index.html) package, which exposes the subroutines of a Fortran library without modifications to the original source code. The package [impy](https://gitlab.com/afedynitch/impy) (currently non-public, but almost completed.) is the recommended user interface. Don't use the python interface by yourself.

## Building/Installation

To use versioned releases, download the latest tarball from [releases](https://github.com/afedynitch/dpmjet/releases) and unpack.
To build the executables:

```bash
make -j<n_jobs> exe
# Wait...

# Run example
bin/DPMJET < examples/dpmjet/ppLHC.inp
```
The Python library is available as a different build target
```bash
make -j<n_jobs> pylib
```
There is currently no example and using this library without `impy` is not recommended.

## Documentation

We are in the process of compiling more documentation and/or examples. For now, browse the docs folder for basic instructions and a marginally outdated manual for PHOJET.

## Authors:
          [Anatoli Fedynitch]*
          ICRR - Institute for Cosmic Ray Research                                 
          The University of Tokyo                            
          Kashiwanoha 5-1-5, Kashiwa, Chiba, Japan
          
          Stefan Roesler
          CERN, DGS-RP
          CH-1211 Geneva 23, Switzerland

          Ralph Engel
          Institut fuer Kernphysik
          Karlsruhe Institute of Technology
          D-76021 Karlsruhe, Germany

*[maintainer](mailto:af.gh179@outlook.com)

This code is part of the heritage of

          Johannes Ranft
          Dept. of Physics
          University of Siegen
          D-57068 Siegen, Germany

who inspired this one and the many predecessor codes.

# Literature and references

Both, DPMJET and PHOJET are not accompanied by full publications that describe the code. As long as a new publication is in preparation, please cite:

- [R. Engel, *Photoproduction within the two component dual parton model. 1. Amplitudes and cross-sections*, Z.Phys. C66 (1995) 203-214](http://inspirehep.net/record/373339), when simulating photon-photon, photon-hadron and hadron-hadron collisions.

- [S. Roesler, R. Engel and J. Ranft, *The Monte Carlo event generator DPMJET-III*, Proc. 'Monte Carlo 2000', Lisbon, Portugal, (2000)](http://inspirehep.net/record/538940), when simulating nuclear interactions.


- [A. Fedynitch, *Cascade equations and hadronic interactions at very high energies*, PhD thesis, 2015, CERN-THESIS-2015-371](http://inspirehep.net/record/1503512), when simulating at or above LHC energies, or when runnign DPMJET in cascade simulations.

A list of references detailing the physics is under construction here.

# Contributors

The code received substantial contributions by

* Alfredo Ferrari (CERN)
* Fritz W. Bopp (U. Siegen)

# [LICENSE](LICENSE)

The code is licensed under the BSD 3-clause license, see [LICENSE](LICENSE) for details. This license gives you maximal freedom to employ the code for academic purposes. We are convinced that forking or extracting parts of the code, breaks its integrity and diminishes the scientific value of our work for the high-energy physics community as a whole. Therefore, we strongly encourage you to respect the MCnet academic usage guidelines, that are included as [GUIDELINES](GUIDELINES). We welcome modifications or extensions that are compatible with this official distribution and will seriously consider every pull request that does not break compatibility.