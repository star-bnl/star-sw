#!/bin/sh

downloadInstallConda () {
    SCRIPT=Miniconda3.sh
    wget 'https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh' -O ${SCRIPT}

    if [[ "$?" != 0 ]]; then
        echo "Failed download miniconda from the Internet. Please install miniconda manually. Abort."
        exit
    fi
    echo "Installing miniconda."
    bash ${SCRIPT} -b -p $( pwd )/miniconda
    echo "Installation complete. Starting up."
    eval "$("$( pwd )"/miniconda/bin/conda shell.bash hook)"
    conda init
    echo "Initialized. Cleaning up."
    rm -f ${SCRIPT}
}

find_in_conda_env(){
    conda env list | grep "${@}" >/dev/null 2>/dev/null
}

# activate bash shell and see if conda exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

# check if conda is installed. Suppress warning
INSTALLED=$(which conda 2>/dev/null)

if [ -z "$INSTALLED" ]
then
    while true; do
        echo "No conda installation is found. Should the installer download miniconda and begin installation process? CAUTION, the installation takes up about 3 GB of space. (y/n)"
        read yn
        case $yn in 
            [Yy]* ) downloadInstallConda; break;;
            [Nn]* ) echo Abort; exit;;
            * ) echo "Please answer y or n.";;
        esac
    done
else
    echo "Conda is installed. Will proceed to installing environment for segmentation."
fi

# Check again to see if installation is successful
INSTALLED=$(which conda 2>/dev/null)

if [ -z "$INSTALLED" ]
then
    echo Failed to verify the installation of miniconda. Please install miniconda manually. Abort.
    exit
fi

# create the approporate conda environment
ENVNAME=Segment
echo "Creating environment "${ENVNAME}" for analysis."
if find_in_conda_env $ENVNAME; 
then
   echo The environment ${ENVNAME} already exists. Try to run the segmentation script and see if it works. If not please rename/remove the existing ${ENVNAME} environment so the installation can proceed. Abort.
   exit
fi

# The correct way is to create and use environment.yml
# But oh well, not enough time to do it
# hacky command it is
conda config --add channels conda-forge
conda create -n ${ENVNAME} matplotlib ruptures pandas numpy scipy
if ! find_in_conda_env $ENVNAME;
then
    echo "Failed to create environment "${ENVNAME}". Abort."
    exit
fi
echo Environment ${ENVNAME} is created.
echo Success! 
