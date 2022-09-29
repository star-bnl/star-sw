---
layout: default
---

The information collected in this document is intended for the users of the STAR
experiment. If you feel something is missing or might be useful for other
collaborators, please let us know and we will add it. Direct contributions are
also welcomed.


* TOC
{:toc}

---
<br/>


## Where is the source code?

The primary repositories containing the STAR code and other support packages are
hosted on GitHub:

- https://github.com/star-bnl/star-sw  &mdash; Contains the code we use to
  reconstruct raw data collected by the experiment

- https://github.com/star-bnl/star-mcgen &mdash; Contains Monte-Carlo generators
  and respective interfaces for simulating detector data

- https://github.com/star-bnl/star-sw-test &mdash; This is a temporary test
  repository for STAR users to play with GitHub's interface during the CVS to
  Git transition period. Feel free to create issues, pull requests, etc. but
  don't expect the changes to be accepted in the official releases. This
  repository will be deleted once the primary repository
  [star-sw](https://github.com/star-bnl/star-sw) starts operating in normal mode
  on May 17, 2021.


## On migration of STAR code from CVS to Git

The primary focus of the STAR code migration from CVS to Git is on the "offline"
code responsible for event reconstruction, geometry, simulation, calibration,
and interaction with the database. The following top-level directories (and all
of their contents) are transfered to the `star-sw` and `star-mcgen` repositores
and will be set to read-only mode in CVS.

    star-sw                     star-mcgen
    |-- asps                    |-- pams
    |-- kumacs                  |   `-- gen
    |-- mgr                     `-- StRoot
    |-- OnlTools                    `-- StarGenerator
    |-- pams
    |-- StarDb
    |-- StarVMC
    |-- StDb
    `-- StRoot

It is worth noting that the "online" code, files from the archived user
analyses, and some other support scripts (i.e. the `/online`, `/offline`, and
`/scripts` directories in CVS) are not part of the migration and can be accessed
and manipulated by the users as usual.


## Accessing the STAR repository hosted on GitHub

### Using HTTPS

The easiest way to get a local copy of the entire `star-sw` repository with the
history of all changes is to do:

    $ git clone https://github.com/<YOUR-USERNAME>/star-sw.git

Then `cd` into the newly created directory, look around, and browse the history
with one of the popular utilities, e.g. `gitk`, `tig` (usualy available along
with `git`), or simply use the `git log` command:

    $ cd star-sw
    $ ls -a
    .  ..  asps  .git  kumacs  mgr  OnlTools  pams  StarDb  StarVMC  StDb  StRoot
    $ git status
    On branch main
    Your branch is up to date with 'origin/main'.

    nothing to commit, working tree clean
    $ git log

The above example shows how to clone the `star-sw` repository via `https://`
protocol. When you `git clone`, `git fetch`, `git pull`, or `git push` to the
remote repository the server will ask for your GitHub username and password. The
password is actually your personal access token that you need to create under
your account settings. For more information, see "[Creating a personal access
token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token)."

Some may find it inconvenient to enter credentials every time you issue
a command to communicate with the remote repository. It is possible to avoid
such prompts by [Caching your GitHub credentials in
Git](https://docs.github.com/en/github/using-git/caching-your-github-credentials-in-git).
Alternatively, one can use the SSH keypair mechanism in combination with
`ssh-agent` to let Git authenticate without distracting the user.


### Using SSH

To work with your STAR repository via SSH you must first generate an SSH keypair
on your computer and then add the **public** key to your GitHub account. For
more information, see "[Connecting to GitHub with
SSH](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh)."
Once the public key is uploaded to GiHub you can add the private key to the
`ssh-agent` running in the background. This setup should completely eliminate
the need to enter credentials when working with the remote.

The command to clone your repository via SSH will look like this:

    $ git clone git@github.com:<YOUR-USERNAME>/star-sw.git


### Creating SSH tunnel

In case a direct communication to GitHub's SSH service is not possible one can
set up an SSH tunnel. Let's say you create a tunnel to map the remote server to
your local port 7777 via a gateway:

    $ ssh -f -N -L 7777:github.com:22 login@gateway

then you should be able to clone the repo using the following command:

    $ git clone ssh://git@localhost:7777/<YOUR-USERNAME>/star-sw.git

Note the addition of the `ssh://` prefix. It helps Git to recognize the general
URL syntax specifying the port number.


## How to checkout only one or a few packages

This is called a sparse checkout. In this case you start by cloning the bare
repository

    $ git clone --no-checkout https://github.com/<YOUR-USERNAME>/star-sw.git
    $ ls -a star-sw/
    .  ..  .git

Note that the above command will still create a local copy of the entire
history in `.git` so you can switch between different versions later (There is
also a way to limit the amount of cloned history). Let `git` know that you want
to work with a limited number of modules

    $ cd star-sw/
    $ git config core.sparseCheckout true

Now create and modify the `.git/info/sparse-checkout` file to include a list of
packages you want to work with locally. The contents of the file
may look like this

    $ cat .git/info/sparse-checkout
    StRoot/Sti

or like this

    $ cat .git/info/sparse-checkout
    StRoot/StTofCalibMaker
    StRoot/StTofHitMaker
    StRoot/StTofMaker

wild cards are also possible to use

    $ cat .git/info/sparse-checkout
    StRoot/StPico*

Finally, ask `git` to checkout the selected subdirectories

    $ git checkout SL20a
    $ ls -a StRoot/
    .  ..  StPicoDstMaker  StPicoEvent

Assuming the default STAR environment on a RACF interactive node the code can be
compiled as usual with the `cons` command.


## Containers

Containers are useful for many reasons. In particular, they can be used to
eliminate the necessity to setup the build and run time software environments
for the developers and users while allowing to run containerized application on
different operating systems and platforms.

It is possible to recreate the environment that includes all dependencies
required to compile the STAR libraries by using the [`star-sw` Docker
images](https://github.com/star-bnl/star-sw/pkgs/container/star-sw). These
images are automatically updated on every successful merge into the main branch
and created for every tagged release.

If `singularity` is installed on your system and, for example, you want to work
with the STAR libraries built against ROOT5 with GCC 4.8.5 run the following
command:

```shell
singularity run docker://ghcr.io/star-bnl/star-sw:main-root5-gcc485 bash -l
```

It may take some time to download the image and instantiate a singularity
container but once completed you should see a shell prompt from where you can
browse your local files. At this point you should be able to compile and test
modifications made to a package in your local `StRoot/` by running `cons` as
usual.

In the above example `bash -l` is a command we wanted to run inside the
container. The shell process is interactive, therefore, in order to quit it just
type `exit` in the terminal. Similarly, an interactive ROOT session inside the
container environment can be started by executing the following command:

```shell
singularity run docker://ghcr.io/star-bnl/star-sw:main-root5-gcc485 root4star
```

In a more elaborate example below, we will bind a volume mounted on interactive
SDCC machines (`/star/rcf`) inside the container and run the `bfc.C` macro over
one of our test files via `root4star` in the batch mode. After the process
completes it should return to your normal host environment.

```shell
singularity run -B /star/rcf docker://ghcr.io/star-bnl/star-sw:main-root5-gcc485 root4star -b -q -l 'bfc.C(10, \"pp2022,StiCA,BEmcChkStat,epdhit\", \"/star/rcf/test/daq/2022/010/23010027/st_physics_23010027_raw_1000015.daq\")'
```



## Contributing to the STAR software library

### Common workflow

We adopt a very common workflow typical for many projects hosting a central
repository on GitHub. It can be summarized in a few steps as outlined below:

1. **Fork the repo.** A "copy" of the central repository is created under your
  GitHub account

2. **Clone your fork.** A local "copy" of the repository is created on your
   machine by using a Git command similar to this:

   ```shell
   $ git clone git@github.com:<YOUR-USERNAME>/star-sw.git
   ```

3. **Make changes locally.** The code is modified and commits with informative
   log messages created

4. **Push to your fork.** The changes are sent to your fork hosted on GitHub

   ```shell
   $ git push
   ```

5. **Create a pull request.** Let others know that you want to merge your
  changes into the central repository


### Policies

GitHub allows to configure and enforce certain policies on how users can
contribute their changes to the project. The main idea behind such rules is to
make the collaborative development more straightforward and avoid unnecessary
backlashes. We choose the rules consistent with historical development in CVS.
Below we list the most prominent settings applied to the STAR Git repositories
of which anyone contributing to the repos should be aware.

- **All branches are protected:** Disable force pushes and prevent branches from
  being deleted

- **Maintain linear history:** Prevent merge commits from being pushed to
  branches

  - A linear history is usually easier for humans to understand and debug

- **Require pull requests on GitHub** as the only way to merge commits onto
remote branch

  - Direct pushes to remote branches are disabled for all collaborators

  - We believe the pull requests submitted via GitHub interface provide a better
    documentation of proposed changes

- **Require pull request reviews before merging**

  - GitHub can suggest reviewers based on historical code changes

  - We suggest two required approving reviews from code owners/maintainers
    before a pull request can be merged

    - Similar to `CVSROOT/avail` we create a `CODEOWNERS` file in our repository
      mapping subdirectories to specific people whose approval is required. For
      example, see
      https://github.com/star-bnl/star-git-tools/blob/main/.github/CODEOWNERS_star-sw

  - In cases when reviewers cannot be easily identified the Infrastructure Team
  will step in


## How to build a release

A decision was made not to migrate the MC event generators (MCEG) originally
present in the STAR CVS repository into the primary Git repository
[`star-sw`](https://github.com/star-bnl/star-sw). Instead the event
generators were moved into a separate Git repository
[`star-mcgen`](https://github.com/star-bnl/star-mcgen). This separation
underlines the external nature of the MCEGs to the rest of the STAR codebase
and allows to build these libraries independently only when necessary.
Meanwhile, in order to build a release with all the libraries one can merge the
code from the two repositories into a single local directory and proceed as
usual. For example, to get the code corresponding to the `SL20a` tag do

    mkdir release-SL20a && cd release-SL20a
    curl -sL https://github.com/star-bnl/star-sw/archive/SL20a.tar.gz | tar -xz  --strip-components 1
    curl -sL https://github.com/star-bnl/star-mcgen/archive/SL20a.tar.gz | tar -xz  --strip-components 1

At this point the code can be compiled as usual, e.g. by running `cons`.
Similarly, to get the most recent code for the "DEV" release replace `SL20a`
with `main` in the above example.


## Equivalent commands for Git and CVS

Here is a small table with equivalent commands for Git and CVS repositories.

Description                                    | CVS               | GIT
---                                            | ---               | ---
Show status of repository                      | `cvs status`      | `git status                                `
Add new files to repository                    | `cvs add <files>` | `git add <files>                           `
Commit changes in existing files to repository | `cvs commit`      | `git add -u <files> && git commit && git push`
Retrieve changes from repository               | `cvs update`      | `git pull                                  `
Show log of changes to a file                  | `cvs log <file>`  | `git log <file>                            `
Show changes in commit/revision                |                   | `git show commit                           `
Resolve conflicts in files                     |                   | `git mergetool                             `

## Tips for csh users

Switching between branches may become confusing. To help you with tracking of the current branch you might want to put following lines to your `~/.cshrc`:

```csh
alias cd 'chdir \!:* && update_prompt'
alias git 'command git \!:* && update_prompt'
alias update_prompt 'set prompt="%B%S[%m]%s%b %.04/`command git branch |& grep \* | awk \{print\ "\\\""\ \("\\\"\\\$"2"\\\""\)"\\\""\}`> "'
```

This will modify your prompt from

```
[rcas6xxx] ~/star-sw/>
```

To something like

```
[rcas6xxx] ~/star-sw/ (main)>
```

*This is just a hack and may fail to update the command prompt. Feel free to open an issue if you encounter any problems or have any suggestions regarding this.*
