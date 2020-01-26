![Build status](https://api.travis-ci.org/AUEB-BALab/fsmove.svg?branch=master)

# FSMoVe

`FSMoVe`
(File System Model Verifier)
is a practical and effective approach
for identifying faults involving ordering violations
missing notifiers in Puppet programs.
To do so, it records the system call trace produced by
the execution of a Puppet manifest through [strace](https://strace.io/).
Then, it operates as follows.
First, `FSMoVe` models the system call trace of
a Puppet execution in a representation
that allows us to precisely capture the interactions
of higher-level programming constructs (i.e., Puppet resources)
with the file system.
By examining their interplays, `FSMoVe `infers the set of the
expected relationships of every Puppet resource with each other.
These relationships correspond to either notifications
(e.g., resource x should notify resource y)
or ordering constraints(e.g., x should run before y).
Then, for the given Puppet program,
`FSMoVe` statically builds a dependency graph
that reflects all the ordering relationships
and notifications that have been specified in Puppet manifests
by the developer. Finally, it verifies whether the expected
relationships (as inferred from the analysis of traces)
hold with respect to the dependency graph.


The full decription of this approach will appear at the proceedings of the upcoming
[International Conference of Software Enginerring](https://conf.researchr.org/track/icse-2020/).
A pre-print is also [available](https://dimitro.gr/assets/papers/SMS20.pdf).


## Building

### Build Docker Images

This repository comes with
a Docker image that contains
an environment for executing and analyzing Puppet programs
using `FSMoVe`.
To build the Docker image locally, run
```bash
docker build -t fsmove --build-arg IMAGE_NAME=<base-image> .
```
where `<base-image>` is the base Docker used to set up
the environment. We have tested our Docker scripts
on `debian:stretch` and `ubuntu:18.04` base images.
Therefore,
to build the Docker image on top of Debian Stretch,
run
```bash
docker build -t fsmove --build-arg IMAGE_NAME=debian:stretch .
```

### Building from source

To build `FSMoVe` from source, you have to
install some necessary packages first
```bash
apt install opam m4
```
Then, install OCaml compiler
(version 4.0.5)
by running
```bash
opam init -y
eval `opam config env`
opam switch 4.05.0
```

Next, install some required opam packages used by `FSMoVe`
```bash
eval `opam config env`
opam install -y ppx_jane core yojson dune ounit fd-send-recv fpath
```

Finally, build `FSMoVe` by running
```bash
dune build -p fsmove
dune install
```

## Running Tests

To run unit tests, run the following command

```bash
dune runtest
```

This will produce something that is similar to the following

```bash
run_tests alias test/runtest
................................................................
Ran: 64 tests in: 0.11 seconds.
OK
```

## Usage

```
> fsmove -help
Applies a Puppet manifest and collects its system call trace.

  fsmove

=== flags ===

  -catalog Path            to the compiled catalog of Puppet manifest.
  -mode Analysis           mode; either online or offline
  [-dump-puppet-out File]  to store output from Puppet execution (for debugging
                           only)
  [-graph-file File]       to store the dependency graph inferred by the
                           compiled catalog.
  [-graph-format Format]   for storing the dependency graph of the provided
                           Puppet manifest.
  [-manifest Path]         to the entrypoint manifest that we need to apply.
                           (Avaiable only when mode is 'online')
  [-modulepath Path]       to the directory of the Puppet modules. (Available
                           only when mode is 'online')
  [-package-notify]        Consider missing notifiers from packages to services
  [-print-stats]           Print stats about execution and analysis
  [-trace-file Path]       to the trace file produced by the 'strace' tool.
  [-build-info]            print info about this build and exit
  [-version]               print the version of this build and exit
  [-help]                  print this help text and exit
                           (alias: -?)

```

## Run and analyze a real-world Puppet module

To run and analyze a real-world Puppet module
through `FSMoVe`, we will use the Docker image
built in a previous step (i.e., `fsmove`)
in order to spawn a fresh environment for Puppet.
Our image expects the following arguments:
* `-m`: The name of Puppet module as it is specified in
 [Forge API](https://forge.puppet.com/).
* `-i`: The version of the module to analyze or `latest`
  to examine the latest version.
* `-s`: A command-line flag used to monitor Puppet execution
 with the `strace` system call tracing utility.

For example, to run and analyze the [`alertlogic-al_agents`](https://forge.puppet.com/alertlogic/al_agents) module
```
docker run --rm -ti \
  --security-opt seccomp:unconfined \
  -v <output-dir>:/home/fsmove/data fsmove \
  -m alertlogic-al_agents-i latest -s
```
A few explanations:

The option `--security-opt seccomp:unconfined` is used to
enable system call tracing inside the Docker container.
The `-v` option is used to mount a local volume
insider Docker container. This is used to store all files
generated by the execution and analysis of the Puppet module
into the provided directory `<output-dir>`.
Specifically, `FSMoVe` produces the following six (6) files
inside the `<output-dir>` directory:
* `alertlogic-al_agents.json`: the compiled catalog of the corresponding Puppet module.
* `alertlogic-al_agents.strace`: a system call trace produced by `strace`.
* `alertlogic-al_agents.size`: the size of system call trace (in bytes)
* `application.time`: time spent to apply the module.
* `alertlogic-al_agents.times`: time spent on trace analysis and fault detection
* `output`: faults detected by `FSMoVe`.

The contents of the `output` file are similar to
```
Missing Ordering Relationships:
===============================
Number of MOR: 1
Pairs:
  * Exec[download]: /etc/puppet/code/environments/production/modules/al_agents/manifests/install.pp: 7
  * Package[al-agent]: /etc/puppet/code/environments/production/modules/al_agents/manifests/install.pp: 24 =>
      Conflict on 1 resources:
      - /tmp/al-agent: Produced by Exec[download] ( open at line 54333 ) and Consumed by Package[al-agent] ( open at line 62579 )

Analysis time: 0.292564153671
```

In particular,
`FSMoVe` detects one missing ordering relationship (MOR)
between the Puppet resource `Exec[download]`
(defined in the `install.pp` file, line 7),
and the resource `Package[al-agent]`
(defined in the `install.pp` file, line 24).
These resources are conflicting on one file,
but there is no dependency between them.
Specifically,
`Exec[download]` produces the file `/tmp/al-agent`,
while `Package[al-agent]` consumes the same file.
For debugging purposes,
`FSMoVe` also reports the system call
and the corresponding line in
the resulting `alertlogin-al_agent.strace` file.
For example, `( open at line 54333 )`
indicates that `Exec[download]` produced
`/tmp/al-agent` by calling the `open()` system call
as it appears at line 54333 of
the corresponding `strace` file.
