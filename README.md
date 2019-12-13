![Build status](https://api.travis-ci.org/AUEB-BALab/fsmove.svg?branch=master)

# FSMove

`FSMove` is a practical and effective approach
for identifying faults involving ordering violations
missing notifiers in Puppet programs.
To do so, it records the system call trace produced by
the execution of a Puppet manifest through [strace](https://strace.io/).
Then, it operates as follows.
First, `FSMove` models the system call trace of
a Puppet execution in a representation
that allows us to precisely capture the interactions
of higher-level programming constructs (i.e., Puppet resources)
with the file system.
By examining their interplays, `FSMove `infers the set of the
expected relationships of every Puppet resource with each other.
These relationships correspond to either notifications
(e.g., resource x should notify resource y)
or ordering constraints(e.g., x should run before y).
Then, for the given Puppet program,
`FSMove` statically builds a dependency graph
that reflects all the ordering relationships
and notifications that have been specified in Puppet manifests
by the developer. Finally, it verifies whether the expected
relationships (as inferred from the analysis of traces)
hold with respect to the dependency graph.


The full decription of this approach will appear at the proceedings of the upcoming
[International Conference of Software Enginerring](https://conf.researchr.org/track/icse-2020/).
A pre-print is also [available](https://dimitro.gr/assets/papers/SMS20.pdf).


**Note**: Improving documentation is an ongoing effort.


## Building

### Build Docker Images

To build a Docker image that contains
an environment for executing and analyzing Puppet programs
through `FSMove`, run

```bash
docker build -t fsmove --build-arg IMAGE_NAME=<base-image> .
```

where `<base-image>` is the base Docker used to set up
the environment. We have tested our Docker script
on `debian:stretch` and `ubuntu:18.04` base images.

### Building from source

To build `FSMove` from source, you have to
install some necessary packages first

```bash
apt install opam m4
```

Then, install OCaml compiler 4.05 by running

```bash
opam init -y
eval `opam config env`
opam switch 4.05.0
```

Next, install some opam packages used by `FSMove`

```bash
eval `opam config env`
opam install -y ppx_jane core yojson dune ounit
```

Finally, build `FSMove` by running

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

## Run and analyze a real-world Puppet module

To run and analyze a real-world Puppet module
through `FSMove`, we will use the Docker image
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
docker run -ti --security-opt seccomp:unconfined -v <output-dir>:/home/fsmove/data fsmove -m alertlogic-al_agents-i latest -s
```

A few explanations:

The option `--security-opt seccomp:unconfined` is used to
enable system call tracing inside the Docker container.

The `-v` option is used to mount a local volume
insider Docker container. This is done to store all files
generated by the execution and analysis of the Puppet module
into the provided directory `output-dir`.

Specifically, `FSMove` produces the following six (6) files:

* `alertlogic-al_agents.json`: Compiled catalog of Puppet module.
* `alertlogic-al_agents.strace`: System call trace produced by `strace`.
* `alertlogic-al_agents.size`: Size of system call trace (in bytes)
* `application.time`: Time spent to apply module.
* `alertlogic-al_agents.times`: Time spent on analysis
* `output`: Faults detected by `FSMove`.

The contents of the `output` file are similar to

```
Missing Ordering Relationships:
===============================
# Faults: 1
Pairs:
  * Exec[download]: /etc/puppet/code/environments/production/modules/al_agents/manifests/install.pp: 7
  * Package[al-agent]: /etc/puppet/code/environments/production/modules/al_agents/manifests/install.pp: 24 =>
      Conflict on 1 resources:
      - /tmp/al-agent: Produced by Exec[download] ( open at line 54333 ) and Consumed by Package[al-agent] ( open at line 62579 )

Analysis time: 0.292564153671
```

This indicates that the module `alertlogic-al_agents`
has one ordering violation
between the `Exec[download]`
and `Package[al-agent]` resources.
