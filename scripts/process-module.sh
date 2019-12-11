#! /bin/bash

basedir=${HOME}/data
modulepath=${HOME}/.puppet/etc/code/modules
install_module="no"
timeout=6
with_strace=0
iterations=1
production_dir=/etc/puppet/code/environments/production


while getopts "m:p:i:st:k:" opt; do
  case "$opt" in
    m)  module=$OPTARG
        ;;
    p)  modulepath=$OPTARG
        ;;
    i)  install_module=$OPTARG
        ;;
    s)  with_strace=1
        ;;
    t)  timeout=$OPTARG
        ;;
    k)  iterations=$OPTARG
        ;;
  esac
done
shift $(($OPTIND - 1));


if [ -z $module ]; then
  # Run script in interactive mode for debugging purposes.
  bash
  exit 0
fi


if [ "$iterations" -lt 1 ]; then
  echo "You must provide a number greater than 1"
  exit 1
fi

function install_module()
{
  local version module
  module=$1
  version=$2
  if [ "$version" = "latest" ]; then
    echo "Installing module $module..."
    timeout -s KILL 6m puppet module install "$module"
    rc=$?
  else
    echo "Installing module $module, version: $version..."
    timeout -s KILL 6m puppet module install "$module" --version "$version"
    rc=$?
  fi
  return $rc
}


function gen_catalog()
{
  local module
  module=$1

  if [ ! -f ${HOME}/init.pp ]; then
    module_basename="$(echo "$module" | cut -d '-' -f2)"
    echo "include $module_basename" > $HOME/init.pp
  fi

  sudo mkdir -p $production_dir/manifests
  sudo cp ${HOME}/init.pp $production_dir/manifests
  sudo cp -r $modulepath $production_dir

  # Get compiled catalog.
  sudo puppet catalog find production |
  tail -n +2 > $basedir/$module.json

  return $?
}


function apply_catalog()
{
  local module modulepath timeout with_strace
  module=$1
  modulepath=$2
  timeout=$3
  with_strace=$4

  # Execute puppet script with timeout 10 minutes in stand-alone mode.
  if [ $with_strace -eq 0 ]; then
    echo "Applying Puppet catalog without strace..."
    timeout -s KILL ${timeout}m sudo puppet apply $HOME/init.pp \
        --debug \
        --evaltrace \
        --modulepath $modulepath |
    grep -oP 'Notice: Applied catalog in [0-9]+.[0-9]+' |
    sed 's/Notice: Applied catalog in //g' > $basedir/application.time
  else
    echo "Applying Puppet catalog with strace..."
    sudo strace -s 300 \
        -e "$(tr -s '\r\n' ',' < $HOME/syscalls.txt | sed -e 's/,$/\n/')" \
        -o $basedir/$module.strace \
        -f puppet apply $HOME/init.pp \
        --debug \
        --evaltrace \
        --modulepath $modulepath &
    pid=$!
    timeout -s KILL ${timeout}m polling.sh $pid $basedir/$module.strace
    cat $basedir/$module.strace |
    grep -oP 'Notice: Applied catalog in [0-9]+.[0-9]+' |
    sed 's/Notice: Applied catalog in //g' > $basedir/application.time
  fi
}


function analyze_traces()
{
  local module iterations
  module=$1
  iterations=$2

  # Remove the last line
  sed -i '$ d' $basedir/$module.strace

  # Compute trace size in MB.
  stat $basedir/$module.strace |
  grep -oP 'Size: \K([0-9]+)' > $basedir/$module.size

  set +e

  echo "Detecting missing ordering relationships and notifiers..."
  if [ ! -f $basedir/$module.times ]; then
    eval `opam config env`
    for i in {1..$iterations}
    do
      fsmove analyze -print-time \
        -package-notify \
        -catalog $basedir/$module.json \
        -traces $basedir/$module.strace > $basedir/output 2> $basedir/err
      if [ $? -ne 0 ]; then
        return 2
      fi
      cat $basedir/output |
      grep -oP 'Analysis time: \K([0-9\.]+)' >> $basedir/$1.times
      rm $basedir/err
    done
  fi
  return 0
}


# Install any puppet code if it necessary.
if [ "$install_module" != "no" ]; then
  install_module "$module" "$install_module"
  if [ $? -ne 0 ]; then
    echo "Unable to install module $module"
    exit 1
  fi
fi


# Run a script before applying the puppet configuration.
if [ -e ${HOME}/pre-script.sh ];
then
    echo "Running pre-script..."
    ${HOME}/pre-script.sh
    if [ $? -ne 0 ]; then
      echo "Prescipt exited with a non-zero exit code"
      exit 1
    fi
fi


gen_catalog "$module"
# Compilation of catalog failed; so exit with non-zero.
if [ $? -ne 0 ]; then
  echo "Couldn't generate catalog of module $module"
  exit 1
fi


apply_catalog "$module" "$modulepath" "$timeout" $with_strace
rc=$?
if [ $rc -ne 1  ]; then
  sudo chown -R fsmove:fsmove $basedir
  if [ $with_strace -eq 0 ]; then
    exit 0
  fi
  analyze_traces "$module" $iterations
  exit $?
else
  # Puppet script returned a non-zero exit code, so we return
  exit 1
fi
