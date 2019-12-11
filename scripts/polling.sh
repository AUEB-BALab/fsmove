pid=$1
strace_file=$2

while true; do
  sleep 20
  if ps -p $pid > /dev/null; then
    if grep -q -oP 'Notice: Applied catalog in [0-9]+.[0-9]+' $strace_file; then
      sudo killall -q -s KILL $pid
      break
    fi
  else
    break
  fi
done
exit 0
