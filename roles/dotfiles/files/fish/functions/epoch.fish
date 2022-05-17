function epoch --description 'Given a number of nanos since epoch, output a UNIX UTC timestamp' --argument epoch_time
  if echo $argv | grep -v '[:-]' >/dev/null
        date -u -d @(math "$argv[1] / 1000000000")
  else
    date -u -d "$argv" '+%s'000000000
  end
end
